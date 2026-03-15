##############################################################
##  Chat MSF  —  Comic Book Edition  v0.3
##  - CSVs fetched from GitHub at startup (no local files)
##  - Token-efficient RAG: one ability per slot, compact stats
##  - Larger fonts throughout
##############################################################

library(shiny)
library(ellmer)

# ── 1.  Fetch CSVs from GitHub ───────────────────────────────────────────────
chars_url     <- "https://raw.githubusercontent.com/matthew-danna/r-samples/refs/heads/main/msf/msf_characters.csv"
abilities_url <- "https://raw.githubusercontent.com/matthew-danna/r-samples/refs/heads/main/msf/msf_abilities_text.csv"

chars_df     <- read.csv(url(chars_url),     stringsAsFactors = FALSE, check.names = FALSE)
abilities_df <- read.csv(url(abilities_url), stringsAsFactors = FALSE, check.names = FALSE)

# Keep only the canonical 4 ability slots per character (no redundant upgrade rows)
CANONICAL_SLOTS <- c("basic", "special", "ultimate", "passive")
abilities_df <- abilities_df[abilities_df$ability_slot %in% CANONICAL_SLOTS, ]

# Tag columns (Y/N team/trait membership)
tag_cols  <- names(chars_df)[3:128]
# Stat columns
stat_cols <- c("power","health","damage","armor","focus","resist",
               "crit damage","crit chance","speed","dodge chance",
               "block chance","block amount","accuracy")

# ── 2.  RAG helpers ──────────────────────────────────────────────────────────
chars_lower    <- tolower(chars_df$character_name)
chars_id_lower <- tolower(chars_df$character_id)
tag_cols_lower <- tolower(tag_cols)

match_characters <- function(query, max_chars = 6) {
  q      <- tolower(query)
  tokens <- unique(unlist(strsplit(q, "[^a-z0-9]+")))
  tokens <- tokens[nchar(tokens) >= 3]
  
  matched_idx <- integer(0)
  
  for (tok in tokens) {
    matched_idx <- union(matched_idx, which(grepl(tok, chars_lower,    fixed = TRUE)))
    matched_idx <- union(matched_idx, which(grepl(tok, chars_id_lower, fixed = TRUE)))
  }
  for (tok in tokens) {
    tag_hit <- which(grepl(tok, tag_cols_lower, fixed = TRUE))
    if (length(tag_hit) > 0) {
      for (col in tag_cols[tag_hit]) {
        matched_idx <- union(matched_idx, which(chars_df[[col]] == "Y"))
      }
    }
  }
  
  if (length(matched_idx) == 0) return(character(0))
  chars_df$character_id[head(matched_idx, max_chars)]
}

build_context <- function(char_ids) {
  if (length(char_ids) == 0) return("")
  
  blocks <- lapply(char_ids, function(cid) {
    row <- chars_df[chars_df$character_id == cid, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    
    name     <- row$character_name
    tags_str <- paste(tag_cols[unlist(row[1, tag_cols]) == "Y"], collapse = ", ")
    
    # Compact stats: "power:1.2M | health:3.4M | damage:250K | speed:120 ..."
    s <- as.numeric(unlist(row[1, stat_cols]))
    names(s) <- stat_cols
    fmt <- function(x) {
      if (is.na(x)) return("?")
      if (x >= 1e6) return(paste0(round(x/1e6, 1), "M"))
      if (x >= 1e3) return(paste0(round(x/1e3,  1), "K"))
      as.character(round(x))
    }
    stats_str <- paste(paste0(names(s), ":", sapply(s, fmt)), collapse=" | ")
    
    # One ability per slot
    ab_rows <- abilities_df[abilities_df$character_id == cid, , drop = FALSE]
    ab_str  <- if (nrow(ab_rows) > 0) {
      paste(apply(ab_rows[match(CANONICAL_SLOTS, ab_rows$ability_slot), , drop = FALSE], 1,
                  function(r) if (any(!is.na(r))) paste0("[", r["ability_slot"], "] ", r["ability_text"]) else NULL
      ), collapse = "\n")
    } else "(no ability data)"
    
    paste0("** ", name, " ** | Teams: ", tags_str, "\n",
           "Stats: ", stats_str, "\n",
           ab_str)
  })
  
  paste(Filter(Negate(is.null), blocks), collapse = "\n\n")
}

# ── 3.  Effects glossary (hardcoded, always in system prompt) ─────────────────
effects_glossary <- paste0(
  "=== MSF STATUS EFFECTS GLOSSARY ===\n\n",
  "-- BUFFS --\n",
  "Offense Up: +50% damage dealt, +50% debuff application chance.\n",
  "Defense Up: -50% incoming damage, +50% Resistance. Can be flipped against you.\n",
  "Speed Up: Faster Speed Bar fill. Can be a liability vs flip-heavy teams.\n",
  "Stealth: Untargetable unless last standing. AoE still hits. Breaks on buff-flip AoE.\n",
  "Taunt: Forces all enemies to target this character. Some characters can ignore it.\n",
  "Immunity: Blocks debuff application. Can be flipped, stolen, or stripped.\n",
  "Regeneration: Heals % of max HP each turn. Stackable.\n",
  "Deflect: Next attack misses entirely. Single use.\n",
  "Counterattack: Auto basic-attacks when hit. Triggers on AoE; devastating when stacked.\n",
  "Charged: Character-specific stacking mechanic (Thor, She-Hulk, Captain Marvel, etc). Triggers bonus effect at threshold.\n",
  "Barrier: Absorbs damage before HP. Lasts until depleted.\n",
  "Deathproof: Survives one lethal hit at 1 HP. Single use per application.\n",
  "Evade: Next attack misses (dodge-based). Similar to Deflect.\n",
  "Ability Energy: Grants charges toward special/ultimate ability.\n",
  "Revive: Brings a defeated ally back at set HP%. Blocked by Trauma on the target.\n\n",
  "-- DEBUFFS --\n",
  "Bleed: Damage over time at turn start based on attacker's damage stat. Stackable.\n",
  "Heal Block: Prevents all healing entirely. Stackable. Essential vs regen teams.\n",
  "Ability Block: Target limited to basic attack only — no special, ultimate, or passive.\n",
  "Stun: Target skips their turn. Disables passives (dodge, counterattack, assists).\n",
  "Slow: Reduces Speed Bar fill rate. Hard counter to speed-reliant teams.\n",
  "Offense Down: -50% damage dealt and -50% debuff application chance.\n",
  "Defense Down: +50% damage taken. Stacks with Offense Up for massive burst.\n",
  "Disrupt: Prevents target gaining any new buffs.\n",
  "Blind: -100% Accuracy; attacks miss. Cyclops (200% accuracy) and Daredevil are immune.\n",
  "Vulnerability: Target takes bonus damage; triggers Skirmisher ISO-8 buff removal on hit.\n",
  "Trauma: Prevents revival if defeated while under this effect.\n",
  "Drain: Reduces target's Speed Bar.\n",
  "Burning/Poison/Frostbite: Damage over time variants tied to specific character kits. Stackable.\n",
  "Ensnare/Web: Prevents Speed Bar gains; applied by Spider-Verse characters.\n",
  "Fear: Forces basic attack only (similar to Ability Block, thematically distinct).\n",
  "Suppressed: Disables passive ability activation.\n\n",
  "-- UTILITY --\n",
  "Flip: Converts buffs to debuffs or vice versa instead of removing them.\n",
  "Clear Positive / Clear Negative: Removes all buffs or all debuffs from target.\n",
  "Copy Positive: Copies all buffs from one character to another.\n",
  "Fill Speed Bar: Advances a character's turn sooner.\n",
  "Summon: Spawns a minion ally into battle.\n",
  "=== END EFFECTS ==="
)

# ── 4.  System prompt ─────────────────────────────────────────────────────────
base_system_prompt <- paste0(
  "You are AGENT ALPHA — a veteran Marvel Strike Force strategist and data scientist ",
  "who has played since global launch. Cite specific stats, ability text, and effect definitions when relevant. ",
  "Each user message may end with a DATA block from the MSF database — use it as your primary source. ",
  "If no character data is present, answer from general MSF knowledge and say so. ",
  "Format numbers with K/M suffixes. Keep answers punchy and focused.\n\n",
  effects_glossary
)

# ── 4.  UI ────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Bangers&family=Comic+Neue:ital,wght@0,400;0,700;1,400&display=swap"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/markdown-it@14.1.0/dist/markdown-it.min.js"),
    
    tags$style(HTML("

      body {
        background-color: #FFEB3B;
        background-image:
          radial-gradient(circle, #c62828 1.5px, transparent 1.5px),
          radial-gradient(circle, #c62828 1.5px, transparent 1.5px);
        background-size: 22px 22px;
        background-position: 0 0, 11px 11px;
        font-family: 'Comic Neue', cursive;
        font-size: 20px;
        margin: 0; padding: 0;
      }

      #comic_outer {
        max-width: 900px;
        margin: 24px auto 48px auto;
        border: 5px solid #111;
        border-radius: 3px;
        box-shadow: 10px 10px 0 #111;
        background: #fff;
        overflow: hidden;
      }

      /* ── Title banner ── */
      #comic_title {
        background: #c62828;
        border-bottom: 5px solid #111;
        padding: 14px 22px 12px 22px;
        display: flex;
        align-items: center;
        gap: 14px;
      }
      #comic_title h1 {
        font-family: 'Bangers', cursive;
        font-size: 3.2rem;
        letter-spacing: 4px;
        color: #FFEB3B;
        -webkit-text-stroke: 2px #111;
        text-shadow: 3px 3px 0 #111;
        margin: 0; line-height: 1;
      }
      #comic_title .badge {
        background: #111;
        color: #FFEB3B;
        font-family: 'Bangers', cursive;
        font-size: 1rem;
        letter-spacing: 2px;
        padding: 4px 12px;
        border-radius: 2px;
      }
      #comic_title .issue {
        font-family: 'Bangers', cursive;
        font-size: 1.15rem;
        color: #ffd54f;
        margin-left: auto;
        letter-spacing: 1px;
      }

      /* ── Chat output ── */
      #chat_output {
        min-height: 500px;
        max-height: 600px;
        overflow-y: auto;
        padding: 22px 24px 14px 24px;
        background: #FFFDE7;
        scrollbar-width: thin;
        scrollbar-color: #c62828 #fff8e1;
      }

      /* ── Panel gutter ── */
      .panel-gutter {
        height: 8px;
        background: repeating-linear-gradient(90deg, #111 0, #111 5px, transparent 5px, transparent 22px);
        border-top: 3px solid #111;
        border-bottom: 3px solid #111;
      }

      /* ── AI bubble ── */
      .chat_reply {
        position: relative;
        max-width: 76%;
        background: #fff;
        border: 3px solid #111;
        border-radius: 16px 16px 16px 3px;
        padding: 12px 17px 16px 17px;
        margin: 8px 0 26px 6px;
        box-shadow: 3px 3px 0 #111;
        animation: popIn 0.22s cubic-bezier(.175,.885,.32,1.275) forwards;
        font-size: 1.25rem;
        line-height: 1.65;
      }
      .chat_reply::before {
        content: '';
        position: absolute;
        bottom: -14px; left: 16px;
        border: 12px solid transparent;
        border-top-color: #111;
        border-right: 0; border-bottom: 0;
      }
      .chat_reply::after {
        content: '';
        position: absolute;
        bottom: -10px; left: 18px;
        border: 10px solid transparent;
        border-top-color: #fff;
        border-right: 0; border-bottom: 0;
      }
      .chat_reply .speaker {
        display: block;
        font-family: 'Bangers', cursive;
        font-size: 1.1rem;
        letter-spacing: 2px;
        color: #c62828;
        margin-bottom: 6px;
      }

      /* ── User bubble ── */
      .chat_input {
        position: relative;
        max-width: 68%;
        background: #1565C0;
        border: 3px solid #111;
        border-radius: 16px 16px 3px 16px;
        padding: 12px 17px 16px 17px;
        margin: 8px 6px 26px auto;
        font-family: 'Comic Neue', cursive;
        font-size: 1.25rem;
        font-weight: 700;
        color: #fff;
        box-shadow: 3px 3px 0 #111;
        animation: popIn 0.22s cubic-bezier(.175,.885,.32,1.275) forwards;
        line-height: 1.5;
        word-break: break-word;
      }
      .chat_input::before {
        content: '';
        position: absolute;
        bottom: -14px; right: 16px;
        border: 12px solid transparent;
        border-top-color: #111;
        border-left: 0; border-bottom: 0;
      }
      .chat_input::after {
        content: '';
        position: absolute;
        bottom: -10px; right: 18px;
        border: 10px solid transparent;
        border-top-color: #1565C0;
        border-left: 0; border-bottom: 0;
      }

      /* ── Input row ── */
      #chat_input_row {
        display: flex;
        align-items: stretch;
        border-top: 5px solid #111;
        background: #c62828;
        min-height: 72px;
      }
      #chat_input_row .shiny-input-container,
      #chat_input_row .form-group {
        margin: 0 !important;
        flex: 1;
        width: auto !important;
      }
      #textarea_chat_input {
        border: none !important;
        border-right: 4px solid #111 !important;
        border-radius: 0 !important;
        background: #FFFDE7 !important;
        font-family: 'Comic Neue', cursive !important;
        font-size: 1.25rem !important;
        font-weight: 700;
        color: #111 !important;
        padding: 14px 16px !important;
        resize: none;
        outline: none !important;
        box-shadow: none !important;
        min-height: 68px !important;
        width: 100% !important;
      }
      #textarea_chat_input::placeholder {
        color: #999; font-style: italic; font-weight: 400;
      }
      #send_text {
        font-family: 'Bangers', cursive !important;
        font-size: 1.7rem !important;
        letter-spacing: 2px;
        color: #111 !important;
        background: #FFEB3B !important;
        border: none !important;
        border-radius: 0 !important;
        width: 120px;
        flex-shrink: 0;
        cursor: pointer;
        transition: background 0.12s, transform 0.1s;
        padding: 0 !important;
        text-shadow: 1px 1px 0 rgba(198,40,40,0.35);
      }
      #send_text:hover  { background: #fff176 !important; transform: scale(1.03); }
      #send_text:active { transform: scale(0.97); }

      /* ── Footer ── */
      #comic_footer {
        background: #111;
        color: #FFEB3B;
        font-family: 'Bangers', cursive;
        font-size: 0.9rem;
        letter-spacing: 3px;
        text-align: center;
        padding: 6px;
      }

      @keyframes popIn {
        from { opacity: 0; transform: scale(0.82) translateY(8px); }
        to   { opacity: 1; transform: scale(1)    translateY(0);   }
      }

      /* ── Markdown in bubbles ── */
      .bubble_content p  { margin: 0 0 6px 0; }
      .bubble_content ul,
      .bubble_content ol { margin: 5px 0 5px 20px; }
      .bubble_content li { margin-bottom: 3px; }
      .bubble_content code {
        background: #f5f5f5; border: 1px solid #ddd;
        border-radius: 3px; padding: 1px 5px; font-size: 0.9em;
      }
      .bubble_content strong { color: #c62828; }
      .bubble_content table  { border-collapse: collapse; font-size: 0.92em; margin: 7px 0; }
      .bubble_content th, .bubble_content td { border: 1px solid #ccc; padding: 4px 10px; }
      .bubble_content th { background: #fce4e4; }

      .form-group { margin: 0 !important; }
    "))
  ),
  
  tags$script(HTML("
    var chunks = '';
    const md = markdownit();

    Shiny.addCustomMessageHandler('newReply', function(x) { chunks = ''; });
    Shiny.addCustomMessageHandler('updateReply', function(chunk) {
      chunks += chunk;
      var bubbles = document.querySelectorAll('.chat_reply .bubble_content');
      bubbles[bubbles.length - 1].innerHTML = md.render(chunks);
      var out = document.getElementById('chat_output');
      out.scrollTop = out.scrollHeight;
    });
    Shiny.addCustomMessageHandler('scrollDown', function(x) {
      var out = document.getElementById('chat_output');
      out.scrollTop = out.scrollHeight;
    });
    // Ctrl/Cmd+Enter to send
    document.addEventListener('keydown', function(e) {
      if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
        document.getElementById('send_text').click();
      }
    });
  ")),
  
  div(id = "comic_outer",
      
      div(id = "comic_title",
          tags$h1("CHAT MSF"),
          span(class = "badge", "NOW WITH OPINIONS"),
          span(class = "issue", "ISSUE #1  \u00b7  NOT ENDORSED BY SCOPELY")
      ),
      
      div(id = "chat_output",
          div(class = "chat_reply",
              span(class = "speaker", "\u26a1 AGENT ALPHA"),
              div(class = "bubble_content",
                  tags$p(tags$strong("Greetings, Commander!")),
                  tags$p("Wired into your MSF database — 363 characters, tier lists, and ability data."),
                  tags$p("Ask about any character, team comp, or synergy. ",
                         tags$em("Ctrl+Enter"), " to send fast.")
              )
          )
      ),
      
      div(class = "panel-gutter"),
      
      div(id = "chat_input_row",
          textAreaInput(
            "textarea_chat_input",
            label       = NULL,
            width       = "100%",
            height      = "68px",
            resize      = "none",
            placeholder = "Ask about a character, team comp, synergy, or tier ranking..."
          ),
          actionButton("send_text", label = "SEND!")
      ),
      
      div(id = "comic_footer",
          "BUILT BY the_notorious_md  \u00b7  DISCORD: the_notorious_md  \u00b7  PROBABLY WRONG ABOUT YOUR FAVES")
  )
)

# ── 5.  Server ────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  chat <- ellmer::chat_github(
    system_prompt = base_system_prompt,
    base_url      = "https://models.inference.ai.azure.com/",
    api_key       = Sys.getenv("GITHUB_PAT"),
    model         = "gpt-4o",
    seed          = NULL,
    api_args      = list(),
    echo          = FALSE
  )
  
  observe({
    req(nchar(trimws(input$textarea_chat_input)) > 0)
    
    user_text <- input$textarea_chat_input
    
    # RAG: find relevant characters, build compact context
    matched_ids   <- match_characters(user_text, max_chars = 5)
    context_block <- if (length(matched_ids) > 0) {
      paste0("\n\n[MSF DATA — ", length(matched_ids), " character(s)]\n",
             build_context(matched_ids))
    } else {
      "\n\n[No specific characters matched. Answer from general MSF knowledge.]"
    }
    
    stream <- chat$stream(paste0(user_text, context_block))
    
    insertUI("#chat_output", where = "beforeEnd",
             ui = div(class = "chat_input", user_text), immediate = TRUE)
    updateTextAreaInput(inputId = "textarea_chat_input", value = "")
    session$sendCustomMessage("scrollDown", list())
    
    insertUI("#chat_output", where = "beforeEnd",
             ui = div(class = "chat_reply",
                      span(class = "speaker", "\u26a1 AGENT ALPHA"),
                      div(class = "bubble_content", "")
             ), immediate = TRUE)
    
    new_msg <- TRUE
    coro::loop(for (chunk in stream) {
      if (new_msg) session$sendCustomMessage("newReply", 1)
      session$sendCustomMessage("updateReply", chunk)
      new_msg <- FALSE
    })
    
  }) |> bindEvent(input$send_text)
}

shinyApp(ui = ui, server = server)
