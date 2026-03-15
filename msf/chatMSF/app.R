##############################################################
##  Chat MSF  —  Comic Book Edition  v0.4
##  RAG: injects characters.csv + meta.csv into every prompt
##############################################################

library(shiny)
library(ellmer)

# ── 1.  Load & Stringify CSVs ────────────────────────────────────────────────
#  Put your CSVs in the same folder as app.R (or adjust paths below).
#  The entire content is embedded as plain text in the system prompt.

read_csv_as_text <- function(path, label) {
  if (!file.exists(path)) {
    return(paste0("[", label, ": file not found at '", path, "']"))
  }
  df  <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  hdr <- paste(names(df), collapse = " | ")
  rows <- apply(df, 1, function(r) paste(r, collapse = " | "))
  paste0(
    "### ", label, "\n",
    hdr, "\n",
    paste(rows, collapse = "\n")
  )
}

characters_text <- read_csv_as_text("/Users/matthewdanna/Downloads/msf_characters.csv", "Character Stats")
meta_text       <- read_csv_as_text("/Users/matthewdanna/Downloads/msf_abilities_text.csv",        "Meta / Tier List / Abilities & Synergies")

system_prompt_text <- paste0(
  "You are a veteran Marvel Strike Force strategist and data scientist who has played since global launch. ",
  "You speak with confidence, use stats to back up your claims, and occasionally drop comic-book flavored flair. ",
  "When answering questions, consult the datasets below first and cite specific numbers or tiers when relevant.\n\n",
  "========== DATASET 1 ==========\n",
  characters_text, "\n\n",
  "========== DATASET 2 ==========\n",
  meta_text, "\n\n",
  "Always ground your answers in the data above. If the answer isn't in the data, say so clearly."
)

# ── 2.  UI ───────────────────────────────────────────────────────────────────
ui <- fluidPage(
  
  # ── Google Fonts + Markdown-it ──
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Bangers&family=Comic+Neue:wght@400;700&display=swap"
    ),
    tags$script(
      src = "https://cdn.jsdelivr.net/npm/markdown-it@14.1.0/dist/markdown-it.min.js"
    ),
    
    # ── Global Styles ──
    tags$style(HTML("

      /* ── Page ── */
      body {
        background-color: #FFEB3B;
        background-image:
          radial-gradient(circle, #E53935 1.5px, transparent 1.5px),
          radial-gradient(circle, #E53935 1.5px, transparent 1.5px);
        background-size: 24px 24px;
        background-position: 0 0, 12px 12px;
        font-family: 'Comic Neue', cursive;
        margin: 0;
        padding: 0;
      }

      /* ── Outer wrapper ── */
      #comic_outer {
        max-width: 900px;
        margin: 28px auto 40px auto;
        border: 5px solid #111;
        border-radius: 4px;
        box-shadow: 8px 8px 0 #111;
        background: #fff;
        overflow: hidden;
      }

      /* ── Title banner ── */
      #comic_title {
        background: #E53935;
        border-bottom: 5px solid #111;
        padding: 14px 22px 10px 22px;
        display: flex;
        align-items: baseline;
        gap: 14px;
      }
      #comic_title h1 {
        font-family: 'Bangers', cursive;
        font-size: 3rem;
        letter-spacing: 3px;
        color: #FFEB3B;
        -webkit-text-stroke: 2px #111;
        text-shadow: 3px 3px 0 #111;
        margin: 0;
        line-height: 1;
      }
      #comic_title .subtitle {
        font-family: 'Comic Neue', cursive;
        font-weight: 700;
        font-size: 0.95rem;
        color: #fff;
        background: #111;
        padding: 2px 8px;
        border-radius: 3px;
        letter-spacing: 1px;
        text-transform: uppercase;
      }
      #comic_title .issue {
        font-family: 'Bangers', cursive;
        font-size: 1.2rem;
        color: #FFEEB3;
        margin-left: auto;
        letter-spacing: 1px;
      }

      /* ── Chat output panel ── */
      #chat_output {
        height: 560px;
        overflow-y: auto;
        padding: 18px 20px 10px 20px;
        background: #FFFDE7;
        scrollbar-width: thin;
        scrollbar-color: #E53935 #fff;
      }

      /* ── Panel divider ── */
      .panel-gutter {
        height: 10px;
        background: repeating-linear-gradient(
          90deg,
          #111 0px, #111 4px,
          transparent 4px, transparent 20px
        );
      }

      /* ── AI bubble (speech balloon, left) ── */
      .chat_reply {
        position: relative;
        max-width: 72%;
        background: #fff;
        border: 3px solid #111;
        border-radius: 18px 18px 18px 4px;
        padding: 12px 16px;
        margin: 6px 0 18px 8px;
        font-family: 'Comic Neue', cursive;
        font-size: 1rem;
        line-height: 1.55;
        color: #111;
        box-shadow: 3px 3px 0 #111;
        animation: popIn 0.25s cubic-bezier(.175,.885,.32,1.275) forwards;
      }
      /* tail */
      .chat_reply::before {
        content: '';
        position: absolute;
        bottom: -13px;
        left: 18px;
        border-width: 12px 10px 0 0;
        border-style: solid;
        border-color: #111 transparent;
      }
      .chat_reply::after {
        content: '';
        position: absolute;
        bottom: -9px;
        left: 20px;
        border-width: 10px 8px 0 0;
        border-style: solid;
        border-color: #fff transparent;
      }
      /* speaker label */
      .chat_reply .speaker {
        font-family: 'Bangers', cursive;
        font-size: 0.85rem;
        letter-spacing: 1px;
        color: #E53935;
        margin-bottom: 4px;
        display: block;
      }

      /* ── User bubble (thought balloon, right) ── */
      .chat_input {
        position: relative;
        max-width: 65%;
        background: #1565C0;
        border: 3px solid #111;
        border-radius: 18px 18px 4px 18px;
        padding: 12px 16px;
        margin: 6px 8px 18px auto;
        font-family: 'Comic Neue', cursive;
        font-size: 1rem;
        font-weight: 700;
        line-height: 1.5;
        color: #fff;
        box-shadow: 3px 3px 0 #111;
        animation: popIn 0.25s cubic-bezier(.175,.885,.32,1.275) forwards;
      }
      .chat_input::before {
        content: '';
        position: absolute;
        bottom: -13px;
        right: 18px;
        border-width: 12px 0 0 10px;
        border-style: solid;
        border-color: #111 transparent;
      }
      .chat_input::after {
        content: '';
        position: absolute;
        bottom: -9px;
        right: 20px;
        border-width: 10px 0 0 8px;
        border-style: solid;
        border-color: #1565C0 transparent;
      }

      /* ── Input row ── */
      #chat_input_row {
        display: flex;
        align-items: stretch;
        border-top: 5px solid #111;
        background: #E53935;
      }
      #chat_input_row textarea {
        flex: 1;
        border: none !important;
        border-right: 4px solid #111 !important;
        border-radius: 0 !important;
        background: #FFFDE7 !important;
        font-family: 'Comic Neue', cursive !important;
        font-size: 1rem !important;
        font-weight: 700;
        color: #111 !important;
        padding: 12px 14px !important;
        resize: none;
        outline: none;
        box-shadow: none !important;
      }
      #chat_input_row textarea::placeholder {
        color: #888;
        font-style: italic;
      }
      #send_text {
        font-family: 'Bangers', cursive !important;
        font-size: 1.4rem !important;
        letter-spacing: 2px;
        color: #111 !important;
        background: #FFEB3B !important;
        border: none !important;
        border-left: 0 !important;
        border-radius: 0 !important;
        width: 120px;
        padding: 0 !important;
        cursor: pointer;
        transition: background 0.15s, transform 0.1s;
        text-shadow: 1px 1px 0 #E53935;
      }
      #send_text:hover {
        background: #fff176 !important;
        transform: scale(1.04);
      }
      #send_text:active {
        transform: scale(0.97);
      }

      /* ── Animations ── */
      @keyframes popIn {
        from { opacity: 0; transform: scale(0.85) translateY(6px); }
        to   { opacity: 1; transform: scale(1)    translateY(0);   }
      }

      /* ── Markdown inside bubbles ── */
      .chat_reply p, .chat_input p { margin: 0 0 6px 0; }
      .chat_reply ul, .chat_reply ol { margin: 4px 0 4px 20px; }
      .chat_reply code {
        background: #f5f5f5;
        border: 1px solid #ccc;
        border-radius: 3px;
        padding: 1px 5px;
        font-size: 0.9em;
      }
      .chat_reply strong { color: #E53935; }

      /* ── Footer tag ── */
      #comic_footer {
        background: #111;
        color: #FFEEB3;
        font-family: 'Bangers', cursive;
        font-size: 0.85rem;
        letter-spacing: 2px;
        text-align: center;
        padding: 5px;
      }

      /* fix bslib/shiny form margin */
      .form-group { margin: 0 !important; }
      .shiny-input-container { width: 100% !important; margin: 0 !important; }
    "))
  ),
  
  # ── JS: streaming handler ──
  tags$script(HTML("
    var chunks = '';
    const md = markdownit();
    Shiny.addCustomMessageHandler('newReply', function(chunk) {
      chunks = '';
    });
    Shiny.addCustomMessageHandler('updateReply', function(chunk) {
      chunks = chunks + chunk;
      var bubbles = document.querySelectorAll('.chat_reply');
      var last = bubbles[bubbles.length - 1];
      var content = last.querySelector('.bubble_content');
      if (content) content.innerHTML = md.render(chunks);
    });
  ")),
  
  # ── Markup ──
  div(id = "comic_outer",
      
      # Title bar
      div(id = "comic_title",
          tags$h1("CHAT MSF"),
          span(class = "subtitle", "Marvel Strike Force Intel"),
          span(class = "issue", "ISSUE #1")
      ),
      
      # Chat output
      div(id = "chat_output",
          # Opening bubble
          div(class = "chat_reply",
              span(class = "speaker", "AGENT ALPHA"),
              div(class = "bubble_content",
                  "Greetings, Commander! I'm your MSF tactical AI — powered by your character stats, ",
                  "meta data, and abilities intel. Ask me anything about the roster, tier lists, or synergies!"
              )
          )
      ),
      
      div(class = "panel-gutter"),
      
      # Input row
      div(id = "chat_input_row",
          textAreaInput(
            "textarea_chat_input",
            label    = NULL,
            width    = "100%",
            height   = "68px",
            resize   = "none",
            placeholder = "Ask about a character, team synergy, or tier ranking..."
          ),
          actionButton("send_text", label = "SEND!")
      ),
      
      div(id = "comic_footer", "POWERED BY MSF DATA · CLASSIFIED INTEL · NOT FOR PUBLIC RELEASE")
  )
)

# ── 3.  Server ───────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  chat <- ellmer::chat_github(
    system_prompt = system_prompt_text,
    base_url      = "https://models.inference.ai.azure.com/",
    api_key       = Sys.getenv("GITHUB_PAT"),
    model         = "gpt-4o",
    seed          = NULL,
    api_args      = list(),
    echo          = FALSE
  )
  
  observe({
    req(input$textarea_chat_input != "")
    
    user_text <- input$textarea_chat_input
    stream    <- chat$stream(user_text)
    
    # Insert user bubble
    insertUI(
      "#chat_output", where = "beforeEnd",
      ui = div(class = "chat_input", user_text),
      immediate = TRUE
    )
    
    updateTextAreaInput(inputId = "textarea_chat_input", value = "")
    
    # Insert empty AI bubble
    insertUI(
      "#chat_output", where = "beforeEnd",
      ui = div(class = "chat_reply",
               span(class = "speaker", "AGENT ALPHA"),
               div(class = "bubble_content", "")
      ),
      immediate = TRUE
    )
    
    # Stream response into bubble
    new_msg <- TRUE
    coro::loop(for (chunk in stream) {
      if (new_msg) session$sendCustomMessage("newReply", 1)
      session$sendCustomMessage("updateReply", chunk)
      new_msg <- FALSE
    })
    
    # Auto-scroll to bottom
    session$sendCustomMessage(
      type    = "scrollDown",
      message = list()
    )
    
  }) |> bindEvent(input$send_text)
}

# ── 4.  Run ───────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
