# =============================================================================
# Marvel Strike Force — Ability Synergy Scraper
# =============================================================================
# Scrapes ability text from each character's page on marvelstrikeforce.com,
# then uses regex patterns to extract structured synergy data from the text.
#
# USAGE: Run msf_character_scraper.R first so chars_final exists in your
#        environment, then source() this script in the same R session.
#        Alternatively, run both scripts together in a single R session.
#
# OUTPUT: msf_synergies.csv
#   character_id  — matches character_id in chars_final
#   ability       — basic / special / ultimate / passive
#   synergy_tag   — the tag referenced (e.g. "Daring Warrior", "X-Men", "Hero")
#   synergy_type  — ally (buffs teammates), enemy (debuffs opponents), self
#   ability_text  — raw ability description scraped from the site
#
# SETUP (run once):
#   install.packages(c("chromote", "rvest", "dplyr", "readr", "stringr"))
#
# Chrome path (if not auto-detected):
#   Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/Google/Chrome/Application/chrome.exe")
#   Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
#   Sys.setenv(CHROMOTE_CHROME = "/usr/bin/google-chrome")
# =============================================================================

library(chromote)
library(rvest)
library(dplyr)
library(readr)
library(stringr)

# =============================================================================
# CONFIG
# =============================================================================

OUTPUT_FILE    <- "msf_synergies.csv"
BASE_URL       <- "https://marvelstrikeforce.com"

ABILITY_WAIT   <- 5     # seconds to wait for ability page to render
ABILITY_TYPES  <- c("basic", "special", "ultimate", "passive")

CLAUDE_MAX_TOK <- 1000

# =============================================================================
# LOAD CHARACTER LIST FROM chars_final
# =============================================================================

if (!exists("chars_final")) {
  stop(
    "'chars_final' not found in your environment.
",
    "Run msf_character_scraper.R first (in the same R session), then re-run this script."
  )
}

chars <- chars_final |>
  select(character_name, character_id) |>
  distinct()

message(sprintf("── Loaded %d characters from chars_final", nrow(chars)))

# =============================================================================
# START CHROME
# =============================================================================

message("\n── Starting Chrome ...")
chrome_path <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
if (is.null(chrome_path) || !nzchar(chrome_path)) {
  stop(
    "\nChrome not found. Set path with:\n",
    '  Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/Google/Chrome/Application/chrome.exe")\n',
    '  Sys.setenv(CHROMOTE_CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")\n',
    '  Sys.setenv(CHROMOTE_CHROME = "/usr/bin/google-chrome")\n'
  )
}
b <- ChromoteSession$new()
on.exit(try(b$close(), silent = TRUE), add = TRUE)

get_html <- function() {
  read_html(b$Runtime$evaluate("document.documentElement.outerHTML")$result$value)
}

# =============================================================================
# SCRAPE ABILITY TEXT PER CHARACTER
# =============================================================================
# Each character's abilities live at:
#   /characters/{character_id}/abilities
# The page renders 4 ability cards: basic, special, ultimate, passive.
# We scrape the text from each card.
# =============================================================================

message("\n── Scraping ability text for each character ...")

# CSS selectors to try for ability cards and their text, in priority order.
# The site uses JS-rendered components — we try several common patterns.
ABILITY_CARD_SELECTORS <- c(
  "[class*='AbilityCard']",
  "[class*='ability-card']",
  "[class*='AbilityItem']",
  "[class*='ability-item']",
  "[class*='Ability']",
  "[class*='SkillCard']",
  "[class*='skill-card']"
)

TEXT_SELECTORS <- c(
  "[class*='AbilityDescription']",
  "[class*='ability-description']",
  "[class*='description']",
  "[class*='Description']",
  "p",
  "span"
)

NAME_SELECTORS <- c(
  "[class*='AbilityName']",
  "[class*='ability-name']",
  "[class*='AbilityTitle']",
  "[class*='ability-title']",
  "h3", "h4"
)

scrape_ability_text <- function(pg) {
  # Try to find distinct ability card containers
  cards <- NULL
  for (sel in ABILITY_CARD_SELECTORS) {
    cards <- tryCatch(pg |> html_elements(sel), error = function(e) NULL)
    if (!is.null(cards) && length(cards) >= 2) break
    cards <- NULL
  }

  if (!is.null(cards) && length(cards) >= 2) {
    # Extract name + description from each card
    result <- lapply(seq_along(cards), function(i) {
      card <- cards[[i]]

      name_text <- NA_character_
      for (sel in NAME_SELECTORS) {
        name_text <- tryCatch(
          card |> html_element(sel) |> html_text(trim = TRUE),
          error = function(e) NA_character_
        )
        if (!is.na(name_text) && nzchar(name_text)) break
      }

      desc_text <- NA_character_
      for (sel in TEXT_SELECTORS) {
        desc_text <- tryCatch(
          card |> html_elements(sel) |> html_text(trim = TRUE) |>
            (\(x) x[nzchar(x)])() |> paste(collapse = " "),
          error = function(e) NA_character_
        )
        if (!is.na(desc_text) && nchar(desc_text) > 20) break
      }

      list(
        ability_slot = ABILITY_TYPES[min(i, length(ABILITY_TYPES))],
        ability_name = name_text %||% paste("Ability", i),
        ability_text = desc_text %||% NA_character_
      )
    })

    return(bind_rows(lapply(result, as.data.frame, stringsAsFactors = FALSE)))
  }

  # Fallback: no card containers found — grab all substantial text blocks
  # and label them by position
  all_text <- pg |>
    html_elements("p, [class*='description'], [class*='Description']") |>
    html_text(trim = TRUE) |>
    (\(x) x[nchar(x) > 30])()

  if (length(all_text) == 0) return(NULL)

  n <- min(length(all_text), 4)
  data.frame(
    ability_slot = ABILITY_TYPES[seq_len(n)],
    ability_name = paste("Ability", seq_len(n)),
    ability_text = all_text[seq_len(n)],
    stringsAsFactors = FALSE
  )
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# Scrape each character
raw_abilities <- vector("list", nrow(chars))

for (i in seq_len(nrow(chars))) {
  cid  <- chars$character_id[i]
  name <- chars$character_name[i]
  url  <- sprintf("%s/characters/%s/abilities", BASE_URL, cid)

  message(sprintf("   [%d/%d] %s", i, nrow(chars), name))

  tryCatch({
    b$Page$navigate(url)
    Sys.sleep(ABILITY_WAIT)
    pg <- get_html()

    abilities <- scrape_ability_text(pg)

    if (!is.null(abilities) && nrow(abilities) > 0) {
      abilities$character_id <- cid
      raw_abilities[[i]] <- abilities
    } else {
      message("     (no ability text found)")
    }
  }, error = function(e) {
    message(sprintf("     WARNING: %s", conditionMessage(e)))
  })

  Sys.sleep(0.3)
}

raw_abilities_df <- bind_rows(raw_abilities)
message(sprintf("\n   Scraped %d ability text blocks across %d characters.",
                nrow(raw_abilities_df),
                n_distinct(raw_abilities_df$character_id)))

# Save raw ability text as an intermediate file (useful for debugging)
write_csv(raw_abilities_df, "/Users/matthewdanna/Documents/GitHub/r-samples/msf/msf_abilities_text.csv")
message("   Raw ability text saved")

# =============================================================================
# EXTRACT SYNERGIES WITH REGEX
# =============================================================================
# Scans each ability text for known tag names and classifies the synergy type
# based on surrounding context words:
#   ally  — tag appears near: ally, allies, friendly, teammates, support, heal,
#            buff, gain, on this team, lowest health
#   enemy — tag appears near: enemy, enemies, opponent, target, debuff, attack,
#            vs, against
#   self  — conditional self-buff: "if X+ <tag>", "when <tag>", "on spawn if"
# =============================================================================

message("\n── Extracting synergies with regex ...")

# All known MSF tags — paste your full tag list here so the extractor knows
# exactly what to look for. Order longer/more-specific names first so they
# match before a shorter substring would.
ALL_TAGS <- c(
  "Daring Warrior", "Bionic Avenger", "Wave I Avenger", "New Avenger",
  "Young Avenger", "Black Order", "Masters of Evil", "Secret Defender",
  "Secret Warrior", "Midnight Sons", "Mercs for Money", "Heroes for Hire",
  "Sinister Six", "Spider-Verse",
  "Infinity Watch", "Unlimited X-Men", "Death Seed", "P.E.G.A.S.U.S.",
  "Web-Warrior", "Power Armor", "Dark Hunter", "New Warrior",
  "Infestation", "Pym Tech", "X-Factor", "X-Force",
  "X-Men", "A-Force", "S.H.I.E.L.D.", "A.I.M.",
  "Hydra", "Kree", "Hand", "Asgardian",
  "Avenger", "Brotherhood", "Cabal",
  "Darkhold", "Defender", "Eternal", "Gamma",
  "Guardian", "Horsemen", "Illuminati", "Inhuman",
  "Invader", "Mercenary", "Military",
  "Ravager", "Symbiote", "Weapon X", 
  "Annihilator", "Bifrost", "Rebirth", 
  "Absolute A-Force", "Accursed", "Alpha Flight",
  "Asgard", "Astonishing", "Astral", "Brimstone",
  "Champion", "Fantastic Four", "Galactic Council", "Guardians",
  "Hellfire", "Hive", "Horseman", "Immortal Weapon",
  "Immortal X-Men", "Insidious", "Knowhere", "Liberty", "Marauders", "Mighty Avenger", 
  "New Mutant", "Nightstalker", "Orchis",
  "Out Of Time", "Phoenix Force", "Retcon", "Secret Avenger",
  "Shadowland", "Spider-Society", "Starjammer", "Superior Six",
  "Supernatural", "Tangled", "Thunderbolt", "Uncanny Avenger",
  "Uncanny", "Underworld", "Undying", "Unlimited",
  "Vampire", "Vigilante", "Wakanda", "War Dog",
  "Slinger", "WinterGuard", "Xtreme",
  "Harbinger", "Conqueror", "Stormbound", "Couples", "Epic", "Exposed",
  "Hero", "Villain", "Bio", "Mutant",
  "Cosmic", "Mystic", "Skill", "Tech",
  "City", "Global", "Legendary", "Mythic", "Minion",
  "BLASTER", "BRAWLER", "CONTROLLER", "PROTECTOR", "SUPPORT")

# Words that signal ally synergy (tag benefits teammates)
ALLY_WORDS    <- paste(c(
  "all(?:y|ies)", "teammate", "friendly", "support", "heal", "buff",
  "grant", "on this team", "lowest health", "most injured", "adjacent"
), collapse = "|")

# Words that signal enemy synergy (tag used to target/debuff opponents)
ENEMY_WORDS   <- paste(c(
  "enem(?:y|ies)", "opponent", "target(?:ed)?", "attack", "debuff",
  "strike", "vs\\.?", "against", "damage"
), collapse = "|")

# Words that signal a self-conditional (this character gets a buff if tag present)
SELF_WORDS    <- paste(c(
  "if \\d+\\+", "if [2-9]\\+", "on spawn", "when .{1,30} on (?:this|your) team",
  "for each", "per ", "gains? (?:an? )?(?:extra|additional|bonus)"
), collapse = "|")

classify_synergy <- function(tag, text) {
  # Find the position of the tag mention
  pos <- str_locate(str_to_lower(text), str_to_lower(fixed(tag)))[1]
  if (is.na(pos)) return(NA_character_)

  # Grab a 120-char window around the tag for context
  start   <- max(1, pos - 60)
  end_pos <- min(nchar(text), pos + nchar(tag) + 60)
  window  <- str_to_lower(substr(text, start, end_pos))

  if (str_detect(window, regex(SELF_WORDS,  ignore_case = TRUE))) return("self")
  if (str_detect(window, regex(ENEMY_WORDS, ignore_case = TRUE))) return("enemy")
  if (str_detect(window, regex(ALLY_WORDS,  ignore_case = TRUE))) return("ally")
  return("ally")   # default: if a tag is mentioned it's most often an ally synergy
}

extract_synergies <- function(cid, slot, txt) {
  if (is.na(txt) || !nzchar(txt)) return(NULL)
  
  rows <- lapply(ALL_TAGS, function(tag) {
    # Case-insensitive whole-word match (word boundary or punctuation boundary)
    pattern <- paste0("(?i)(?<![a-zA-Z])", str_replace_all(fixed(tag), "\\.", "\\\\."), "(?![a-zA-Z])")
    if (!str_detect(txt, regex(pattern, ignore_case = TRUE))) return(NULL)
    
    synergy_type <- classify_synergy(tag, txt)
    data.frame(
      character_id = cid,
      ability      = slot,
      synergy_tag  = tag,
      synergy_type = synergy_type,
      ability_text = txt,
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(Filter(Negate(is.null), rows))
}

# Process each ability text block
synergy_rows <- vector("list", nrow(raw_abilities_df))

for (i in seq_len(nrow(raw_abilities_df))) {
  row  <- raw_abilities_df[i, ]
  cid  <- row$character_id
  slot <- row$ability_slot
  txt  <- row$ability_text

  if (is.na(txt) || !nzchar(txt)) next

  result <- extract_synergies(cid, slot, txt)

  if (!is.null(result) && nrow(result) > 0) {
    synergy_rows[[i]] <- result
    message(sprintf("   [%d/%d] %s / %s  → %s",
                    i, nrow(raw_abilities_df), cid, slot,
                    paste(result$synergy_tag, collapse=", ")))
  }
}

# =============================================================================
# ASSEMBLE AND SAVE
# =============================================================================

synergies_df <- bind_rows(Filter(Negate(is.null), synergy_rows)) |>
  distinct(character_id, ability, synergy_tag, .keep_all = TRUE) |>
  arrange(character_id, ability, synergy_tag)

write_csv(synergies_df, "/Users/matthewdanna/Documents/GitHub/r-samples/msf/msf_abilities_synergies.csv")

message(sprintf("Done! %d synergy rows across %d characters",
                nrow(synergies_df),
                if (nrow(synergies_df) > 0) n_distinct(synergies_df$character_id) else 0L))
