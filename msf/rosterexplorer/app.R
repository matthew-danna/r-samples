# =============================================================================
# Marvel Strike Force — Roster Xplorer
# Shiny App  |  by the_notorious_md on Discord
# =============================================================================
# REQUIRES these data frames in your R environment (run both scraper scripts first):
#   chars_final      — characters, Y/N tag cols, numeric stat cols
#   raw_abilities_df — character_id, ability_slot, ability_name, ability_text
#   synergies_df     — character_id, ability, synergy_tag, synergy_type, ability_text
#
# RUN:
#   install.packages(c("shiny", "dplyr", "stringr", "DT", "tidyr", "visNetwork"))
#   source("msf_app.R")
# =============================================================================

library(shiny)
library(dplyr)
library(stringr)
library(DT)
library(tidyr)
library(visNetwork)

# =============================================================================
# DATA LOAD
# =============================================================================
# Data frames are loaded from GitHub if not already present in the environment.
# To use your own local versions instead, just have them loaded before sourcing
# this app — it will skip the download if they already exist.

GITHUB_BASE <- "https://raw.githubusercontent.com/matthew-danna/r-samples/main/msf"

load_or_fetch <- function(var_name, filename) {
  if (exists(var_name, envir = .GlobalEnv)) {
    df <- get(var_name, envir = .GlobalEnv)
    message(sprintf("  Using existing '%s' from environment (%d rows)", var_name, nrow(df)))
    return(df)
  }
  url <- paste0(GITHUB_BASE, "/", filename)
  message(sprintf("  '%s' not found — downloading from GitHub...", var_name))
  df <- tryCatch(
    readr::read_csv(url, show_col_types = FALSE),
    error = function(e) stop(sprintf(
      "Failed to load '%s' from GitHub.\nURL: %s\nError: %s\n\nEither run the scraper scripts first, or check your internet connection.",
      var_name, url, conditionMessage(e)
    ))
  )
  message(sprintf("  Loaded '%s': %d rows x %d cols", var_name, nrow(df), ncol(df)))
  df
}

message("── Loading data...")
chars_final      <- load_or_fetch("chars_final",      "msf_characters.csv")
raw_abilities_df <- load_or_fetch("raw_abilities_df", "msf_abilities_text.csv")
synergies_df     <- load_or_fetch("synergies_df",     "msf_abilities_synergies.csv")
message("── Data ready.")

# =============================================================================
# DATA PREP
# =============================================================================

# Sanitise column names — replace spaces/special chars with underscores
# so cols like "crit damage" and "#" don't break downstream code
names(chars_final) <- make.names(names(chars_final), unique = TRUE)

# Tag cols = columns that only contain "Y" / "N"
is_yn_col <- function(col) { vals <- unique(na.omit(col)); all(vals %in% c("Y","N")) && length(vals) <= 2 }
tag_cols <- names(chars_final)[vapply(chars_final, is_yn_col, logical(1))]

# Stat cols = numeric columns that are not id/index columns
NON_STAT_COLS <- c("X.", "X", "character_id")   # "#" becomes "X." after make.names
stat_cols <- names(chars_final)[vapply(chars_final, is.numeric, logical(1))]
stat_cols <- setdiff(stat_cols, NON_STAT_COLS)

all_tags   <- sort(tag_cols)
char_names <- sort(unique(chars_final$character_name))

# Ability slot labels
ABILITY_LABELS <- c(
  "basic"    = "Basic",
  "special"  = "Special",
  "ultimate" = "Ultimate",
  "passive"  = "Passive"
)

# =============================================================================
# CSS
# =============================================================================

comic_css <- '
@import url("https://fonts.googleapis.com/css2?family=Bangers&family=Barlow+Condensed:wght@400;600;700&family=Barlow:wght@400;500&display=swap");

:root {
  --black:   #0a0a0f;
  --navy:    #0d1b2a;
  --panel:   #111827;
  --border:  #1e3a5f;
  --yellow:  #ffd700;
  --red:     #e8001d;
  --blue:    #1a6bce;
  --text:    #e8eaf0;
  --muted:   #7a8499;
  --success: #00c853;
  --radius:  4px;
}

* { box-sizing: border-box; }

body {
  background-color: var(--black);
  color: var(--text);
  font-family: "Barlow", sans-serif;
  font-size: 15px;
  margin: 0;
  background-image:
    radial-gradient(circle, rgba(255,215,0,0.03) 1px, transparent 1px);
  background-size: 24px 24px;
}

/* ── Header ── */
.msf-header {
  background: linear-gradient(135deg, #0d1b2a 0%, #0a0a0f 60%, #1a0005 100%);
  border-bottom: 3px solid var(--yellow);
  padding: 18px 32px 14px;
  display: flex;
  align-items: center;
  gap: 20px;
  position: relative;
  overflow: hidden;
}
.msf-header::before {
  content: "";
  position: absolute;
  inset: 0;
  background: repeating-linear-gradient(
    -45deg, transparent, transparent 10px,
    rgba(255,215,0,0.015) 10px, rgba(255,215,0,0.015) 11px
  );
  pointer-events: none;
}
.msf-logo {
  font-family: "Bangers", cursive;
  font-size: 42px;
  letter-spacing: 3px;
  color: var(--yellow);
  text-shadow: 3px 3px 0 var(--red), 6px 6px 0 rgba(0,0,0,0.5);
  line-height: 1;
  white-space: nowrap;
}
.msf-subtitle {
  font-family: "Barlow Condensed", sans-serif;
  font-size: 13px;
  font-weight: 600;
  letter-spacing: 3px;
  text-transform: uppercase;
  color: var(--muted);
  margin-top: 4px;
}
.msf-badge {
  margin-left: auto;
  font-family: "Barlow Condensed", sans-serif;
  font-size: 11px;
  letter-spacing: 1px;
  color: var(--muted);
  text-align: right;
  line-height: 1.6;
}
.msf-badge strong { color: var(--yellow); font-size: 13px; }

/* ── Nav tabs ── */
.nav-tabs {
  background: var(--navy);
  border-bottom: 2px solid var(--border);
  padding: 0 24px;
  display: flex;
  gap: 4px;
}
.nav-tabs > li > a {
  font-family: "Bangers", cursive;
  font-size: 18px;
  letter-spacing: 1.5px;
  color: var(--muted) !important;
  background: transparent !important;
  border: none !important;
  border-bottom: 3px solid transparent !important;
  padding: 10px 20px !important;
  margin-bottom: -2px;
  transition: color 0.15s, border-color 0.15s;
  border-radius: 0 !important;
}
.nav-tabs > li > a:hover { color: var(--text) !important; }
.nav-tabs > li.active > a {
  color: var(--yellow) !important;
  border-bottom-color: var(--yellow) !important;
  background: transparent !important;
}

/* ── Tab content ── */
.tab-content { padding: 24px; }

/* ── Sidebar / control panels ── */
.control-panel {
  background: var(--panel);
  border: 1px solid var(--border);
  border-left: 4px solid var(--yellow);
  border-radius: var(--radius);
  padding: 16px;
  margin-bottom: 16px;
}
.control-panel h4 {
  font-family: "Bangers", cursive;
  font-size: 20px;
  letter-spacing: 1px;
  color: var(--yellow);
  margin: 0 0 12px;
}

/* ── Inputs ── */
.form-control, .selectize-input {
  background: #1a2233 !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
  border-radius: var(--radius) !important;
  font-family: "Barlow", sans-serif !important;
}
.selectize-dropdown {
  background: #1a2233 !important;
  border: 1px solid var(--border) !important;
  color: var(--text) !important;
}
.selectize-dropdown-content .option { padding: 6px 10px; }
.selectize-dropdown-content .option:hover,
.selectize-dropdown-content .option.active {
  background: var(--border) !important;
  color: var(--yellow) !important;
}
.selectize-input.focus { border-color: var(--yellow) !important; box-shadow: none !important; }
label { color: var(--muted); font-size: 12px; font-weight: 600; letter-spacing: 1px; text-transform: uppercase; }

/* ── Buttons ── */
.btn-comic {
  font-family: "Bangers", cursive;
  font-size: 18px;
  letter-spacing: 1.5px;
  background: var(--yellow);
  color: var(--black);
  border: none;
  padding: 8px 24px;
  border-radius: var(--radius);
  cursor: pointer;
  transition: transform 0.1s, box-shadow 0.1s;
  box-shadow: 3px 3px 0 var(--red);
}
.btn-comic:hover { transform: translate(-1px,-1px); box-shadow: 4px 4px 0 var(--red); }
.btn-comic:active { transform: translate(1px,1px); box-shadow: 2px 2px 0 var(--red); }
.btn-comic.btn-danger {
  background: var(--red);
  color: #fff;
  box-shadow: 3px 3px 0 #6b0010;
}

/* ── Character cards ── */
.char-card {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 14px;
  margin-bottom: 10px;
  cursor: pointer;
  transition: border-color 0.15s, background 0.15s;
  position: relative;
}
.char-card:hover { border-color: var(--yellow); background: #151f30; }
.char-card.selected {
  border-color: var(--yellow);
  border-left: 4px solid var(--yellow);
  background: #1a2235;
}
.char-name {
  font-family: "Bangers", cursive;
  font-size: 20px;
  letter-spacing: 1px;
  color: var(--yellow);
  margin-bottom: 6px;
}
.char-id { font-size: 11px; color: var(--muted); letter-spacing: 0.5px; }

/* ── Tag pills ── */
.tag-pill {
  display: inline-block;
  font-family: "Barlow Condensed", sans-serif;
  font-size: 11px;
  font-weight: 700;
  letter-spacing: 0.5px;
  text-transform: uppercase;
  padding: 2px 8px;
  border-radius: 2px;
  margin: 2px 2px 2px 0;
}
.tag-pill.type   { background: #1a3a6b; color: #6ab0ff; border: 1px solid #1a5abf; }
.tag-pill.team   { background: #3a1a0a; color: #ff9052; border: 1px solid #8b3a10; }
.tag-pill.origin { background: #0a2a1a; color: #52ff9a; border: 1px solid #0a6b3a; }
.tag-pill.role   { background: #2a1a3a; color: #c07aff; border: 1px solid #6a1abf; }
.tag-pill.ally   { background: #1a3a1a; color: #6aff6a; }
.tag-pill.enemy  { background: #3a1a1a; color: #ff6a6a; }
.tag-pill.self   { background: #1a2a3a; color: #6ab4ff; }

/* ── Stat bars ── */
.stat-row {
  display: flex;
  align-items: center;
  gap: 10px;
  margin-bottom: 5px;
}
.stat-label {
  font-family: "Barlow Condensed", sans-serif;
  font-size: 12px;
  font-weight: 700;
  color: var(--muted);
  text-transform: uppercase;
  letter-spacing: 0.5px;
  min-width: 80px;
  text-align: right;
}
.stat-bar-wrap {
  flex: 1;
  height: 8px;
  background: #1a2233;
  border-radius: 2px;
  overflow: hidden;
}
.stat-bar-fill {
  height: 100%;
  background: linear-gradient(90deg, var(--blue), var(--yellow));
  border-radius: 2px;
  transition: width 0.4s;
}
.stat-value {
  font-family: "Barlow Condensed", sans-serif;
  font-size: 13px;
  font-weight: 600;
  color: var(--text);
  min-width: 60px;
}

/* ── Section dividers ── */
.section-head {
  font-family: "Bangers", cursive;
  font-size: 26px;
  letter-spacing: 2px;
  color: var(--yellow);
  border-bottom: 2px solid var(--border);
  padding-bottom: 6px;
  margin: 20px 0 14px;
}
.section-head span {
  background: var(--yellow);
  color: var(--black);
  padding: 0 8px;
  margin-right: 8px;
}

/* ── Ability cards ── */
.ability-card {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: var(--radius);
  padding: 14px;
  margin-bottom: 10px;
}
.ability-slot {
  font-family: "Bangers", cursive;
  font-size: 14px;
  letter-spacing: 1px;
  color: var(--muted);
  text-transform: uppercase;
}
.ability-name {
  font-family: "Barlow Condensed", sans-serif;
  font-size: 17px;
  font-weight: 700;
  color: var(--text);
  margin: 2px 0 8px;
}
.ability-text {
  font-size: 14px;
  color: #b0bcd0;
  line-height: 1.6;
}

/* ── DT tables ── */
.dataTables_wrapper { color: var(--text) !important; }
table.dataTable { background: var(--panel) !important; color: var(--text) !important; border-collapse: collapse !important; }
table.dataTable thead th {
  background: var(--navy) !important;
  color: var(--yellow) !important;
  font-family: "Bangers", cursive !important;
  font-size: 16px !important;
  letter-spacing: 1px !important;
  border-bottom: 2px solid var(--border) !important;
}
table.dataTable tbody tr { background: var(--panel) !important; }
table.dataTable tbody tr:hover { background: #151f30 !important; }
table.dataTable tbody td { border-bottom: 1px solid var(--border) !important; padding: 8px 12px !important; }
.dataTables_filter input { background: #1a2233 !important; border: 1px solid var(--border) !important; color: var(--text) !important; border-radius: var(--radius) !important; }
.dataTables_length select { background: #1a2233 !important; border: 1px solid var(--border) !important; color: var(--text) !important; }
.dataTables_paginate .paginate_button { color: var(--muted) !important; }
.dataTables_paginate .paginate_button.current, .dataTables_paginate .paginate_button:hover { background: var(--border) !important; color: var(--yellow) !important; border-radius: var(--radius) !important; }

/* ── Counter result highlight ── */
.counter-hit {
  background: #0a1f0a;
  border: 1px solid #1a5a1a;
  border-left: 4px solid var(--success);
  border-radius: var(--radius);
  padding: 12px 14px;
  margin-bottom: 8px;
}
.counter-score {
  font-family: "Bangers", cursive;
  font-size: 28px;
  color: var(--success);
  float: right;
}

/* ── Empty state ── */
.empty-state {
  text-align: center;
  padding: 60px 20px;
  color: var(--muted);
}
.empty-state .big { font-family: "Bangers", cursive; font-size: 48px; color: var(--border); }

/* ── Synergy summary ── */
.synergy-summary {
  background: var(--panel);
  border: 1px solid var(--border);
  border-top: 3px solid var(--red);
  border-radius: var(--radius);
  padding: 16px;
  margin-top: 16px;
}
.synergy-count {
  font-family: "Bangers", cursive;
  font-size: 32px;
  color: var(--red);
}

/* ── Footer ── */
.msf-footer {
  border-top: 1px solid var(--border);
  padding: 12px 32px;
  text-align: center;
  font-size: 12px;
  color: var(--muted);
  margin-top: 40px;
}
.msf-footer strong { color: var(--yellow); }
'

# =============================================================================
# HELPERS
# =============================================================================

# Get tags a character has (returns character vector of tag names)
get_char_tags <- function(char_name) {
  row <- chars_final |> filter(character_name == char_name)
  if (nrow(row) == 0) return(character(0))
  vals <- unlist(row[1, tag_cols], use.names = FALSE)
  tag_cols[!is.na(vals) & vals == "Y"]
}

# Render tag pills HTML
render_tags <- function(tags, pill_class = "team") {
  if (length(tags) == 0) return("<span style='color:#7a8499'>None</span>")
  paste(sprintf('<span class="tag-pill %s">%s</span>', pill_class, tags), collapse = " ")
}

# Stat bar HTML
render_stat_bar <- function(label, value, max_val) {
  if (is.na(value) || is.na(max_val) || max_val == 0) return("")
  pct <- round(100 * value / max_val)
  sprintf('
    <div class="stat-row">
      <div class="stat-label">%s</div>
      <div class="stat-bar-wrap"><div class="stat-bar-fill" style="width:%d%%"></div></div>
      <div class="stat-value">%s</div>
    </div>',
    label, pct, formatC(value, format = "d", big.mark = ",")
  )
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(a[1])) a else b

# Character detail panel HTML
char_detail_html <- function(char_name) {
  row <- chars_final |> filter(character_name == char_name)
  if (nrow(row) == 0) return("")

  ctags       <- get_char_tags(char_name)
  origin_tags <- intersect(ctags, c("Bio","Cosmic","Mystic","Skill","Tech","Mutant"))
  type_tags   <- intersect(ctags, c("Hero","Villain","City","Global"))
  role_tags   <- intersect(ctags, c("Blaster","Brawler","Controller","Protector","Support",
                                     "BLASTER","BRAWLER","CONTROLLER","PROTECTOR","SUPPORT"))
  team_tags   <- setdiff(ctags, c(origin_tags, type_tags, role_tags))
  tag_html    <- paste(render_tags(type_tags,"type"), render_tags(origin_tags,"origin"),
                       render_tags(role_tags,"role"), render_tags(team_tags,"team"))

  # Stats
  stat_html <- ""
  if (length(stat_cols) > 0) {
    stat_maxes <- vapply(stat_cols, function(s) max(chars_final[[s]], na.rm = TRUE), numeric(1))
    stat_bars  <- vapply(stat_cols, function(s) render_stat_bar(s, row[[s]][1], stat_maxes[s]), character(1))
    stat_html  <- paste(stat_bars, collapse = "")
  }
  stat_section <- if (nzchar(stat_html)) {
    paste0('<div style="margin:12px 0">', stat_html, '</div>')
  } else {
    "<p style='color:#7a8499'>No stat data available.</p>"
  }

  # Abilities
  ab_rows <- raw_abilities_df |> filter(character_id == row$character_id[1])
  make_ability_card <- function(i) {
    r          <- ab_rows[i, ]
    slot_label <- ABILITY_LABELS[r$ability_slot] %||% r$ability_slot
    ab_name    <- if (!is.na(r$ability_name)) r$ability_name else ""
    ab_txt     <- if (!is.na(r$ability_text)) r$ability_text else "<em>No description available</em>"
    sprintf(
      '<div class="ability-card"><div class="ability-slot">%s</div><div class="ability-name">%s</div><div class="ability-text">%s</div></div>',
      slot_label, ab_name, ab_txt
    )
  }
  ab_html <- if (nrow(ab_rows) > 0) {
    paste(vapply(seq_len(nrow(ab_rows)), make_ability_card, character(1)), collapse = "")
  } else {
    "<p style='color:#7a8499'>No ability data available.</p>"
  }

  # Synergies
  syn_rows <- synergies_df |> filter(character_id == row$character_id[1])
  syn_html <- if (nrow(syn_rows) > 0) {
    by_ability <- split(syn_rows, syn_rows$ability)
    make_syn_block <- function(ab) {
      pills <- paste(sprintf(
        '<span class="tag-pill %s">%s · %s</span>',
        by_ability[[ab]]$synergy_type,
        by_ability[[ab]]$synergy_tag,
        toupper(substr(by_ability[[ab]]$synergy_type, 1, 1))
      ), collapse = " ")
      sprintf(
        '<div style="margin-bottom:6px"><span style="color:#7a8499;font-size:12px;text-transform:uppercase;letter-spacing:1px">%s:</span> %s</div>',
        ABILITY_LABELS[ab] %||% ab, pills
      )
    }
    paste(vapply(names(by_ability), make_syn_block, character(1)), collapse = "")
  } else {
    "<p style='color:#7a8499'>No tag synergies detected in abilities.</p>"
  }

  # Assemble — no inline if/else inside sprintf arguments
  paste0(
    '<div class="char-name" style="font-size:28px">', char_name, '</div>',
    '<div class="char-id">ID: ', row$character_id[1], '</div>',
    '<div style="margin:12px 0">', tag_html, '</div>',
    '<div class="section-head"><span>STATS</span></div>', stat_section,
    '<div class="section-head"><span>ABILITIES</span></div>', ab_html,
    '<div class="section-head"><span>SYNERGIES</span></div>', syn_html
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  tags$head(
    tags$style(HTML(comic_css)),
    tags$title("MSF X-plorer")
  ),

  # Header
  div(class = "msf-header",
    div(
      div(class = "msf-logo", "MSF X-PLORER"),
      div(class = "msf-subtitle", "Marvel Strike Force · Character Explorer")
    ),
    div(class = "msf-badge",
      div("Made by"),
      tags$strong("the_notorious_md"),
      div("on Discord")
    )
  ),

  # Navigation
  navbarPage(
    title = NULL,
    windowTitle = "MSF X-plorer",
    theme = NULL,
    position = "static-top",
    collapsible = TRUE,
    id = "main_nav",
    header = NULL,

    # ── TAB 1: Character Search ──────────────────────────────────────────────
    tabPanel("⚡ ROSTER",
      fluidRow(
        column(3,
          div(class = "control-panel",
            tags$h4("FILTER ROSTER"),
            selectizeInput("search_tags", "HAS TAGS (all must match)",
              choices = all_tags, multiple = TRUE,
              options = list(placeholder = "Any tag...", plugins = list("remove_button"))
            ),

            tags$hr(style="border-color:#1e3a5f"),
            textInput("search_name", "SEARCH BY NAME", placeholder = "e.g. Spider..."),
            tags$hr(style="border-color:#1e3a5f"),
            tags$p(style="color:#7a8499;font-size:12px",
              textOutput("roster_count", inline = TRUE), " characters shown"
            )
          )
        ),
        column(4,
          div(class = "section-head", tags$span("CHARACTERS")),
          div(id = "roster_list", style = "max-height:75vh; overflow-y:auto; padding-right:4px",
            uiOutput("roster_cards")
          )
        ),
        column(5,
          div(class = "section-head", tags$span("DETAIL")),
          div(id = "char_detail_panel",
            style = "background:var(--panel);border:1px solid var(--border);border-radius:4px;padding:20px;min-height:200px",
            uiOutput("char_detail")
          )
        )
      )
    ),

    # ── TAB 2: Team Builder ──────────────────────────────────────────────────
    tabPanel("🛡 TEAM BUILDER",
      fluidRow(
        column(4,
          div(class = "control-panel",
            tags$h4("BUILD YOUR TEAM"),
            tags$p(style = "color:#7a8499;font-size:13px", "Select up to 5 characters."),
            lapply(1:5, function(i) {
              selectizeInput(paste0("team_slot_", i),
                paste("SLOT", i),
                choices = c("— Empty —" = "", char_names),
                options = list(placeholder = "Pick a hero...")
              )
            }),
            tags$hr(style="border-color:#1e3a5f"),
            actionButton("clear_team", "CLEAR TEAM", class = "btn-comic btn-danger",
                         style = "width:100%")
          )
        ),
        column(8,
          div(class = "section-head", tags$span("TEAM ANALYSIS")),
          uiOutput("team_analysis")
        )
      )
    ),

    # ── TAB 3: Counter Finder ────────────────────────────────────────────────
    tabPanel("🎯 COUNTER FINDER",
      fluidRow(
        column(4,
          div(class = "control-panel",
            tags$h4("ENEMY TEAM TAGS"),
            tags$p(style = "color:#7a8499;font-size:13px",
              "Select the tags on the enemy team you want to counter."),
            selectizeInput("enemy_tags", "ENEMY TAGS",
              choices = all_tags, multiple = TRUE,
              options = list(placeholder = "Add enemy tags...", plugins = list("remove_button"))
            ),
            selectizeInput("counter_type", "SYNERGY TYPE TO FIND",
              choices = c("enemy" = "enemy", "any" = "any"),
              selected = "enemy"
            ),
            tags$hr(style="border-color:#1e3a5f"),
            actionButton("find_counters", "FIND COUNTERS", class = "btn-comic",
                         style = "width:100%")
          )
        ),
        column(8,
          div(class = "section-head", tags$span("BEST COUNTERS")),
          uiOutput("counter_results")
        )
      )
    ),

    # ── TAB 4: Ability Browser ───────────────────────────────────────────────
    tabPanel("📖 ABILITY BROWSER",
      fluidRow(
        column(3,
          div(class = "control-panel",
            tags$h4("SEARCH ABILITIES"),
            textInput("ability_search", "KEYWORD IN ABILITY TEXT",
              placeholder = "e.g. Bleed, Heal, Chain..."),
            selectizeInput("ability_char", "CHARACTER",
              choices = c("All" = "", char_names),
              options = list(placeholder = "All characters...")
            ),
            selectizeInput("ability_slot", "ABILITY TYPE",
              choices = c("All" = "", ABILITY_LABELS),
              options = list(placeholder = "All types...")
            ),
            selectizeInput("ability_syn_tag", "HAS SYNERGY WITH TAG",
              choices = c("Any" = "", sort(unique(na.omit(synergies_df$synergy_tag)))),
              options = list(placeholder = "Any tag...")
            )
          )
        ),
        column(9,
          div(class = "section-head", tags$span("ABILITIES")),
          DTOutput("ability_table")
        )
      )
    ),
    # ── TAB 5: Network Visualization ─────────────────────────────────────────
    tabPanel("🕸 NETWORK",
      fluidRow(
        column(3,
          div(class = "control-panel",
            tags$h4("NETWORK EXPLORER"),
            selectizeInput("net_character", "CENTER CHARACTER",
              choices = char_names,
              options = list(placeholder = "Pick a character...")
            ),
            tags$hr(style="border-color:#1e3a5f"),
            checkboxGroupInput("net_edge_types",
              "SHOW CONNECTIONS VIA",
              choices = c(
                "Shared tags/traits"     = "tags",
                "Ally synergies (kit)"   = "ally",
                "Enemy synergies (kit)"  = "enemy"
              ),
              selected = c("tags", "ally", "enemy")
            ),
            tags$hr(style="border-color:#1e3a5f"),
            sliderInput("net_depth", "DEGREES OF SEPARATION",
              min = 1, max = 2, value = 1, step = 1
            ),
            sliderInput("net_min_shared", "MIN SHARED TAGS TO CONNECT",
              min = 1, max = 5, value = 1, step = 1
            ),
            tags$hr(style="border-color:#1e3a5f"),
            tags$p(style="color:#7a8499;font-size:12px",
              "Node size = Power stat. ",
              "Gold = center character. ",
              "Blue = tag connection. ",
              "Red = enemy synergy. ",
              "Green = ally synergy."
            ),
            tags$p(style="color:#7a8499;font-size:12px",
              textOutput("net_node_count", inline=TRUE), " nodes · ",
              textOutput("net_edge_count", inline=TRUE), " edges"
            )
          )
        ),
        column(9,
          div(class = "section-head", tags$span("CHARACTER NETWORK")),
          div(
            style = "background:var(--panel);border:1px solid var(--border);border-radius:4px;padding:4px",
            visNetworkOutput("network_plot", height = "680px")
          ),
          div(style="margin-top:10px;color:#7a8499;font-size:12px",
            "Click a node to re-center · Scroll to zoom · Drag to pan"
          )
        )
      )
    )

  ),

  # Footer
  div(class = "msf-footer",
    "MSF X-plorer · Character Explorer · Built by ",
    tags$strong("the_notorious_md"),
    " on Discord · Data gathered via marvelstrikeforce.com"
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  selected_char <- reactiveVal(NULL)

  # ── ROSTER TAB ─────────────────────────────────────────────────────────────

  filtered_chars <- reactive({
    df <- chars_final

    # Name search
    if (nzchar(input$search_name)) {
      df <- df |> filter(str_detect(str_to_lower(character_name),
                                    str_to_lower(fixed(input$search_name))))
    }


    # Tag filters
    if (length(input$search_tags) > 0) {
      for (tg in input$search_tags) {
        if (tg %in% names(df)) df <- df |> filter(.data[[tg]] == "Y")
      }
    }

    df |> arrange(character_name)
  })

  output$roster_count <- renderText(nrow(filtered_chars()))

  output$roster_cards <- renderUI({
    chars <- filtered_chars()
    if (nrow(chars) == 0) {
      return(div(class = "empty-state",
        div(class = "big", "???"),
        div("No characters match your filters.")
      ))
    }

    sel <- selected_char()
    lapply(seq_len(min(nrow(chars), 200)), function(i) {
      cn    <- chars$character_name[i]
      tags  <- get_char_tags(cn)
      is_sel <- !is.null(sel) && sel == cn
      div(
        class = paste("char-card", if (is_sel) "selected" else ""),
        onclick = sprintf("Shiny.setInputValue('select_char', '%s', {priority: 'event'})",
                          gsub("'", "\\\\'", cn)),
        div(class = "char-name", cn),
        div(style = "margin-top:4px",
          HTML(paste(sprintf('<span class="tag-pill team">%s</span>',
                             head(tags, 6)), collapse = " "))
        )
      )
    })
  })

  observeEvent(input$select_char, { selected_char(input$select_char) })

  output$char_detail <- renderUI({
    cn <- selected_char()
    if (is.null(cn) || !nzchar(cn)) {
      return(div(class = "empty-state",
        div(class = "big", "👆"),
        div("Select a character to view their full profile.")
      ))
    }
    HTML(char_detail_html(cn))
  })

  # ── TEAM BUILDER TAB ───────────────────────────────────────────────────────

  team_chars <- reactive({
    slots <- sapply(1:5, function(i) input[[paste0("team_slot_", i)]])
    slots[nzchar(slots)]
  })

  observeEvent(input$clear_team, {
    for (i in 1:5) updateSelectizeInput(session, paste0("team_slot_", i), selected = "")
  })

  output$team_analysis <- renderUI({
    team <- team_chars()
    if (length(team) == 0) {
      return(div(class = "empty-state",
        div(class = "big", "🛡"),
        div("Add characters on the left to build your team.")
      ))
    }

    # All tags across the team
    all_team_tags <- unique(unlist(lapply(team, get_char_tags)))

    # Shared tags (appear on 2+ members)
    tag_counts <- table(unlist(lapply(team, get_char_tags)))
    shared_tags <- names(tag_counts[tag_counts >= 2])

    # Team synergies — abilities that buff the team's tags
    team_ids <- chars_final |>
      filter(character_name %in% team) |>
      pull(character_id)

    team_syn <- synergies_df |>
      filter(character_id %in% team_ids,
             synergy_tag %in% all_team_tags,
             synergy_type %in% c("ally","self"))

    # Stat totals
    stat_totals <- if (length(stat_cols) > 0) {
      chars_final |>
        filter(character_name %in% team) |>
        summarise(across(all_of(stat_cols), ~ sum(.x, na.rm = TRUE)))
    } else NULL

    # Member cards
    make_member_card <- function(cn) {
      tags      <- get_char_tags(cn)
      tag_pills <- paste(sprintf('<span class="tag-pill team">%s</span>', head(tags, 6)), collapse = " ")
      sprintf(
        '<div class="char-card selected" style="cursor:default;margin-bottom:8px"><div class="char-name">%s</div><div style="margin-top:4px">%s</div></div>',
        cn, tag_pills
      )
    }
    member_html <- paste(vapply(team, make_member_card, character(1)), collapse = "")

    # Shared tag pills
    shared_html <- if (length(shared_tags) > 0) {
      paste(sprintf('<span class="tag-pill team" style="font-size:13px;padding:4px 10px">%s <span style="opacity:.5">×%d</span></span>',
                    shared_tags, tag_counts[shared_tags]), collapse = " ")
    } else "<span style='color:#7a8499'>No shared tags</span>"

    # Internal synergy list
    syn_html <- if (nrow(team_syn) > 0) {
      by_char <- split(team_syn, team_syn$character_id)
      paste(vapply(names(by_char), function(cid) {
        cn    <- chars_final$character_name[chars_final$character_id == cid][1]
        rows  <- by_char[[cid]]
        pills <- paste(sprintf('<span class="tag-pill %s">%s · %s · %s</span>',
                               rows$synergy_type, rows$ability, rows$synergy_tag,
                               toupper(substr(rows$synergy_type, 1, 1))), collapse = " ")
        sprintf('<div style="margin-bottom:8px"><span style="color:var(--yellow);font-family:Bangers,cursive;font-size:16px">%s</span><br>%s</div>',
                cn, pills)
      }, character(1)), collapse = "")
    } else "<p style='color:#7a8499'>No internal tag synergies detected.</p>"

    # Stat totals html
    make_stat_row <- function(s) {
      sprintf(
        '<div style="display:flex;justify-content:space-between;border-bottom:1px solid var(--border);padding:4px 0"><span style="color:var(--muted);font-size:13px;text-transform:uppercase;letter-spacing:.5px">%s</span><span style="font-family:Barlow Condensed,sans-serif;font-weight:700;color:var(--yellow)">%s</span></div>',
        s, formatC(stat_totals[[s]], format = "d", big.mark = ",")
      )
    }
    stat_html <- if (!is.null(stat_totals)) {
      paste(vapply(stat_cols, make_stat_row, character(1)), collapse = "")
    } else ""

    tagList(
      fluidRow(
        column(5,
          div(class = "section-head", tags$span("LINEUP")),
          HTML(member_html)
        ),
        column(7,
          div(class = "section-head", tags$span("SHARED TAGS")),
          div(style="margin-bottom:16px", HTML(shared_html)),

          div(class = "section-head", tags$span("INTERNAL SYNERGIES")),
          div(class = "synergy-summary", HTML(syn_html)),

          if (nzchar(stat_html)) tagList(
            div(class = "section-head", tags$span("COMBINED STATS")),
            div(class = "control-panel", style="border-left-color:var(--blue)", HTML(stat_html))
          )
        )
      )
    )
  })

  # ── COUNTER FINDER TAB ─────────────────────────────────────────────────────

  counter_results <- eventReactive(input$find_counters, {
    tags_to_counter <- input$enemy_tags
    if (length(tags_to_counter) == 0) return(NULL)

    syn_type_filter <- if (input$counter_type == "enemy") "enemy" else c("enemy","ally","self")

    # Find abilities that reference enemy tags as enemy synergies
    hits <- synergies_df |>
      filter(synergy_tag %in% tags_to_counter,
             synergy_type %in% syn_type_filter)

    if (nrow(hits) == 0) return(data.frame())

    # Score: count of matching synergy tags per character
    scores <- hits |>
      group_by(character_id) |>
      summarise(
        score        = n(),
        matched_tags = paste(unique(synergy_tag), collapse=", "),
        abilities    = paste(unique(ability), collapse=", "),
        .groups      = "drop"
      ) |>
      left_join(chars_final |> select(character_id, character_name), by="character_id") |>
      arrange(desc(score))

    scores
  })

  output$counter_results <- renderUI({
    if (length(input$enemy_tags) == 0) {
      return(div(class = "empty-state",
        div(class = "big", "🎯"),
        div("Select enemy tags on the left, then click FIND COUNTERS.")
      ))
    }

    res <- counter_results()
    if (is.null(res) || nrow(res) == 0) {
      return(div(class = "empty-state",
        div(class = "big", "?"),
        div("No counters found for those tags.")
      ))
    }

    make_counter_card <- function(i) {
      r         <- res[i, ]
      cn        <- if (!is.na(r$character_name)) r$character_name else r$character_id
      char_tags <- get_char_tags(cn)
      tag_pills <- HTML(paste(sprintf('<span class="tag-pill team">%s</span>', head(char_tags, 6)), collapse = " "))
      div(class = "counter-hit",
        div(class = "counter-score", r$score),
        div(class = "char-name", style = "font-size:20px", cn),
        div(style = "margin:4px 0 6px;font-size:13px;color:var(--muted)",
          "Counters via: ", tags$strong(style = "color:var(--text)", r$matched_tags),
          " · Abilities: ", tags$strong(style = "color:var(--text)", r$abilities)
        ),
        tag_pills
      )
    }
    lapply(seq_len(min(nrow(res), 30)), make_counter_card)
  })

  # ── ABILITY BROWSER TAB ────────────────────────────────────────────────────

  ability_data <- reactive({
    df <- raw_abilities_df

    if (nzchar(input$ability_search)) {
      df <- df |> filter(str_detect(str_to_lower(ability_text),
                                    str_to_lower(fixed(input$ability_search))))
    }
    if (nzchar(input$ability_char)) {
      df <- df |> filter(character_id %in%
        (chars_final |> filter(character_name == input$ability_char) |> pull(character_id)))
    }
    if (nzchar(input$ability_slot)) {
      slot_key <- names(ABILITY_LABELS)[ABILITY_LABELS == input$ability_slot]
      if (length(slot_key) > 0) df <- df |> filter(ability_slot == slot_key)
    }
    if (nzchar(input$ability_syn_tag)) {
      chars_with_syn <- synergies_df |>
        filter(synergy_tag == input$ability_syn_tag) |>
        pull(character_id) |> unique()
      df <- df |> filter(character_id %in% chars_with_syn)
    }

    # Join display name
    df |>
      left_join(chars_final |> select(character_id, character_name), by = "character_id") |>
      mutate(
        ability_slot = ABILITY_LABELS[ability_slot] %||% ability_slot,
        character_name = if_else(is.na(character_name), character_id, character_name)
      ) |>
      select(character_name, ability_slot, ability_name, ability_text) |>
      rename(Character = character_name, Type = ability_slot,
             Ability = ability_name, Description = ability_text)
  })

  output$ability_table <- renderDT({
    datatable(
      ability_data(),
      options = list(
        pageLength = 20,
        scrollX    = TRUE,
        columnDefs = list(list(width = "40%", targets = 3))
      ),
      rownames   = FALSE,
      escape     = TRUE,
      class      = "display compact"
    )
  })
  # ── NETWORK TAB ────────────────────────────────────────────────────────────

  # Build network data reactive
  network_data <- reactive({
    center <- input$net_character
    if (is.null(center) || !nzchar(center)) return(NULL)

    edge_types  <- input$net_edge_types
    min_shared  <- input$net_min_shared
    depth       <- input$net_depth

    # Get center character's ID and tags
    center_row  <- chars_final |> filter(character_name == center)
    if (nrow(center_row) == 0) return(NULL)
    center_id   <- center_row$character_id[1]
    center_tags <- get_char_tags(center)

    # Power stat for node sizing (use first numeric stat if Power absent)
    size_col <- if ("Power" %in% stat_cols) "Power" else if (length(stat_cols) > 0) stat_cols[1] else NULL
    max_size <- if (!is.null(size_col)) max(chars_final[[size_col]], na.rm=TRUE) else 1

    node_size <- function(cn) {
      if (is.null(size_col)) return(20)
      v <- chars_final[[size_col]][chars_final$character_name == cn]
      if (length(v) == 0 || is.na(v[1])) return(15)
      round(12 + 28 * (v[1] / max_size))
    }

    # ── Collect edges ──────────────────────────────────────────────────────
    edges_list <- list()

    find_edges_for <- function(focal_name, focal_id, focal_tags) {

      # --- Tag-based edges ---
      if ("tags" %in% edge_types && length(focal_tags) > 0) {
        for (cn in char_names) {
          if (cn == focal_name) next
          shared <- intersect(focal_tags, get_char_tags(cn))
          if (length(shared) >= min_shared) {
            key <- paste(sort(c(focal_name, cn)), collapse="__")
            if (!key %in% names(edges_list)) {
              edges_list[[key]] <<- data.frame(
                from        = focal_name,
                to          = cn,
                label       = paste(head(shared, 3), collapse=", "),
                color       = "#1a6bce",
                dashes      = FALSE,
                width       = min(1 + length(shared), 5),
                edge_type   = "tags",
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }

      # --- Synergy-based edges (ally / enemy) ---
      focal_syn <- synergies_df |> filter(character_id == focal_id)

      if (nrow(focal_syn) > 0) {
        for (et in intersect(c("ally","enemy"), edge_types)) {
          typed_syn <- focal_syn |> filter(synergy_type == et)
          if (nrow(typed_syn) == 0) next
          syn_tags <- unique(typed_syn$synergy_tag)

          # Find characters who HAVE those tags
          for (cn in char_names) {
            if (cn == focal_name) next
            cn_tags <- get_char_tags(cn)
            if (any(syn_tags %in% cn_tags)) {
              key <- paste(focal_name, cn, et, sep="__")
              if (!key %in% names(edges_list)) {
                matched_tags <- intersect(syn_tags, cn_tags)
                col <- if (et == "ally") "#00c853" else "#e8001d"
                edges_list[[key]] <<- data.frame(
                  from      = focal_name,
                  to        = cn,
                  label     = paste(head(matched_tags, 2), collapse=", "),
                  color     = col,
                  dashes    = et == "enemy",
                  width     = 2,
                  edge_type = et,
                  stringsAsFactors = FALSE
                )
              }
            }
          }
        }
      }
    }

    # Depth 1: edges from center
    find_edges_for(center, center_id, center_tags)

    # Depth 2: also run for each connected neighbour
    if (depth == 2 && length(edges_list) > 0) {
      neighbours <- unique(c(
        bind_rows(edges_list)$from,
        bind_rows(edges_list)$to
      ))
      neighbours <- setdiff(neighbours, center)
      for (nb in head(neighbours, 20)) {  # cap at 20 to keep graph readable
        nb_row <- chars_final |> filter(character_name == nb)
        if (nrow(nb_row) == 0) next
        find_edges_for(nb, nb_row$character_id[1], get_char_tags(nb))
      }
    }

    if (length(edges_list) == 0) return(list(nodes=data.frame(), edges=data.frame()))

    edges_df <- bind_rows(edges_list)

    # ── Build nodes ────────────────────────────────────────────────────────
    node_names <- unique(c(center, edges_df$from, edges_df$to))

    nodes_df <- data.frame(
      id    = node_names,
      label = node_names,
      value = sapply(node_names, node_size),
      color = vapply(node_names, function(cn) {
        if (cn == center) return("#ffd700")
        ctags <- get_char_tags(cn)
        if ("Hero" %in% ctags) "#1a4a8a" else if ("Villain" %in% ctags) "#6a1a2a" else "#2a3a4a"
      }, character(1)),
      font.color = "#e8eaf0",
      font.size  = 13,
      borderWidth = vapply(node_names, function(cn) if (cn == center) 3L else 1L, integer(1)),
      shadow = TRUE,
      stringsAsFactors = FALSE
    )

    list(nodes = nodes_df, edges = edges_df)
  })

  output$net_node_count <- renderText({
    nd <- network_data()
    if (is.null(nd)) return("0")
    nrow(nd$nodes)
  })

  output$net_edge_count <- renderText({
    nd <- network_data()
    if (is.null(nd)) return("0")
    nrow(nd$edges)
  })

  output$network_plot <- renderVisNetwork({
    center <- input$net_character
    if (is.null(center) || !nzchar(center)) {
      return(visNetwork(
        data.frame(id=1, label="Select a character", color="#1e3a5f",
                   font.color="#7a8499", font.size=16),
        data.frame(from=integer(0), to=integer(0))
      ) |>
        visOptions(highlightNearest=TRUE) |>
        visLayout(randomSeed=42) |>
        visPhysics(stabilization=FALSE) |>
        visInteraction(navigationButtons=FALSE) |>
        visEdges(smooth=list(type="curvedCW")) |>
        visNodes(shape="dot") |>
        visBackground(color="#0a0a0f"))
    }

    nd <- network_data()
    if (is.null(nd) || nrow(nd$nodes) == 0) return(NULL)

    nodes <- nd$nodes
    edges <- nd$edges

    visNetwork(nodes, edges, background = "#0a0a0f") |>
      visNodes(
        shape         = "dot",
        scaling       = list(min=15, max=40),
        shadow        = list(enabled=TRUE, color="rgba(0,0,0,0.6)", size=8),
        font          = list(color="#e8eaf0", size=13, face="Barlow Condensed")
      ) |>
      visEdges(
        smooth        = list(type="curvedCW", roundness=0.2),
        font          = list(color="#7a8499", size=10, align="middle"),
        color         = list(inherit=FALSE),
        arrows        = list(to=list(enabled=FALSE))
      ) |>
      visOptions(
        highlightNearest = list(enabled=TRUE, degree=1, hover=TRUE),
        nodesIdSelection = list(
          enabled   = TRUE,
          main      = "Re-center on node",
          style     = "background:#1a2233;color:#e8eaf0;border:1px solid #1e3a5f;border-radius:4px;padding:4px 8px;font-family:Barlow,sans-serif"
        )
      ) |>
      visLayout(randomSeed = 42) |>
      visPhysics(
        solver       = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant=-60, springLength=120, springConstant=0.05),
        stabilization    = list(enabled=TRUE, iterations=200)
      ) |>
      visInteraction(
        navigationButtons = TRUE,
        hover             = TRUE,
        tooltipDelay      = 100
      ) |>
      visEvents(selectNode = paste0(
        "function(params) {",
        "  var nodeId = params.nodes[0];",
        "  Shiny.setInputValue('net_clicked_node', nodeId, {priority: 'event'});",
        "}"
      ))
  })

  # Re-center network when a node is clicked
  observeEvent(input$net_clicked_node, {
    clicked <- input$net_clicked_node
    if (!is.null(clicked) && clicked %in% char_names) {
      updateSelectizeInput(session, "net_character", selected = clicked)
    }
  })


}

# =============================================================================
# LAUNCH
# =============================================================================

shinyApp(ui, server)
