library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(plotly)
library(scales)

required_cols <- c("Character.Id", "Level", "Gear.Tier", "Stars", "Red.Stars", "Power", "Player")
iso_classes <- c("Striker", "Fortifier", "Healer", "Raider", "Skirmisher")
all_required <- c(required_cols, iso_classes)
default_msf_urls <- c(
  "https://marvelstrikeforce.com/en/updates",
  "https://marvelstrikeforce.com/en/news",
  "https://marvelstrikeforce.com/en/blog",
  "https://marvelstrikeforce.com/en/characters"
)

PALETTE <- list(
  blue = "#4f8ef7",
  purple = "#9b59b6",
  gold = "#f1c40f",
  red = "#e74c3c",
  green = "#2ecc71",
  dark_bg = "#1a1a2e",
  card_bg = "#16213e",
  text = "#e0e0e0",
  muted = "#8899aa"
)

ISO_COLOURS <- c(
  striker = "#e74c3c",
  raider = "#e67e22",
  skirmisher = "#f1c40f",
  healer = "#2ecc71",
  fortifier = "#3498db"
)

MATRIX_COLOURS <- c(
  none = "#555566",
  blue = "#4f8ef7",
  purple = "#9b59b6"
)

dark_layout <- function(p, title = NULL, xlab = NULL, ylab = NULL) {
  p %>% layout(
    title = list(text = title, font = list(color = PALETTE$text, size = 14)),
    paper_bgcolor = PALETTE$dark_bg,
    plot_bgcolor = PALETTE$card_bg,
    font = list(color = PALETTE$text),
    xaxis = list(title = xlab, gridcolor = "#2a2a4a", zerolinecolor = "#2a2a4a"),
    yaxis = list(title = ylab, gridcolor = "#2a2a4a", zerolinecolor = "#2a2a4a"),
    legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = PALETTE$text)),
    margin = list(t = 40, l = 60, r = 20, b = 60)
  )
}

read_roster_csv <- function(path_or_url) {
  detect_delimiter <- function(path) {
    header_lines <- tryCatch(
      readLines(path, n = 5, warn = FALSE, encoding = "UTF-8"),
      error = function(e) character(0)
    )
    if (length(header_lines) == 0) return(",")
    first <- header_lines[1]
    candidates <- c(",", ";", "\t", "|")
    counts <- vapply(candidates, function(sep) {
      m <- gregexpr(sep, first, fixed = TRUE)[[1]]
      if (length(m) == 1 && m[1] == -1) 0L else length(m)
    }, integer(1))
    best <- candidates[which.max(counts)]
    if (length(best) == 0 || max(counts) == 0) "," else best
  }

  read_delim_with_encoding_fallback <- function(path, sep) {
    encodings <- c("UTF-8", "latin1", "windows-1252", "")
    for (enc in encodings) {
      dat <- tryCatch(
        read.table(
          path,
          header = TRUE,
          sep = sep,
          quote = "\"",
          comment.char = "",
          stringsAsFactors = FALSE,
          check.names = FALSE,
          fill = TRUE,
          fileEncoding = enc
        ),
        error = function(e) NULL
      )
      if (!is.null(dat) && ncol(dat) > 1) return(dat)
    }
    NULL
  }

  sep <- detect_delimiter(path_or_url)
  df <- read_delim_with_encoding_fallback(path_or_url, sep)
  if (is.null(df)) {
    return(list(ok = FALSE, data = NULL, error = "Could not parse file. Tried common delimiters/encodings."))
  }
  names(df) <- sub("^\ufeff", "", names(df))

  # Normalize common export headers from msf.gg/website roster exports.
  rename_map <- c(
    "Name" = "Player",
    "Character Id" = "Character.Id",
    "Character ID" = "Character.Id",
    "CharacterId" = "Character.Id",
    "Gear Tier" = "Gear.Tier",
    "GearTier" = "Gear.Tier",
    "Red Stars" = "Red.Stars",
    "RedStars" = "Red.Stars"
  )
  for (old_name in names(rename_map)) {
    if (old_name %in% names(df)) names(df)[names(df) == old_name] <- rename_map[[old_name]]
  }

  core_required <- c("Character.Id", "Level", "Gear.Tier", "Stars", "Red.Stars", "Power", "Player")
  missing_core <- setdiff(core_required, names(df))
  if (length(missing_core) > 0) {
    return(list(ok = FALSE, data = NULL, error = paste("Missing required columns:", paste(missing_core, collapse = ", "))))
  }

  # Coerce numeric analysis columns.
  numeric_cols <- c("Level", "Gear.Tier", "Stars", "Red.Stars", "Power")
  for (col in numeric_cols) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }

  # Build ISO flags from either ISO Class text or class-specific score columns.
  iso_class_text <- if ("ISO Class" %in% names(df)) tolower(trimws(df[["ISO Class"]])) else rep("", nrow(df))
  has_iso_text <- any(nzchar(iso_class_text))
  for (cls in iso_classes) {
    if (has_iso_text) {
      df[[cls]] <- iso_class_text == tolower(cls)
    } else {
      source_col <- cls
      by_value <- rep(FALSE, nrow(df))
      if (source_col %in% names(df)) {
        vals <- suppressWarnings(as.numeric(df[[source_col]]))
        by_value <- !is.na(vals) & vals > 0
      }
      df[[cls]] <- by_value
    }
  }

  list(ok = TRUE, data = df, error = NULL)
}

read_characters_context <- function(file_path) {
  if (!file.exists(file_path)) return("No local character reference file available.")
  chars <- tryCatch(read.csv(file_path, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(chars) || !"character" %in% names(chars)) return("Character reference file exists but could not be parsed.")

  chars <- chars %>% arrange(desc(ID))
  latest_names <- head(chars$character, 80)
  paste("Known character IDs and names (highest IDs are likely newer):", paste(latest_names, collapse = ", "))
}

extract_character_mentions <- function(text, character_names, max_n = 6) {
  if (!nzchar(text) || length(character_names) == 0) return(character(0))
  hits <- character_names[vapply(character_names, function(nm) grepl(nm, text, ignore.case = TRUE, fixed = TRUE), logical(1))]
  hits <- unique(hits[nzchar(hits)])
  head(hits, max_n)
}

fetch_msf_web_context <- function(urls = default_msf_urls, max_lines = 120, max_entries = 25) {
  if (!requireNamespace("httr2", quietly = TRUE) || !requireNamespace("rvest", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(
      ok = FALSE,
      text = "",
      status = "Live web context unavailable: install.packages(c('httr2','rvest','jsonlite')).",
      fetched_at = as.character(Sys.time())
    ))
  }

  character_names <- character(0)
  char_path <- file.path(getwd(), "characters.csv")
  if (file.exists(char_path)) {
    char_df <- tryCatch(read.csv(char_path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(char_df) && "character" %in% names(char_df)) {
      character_names <- unique(char_df$character[nzchar(char_df$character)])
    }
  }

  extract_text_from_json <- function(x, out = character(0)) {
    if (is.list(x)) {
      nms <- names(x)
      if (!is.null(nms)) {
        target_names <- c("headline", "title", "name", "description", "summary", "body", "content", "datePublished", "dateModified")
        keep <- x[nms %in% target_names]
        if (length(keep) > 0) {
          vals <- unlist(keep, use.names = FALSE)
          vals <- vals[is.character(vals)]
          out <- c(out, vals)
        }
      }
      for (item in x) out <- extract_text_from_json(item, out)
      return(out)
    }
    if (is.character(x) && length(x) == 1) return(c(out, x))
    out
  }

  fetch_single_url <- function(u) {
    tryCatch({
      resp <- httr2::request(u) |>
        httr2::req_user_agent("MSF-Shiny-Analyzer/1.1") |>
        httr2::req_timeout(25) |>
        httr2::req_headers(Accept = "text/html,application/json;q=0.9,*/*;q=0.8") |>
        httr2::req_perform()
      status_code <- httr2::resp_status(resp)
      body <- httr2::resp_body_string(resp)
      html <- rvest::read_html(body)

      html_nodes <- rvest::html_elements(
        html,
        "article h1, article h2, article h3, article p, main h1, main h2, main h3, main p, section h1, section h2, section h3, section p, li, time, .news-card, .blog-card"
      )
      html_txt <- rvest::html_text2(html_nodes)

      # Parse schema.org payloads often used by modern sites for article metadata.
      ld_nodes <- rvest::html_elements(html, "script[type='application/ld+json']")
      ld_txt <- rvest::html_text2(ld_nodes)
      ld_vals <- unlist(lapply(ld_txt, function(s) {
        parsed <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
        if (is.null(parsed)) return(character(0))
        extract_text_from_json(parsed, character(0))
      }), use.names = FALSE)

      # Parse Next.js/Nuxt app-state JSON where article data is often embedded.
      app_json_nodes <- rvest::html_elements(html, "script#__NEXT_DATA__, script#__NUXT_DATA__, script[type='application/json']")
      app_json_txt <- rvest::html_text2(app_json_nodes)
      app_vals <- unlist(lapply(app_json_txt, function(s) {
        parsed <- tryCatch(jsonlite::fromJSON(s, simplifyVector = FALSE), error = function(e) NULL)
        if (is.null(parsed)) return(character(0))
        extract_text_from_json(parsed, character(0))
      }), use.names = FALSE)

      txt <- c(html_txt, ld_vals, app_vals)
      txt <- txt[nzchar(trimws(txt))]
      txt <- unique(trimws(txt))
      txt <- txt[nchar(txt) > 8 & nchar(txt) < 450]
      list(ok = length(txt) > 0, text = txt, status = status_code, error = "")
    }, error = function(e) {
      list(ok = FALSE, text = character(0), status = NA_integer_, error = conditionMessage(e))
    })
  }

  all_lines <- character(0)
  touched <- character(0)
  failed <- character(0)
  for (u in urls) {
    result <- fetch_single_url(u)
    if (isTRUE(result$ok)) {
      touched <- c(touched, u)
      all_lines <- c(all_lines, result$text)
    } else {
      failed <- c(failed, paste0(u, " (", ifelse(is.na(result$status), "no-status", result$status), ": ", result$error, ")"))
    }
  }

  all_lines <- unique(trimws(all_lines))
  all_lines <- all_lines[nchar(all_lines) > 8 & nchar(all_lines) < 400]
  all_lines <- head(all_lines, max_lines)

  if (length(all_lines) == 0) {
    return(list(
      ok = FALSE,
      text = "",
      status = paste0("Live web context fetch failed for all sources. Failures: ", paste(failed, collapse = " | ")),
      fetched_at = as.character(Sys.time())
    ))
  }

  date_pattern <- paste(
    "(\\b\\d{4}-\\d{2}-\\d{2}\\b)",
    "(\\b(?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*\\s+\\d{1,2},\\s+\\d{4}\\b)",
    "(\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b)",
    sep = "|"
  )

  entries <- list()
  current <- list(date = "", title = "", detail = "")
  push_entry <- function(entry, dest) {
    if (!nzchar(entry$title) && nzchar(entry$detail)) entry$title <- entry$detail
    if (!nzchar(entry$title)) return(dest)
    dest[[length(dest) + 1]] <- entry
    dest
  }

  for (line in all_lines) {
    if (grepl(date_pattern, line, ignore.case = TRUE, perl = TRUE)) {
      entries <- push_entry(current, entries)
      current <- list(date = line, title = "", detail = "")
      next
    }
    if (!nzchar(current$title) && nchar(line) <= 120) {
      current$title <- line
      next
    }
    if (!nzchar(current$detail)) current$detail <- line
  }
  entries <- push_entry(current, entries)
  entries <- head(entries, max_entries)

  if (length(entries) == 0) {
    entries <- lapply(head(all_lines, 20), function(x) list(date = "", title = x, detail = ""))
  }

  structured_lines <- vapply(entries, function(e) {
    combined <- paste(e$title, e$detail)
    mentions <- extract_character_mentions(combined, character_names)
    mention_text <- if (length(mentions) > 0) paste(mentions, collapse = ", ") else "none detected"
    paste0(
      "- Date: ", ifelse(nzchar(e$date), e$date, "unknown"),
      " | Title: ", e$title,
      ifelse(nzchar(e$detail), paste0(" | Detail: ", e$detail), ""),
      " | CharacterMentions: ", mention_text
    )
  }, character(1))

  text_payload <- paste(
    "Structured MSF update digest (auto-fetched at", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), ").",
    "\nSources:", paste(unique(touched), collapse = ", "),
    "\nUse dates above when deciding what is current.",
    "\n\n", paste(structured_lines, collapse = "\n"),
    sep = " "
  )

  list(
    ok = TRUE,
    text = text_payload,
    status = paste0(
      "Loaded digest from ", length(unique(touched)), " source(s), ",
      length(entries), " entries. ",
      if (length(failed) > 0) paste0("Some sources failed: ", paste(head(failed, 2), collapse = " | ")) else "No source failures."
    ),
    fetched_at = as.character(Sys.time())
  )
}

fetch_msf_character_context <- function(
  listing_url = "https://marvelstrikeforce.com/en/characters",
  max_characters = 25,
  max_traits_per_char = 4
) {
  if (!requireNamespace("httr2", quietly = TRUE) || !requireNamespace("rvest", quietly = TRUE)) {
    return(list(
      ok = FALSE,
      text = "",
      status = "Character fetch unavailable: install.packages(c('httr2','rvest'))."
    ))
  }

  make_abs <- function(href) {
    if (!nzchar(href)) return("")
    if (grepl("^https?://", href)) return(href)
    paste0("https://marvelstrikeforce.com", href)
  }

  traits_pattern <- paste(
    "Health|Damage|Armor|Focus|Resistance|Speed|Hero|Villain|Global|Cosmic|City|",
    "Mutant|Bio|Skill|Tech|Mystic|Blaster|Brawler|Protector|Controller|Support",
    sep = ""
  )

  listing_resp <- tryCatch({
    httr2::request(listing_url) |>
      httr2::req_user_agent("MSF-Shiny-Analyzer/1.2") |>
      httr2::req_timeout(25) |>
      httr2::req_perform()
  }, error = function(e) NULL)

  if (is.null(listing_resp)) {
    return(list(ok = FALSE, text = "", status = "Character listing request failed."))
  }

  listing_html <- tryCatch(rvest::read_html(httr2::resp_body_string(listing_resp)), error = function(e) NULL)
  if (is.null(listing_html)) {
    return(list(ok = FALSE, text = "", status = "Character listing HTML parse failed."))
  }

  hrefs <- rvest::html_attr(rvest::html_elements(listing_html, "a[href*='/en/characters/']"), "href")
  hrefs <- unique(vapply(hrefs, make_abs, character(1)))
  hrefs <- hrefs[nzchar(hrefs)]
  hrefs <- hrefs[!grepl("/en/characters/?$", hrefs)]
  hrefs <- head(hrefs, max_characters)

  if (length(hrefs) == 0) {
    return(list(ok = FALSE, text = "", status = "No character detail links found on listing page."))
  }

  character_lines <- character(0)
  failures <- character(0)
  for (u in hrefs) {
    one <- tryCatch({
      resp <- httr2::request(u) |>
        httr2::req_user_agent("MSF-Shiny-Analyzer/1.2") |>
        httr2::req_timeout(20) |>
        httr2::req_perform()
      html <- rvest::read_html(httr2::resp_body_string(resp))
      h1 <- rvest::html_text2(rvest::html_elements(html, "h1"))
      h1 <- h1[nzchar(trimws(h1))]
      char_name <- if (length(h1) > 0) trimws(h1[1]) else sub(".*/en/characters/", "", u)

      txt <- rvest::html_text2(rvest::html_elements(html, "main p, article p, section p, li, .stat, .trait, .tag"))
      txt <- unique(trimws(txt[nzchar(trimws(txt))]))
      trait_lines <- txt[grepl(traits_pattern, txt, ignore.case = TRUE, perl = TRUE)]
      trait_lines <- head(trait_lines, max_traits_per_char)
      if (length(trait_lines) == 0) trait_lines <- "No clear stats/traits text found on detail page."
      paste0("- ", char_name, " | ", paste(trait_lines, collapse = " ; "))
    }, error = function(e) {
      failures <<- c(failures, paste0(u, " (", conditionMessage(e), ")"))
      character(0)
    })
    if (length(one) > 0) character_lines <- c(character_lines, one)
  }

  character_lines <- unique(character_lines[nzchar(character_lines)])
  if (length(character_lines) == 0) {
    return(list(
      ok = FALSE,
      text = "",
      status = paste0("Character detail fetch failed. ", if (length(failures) > 0) paste(head(failures, 2), collapse = " | ") else "")
    ))
  }

  list(
    ok = TRUE,
    text = paste(
      "Live character digest (from official character pages, fetched at", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "):",
      "\n", paste(character_lines, collapse = "\n"),
      sep = " "
    ),
    status = paste0(
      "Loaded ", length(character_lines), " character profiles from website.",
      if (length(failures) > 0) paste0(" Some profile pages failed: ", paste(head(failures, 2), collapse = " | ")) else ""
    )
  )
}

ui <- page_navbar(
  title = "ChatMSF",
  theme = bs_theme(bootswatch = "minty"),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Bangers&family=Nunito:wght@500;700;900&display=swap"),
    tags$style(HTML("
      body { font-family: 'Nunito', sans-serif; }
      .comic-panel { border: 3px solid #0f172a !important; border-radius: 12px !important; box-shadow: 6px 6px 0 #0f172a !important; background: #ffffff; }
      .comic-panel .card-header { font-family: 'Bangers', cursive; letter-spacing: 1px; font-size: 1.4rem; background: #fee440; color: #111827; }
      details.fold { border: 2px solid #111827; border-radius: 10px; background: #f8fafc; padding: 8px; }
      details.fold > summary { cursor: pointer; font-weight: 900; color: #0f172a; list-style: none; }
      details.fold > summary::-webkit-details-marker { display: none; }
      details.fold > summary::before { content: '▶ '; font-size: 0.9rem; }
      details.fold[open] > summary::before { content: '▼ '; }
      .kpi-row { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 10px; margin-bottom: 10px; }
      .kpi-card { border: 3px solid #111827; border-radius: 12px; background: linear-gradient(150deg, #ffffff 0%, #fee2e2 45%, #dbeafe 100%); padding: 8px 10px; box-shadow: 4px 4px 0 #111827; }
      .kpi-label { font-size: 0.78rem; text-transform: uppercase; font-weight: 900; color: #334155; }
      .kpi-value { font-size: 1.4rem; font-weight: 900; color: #b91c1c; }
      .btn.action-button,.btn-primary { background-color: #b91c1c !important; border-color: #7f1d1d !important; color: #ffffff !important; font-weight: 700; }
      .btn.action-button:hover,.btn-primary:hover { background-color: #991b1b !important; }
      .chat-wrap { border: 3px solid #0f172a; border-radius: 10px; box-shadow: 6px 6px 0 #0f172a; background: repeating-linear-gradient(45deg, #f8fbfc, #f8fbfc 10px, #eef7fa 10px, #eef7fa 20px); }
      .chat-output { height: 520px; overflow-y: auto; padding: 10px; }
      .chat-reply { background: #0b3c49; color: #ffffff; border: 2px solid #08252d; border-radius: 10px; padding: 10px; margin: 6px 0; margin-right: 42px; }
      .chat-input-bubble { background: #4b3f72; color: #ffffff; border: 2px solid #2d2544; border-radius: 10px; padding: 10px; margin: 6px 0; margin-left: 42px; }
      .small-note { font-size: 0.9rem; color: #5f6b6d; }
      .status-ok { color: #166534; font-weight: 700; }
      .status-warn { color: #9a3412; font-weight: 700; }
      .context-box { background: #eef6f8; border: 2px solid #0f172a; border-radius: 8px; padding: 8px; margin-top: 8px; white-space: pre-wrap; }
      .howto-box { border: 2px dashed #0f172a; border-radius: 10px; background: #fff7ed; padding: 8px; margin-bottom: 8px; }
      .starter-row { display: flex; gap: 8px; flex-wrap: wrap; padding: 8px 10px 0 10px; }
      .starter-btn { background: #111827 !important; border-color: #111827 !important; }
      @media (max-width: 1200px) { .kpi-row { grid-template-columns: repeat(2, minmax(0, 1fr)); } }
    ")),
    tags$script(src = "https://cdn.jsdelivr.net/npm/markdown-it@14.1.0/dist/markdown-it.min.js"),
    tags$script(HTML("
      let chunks = '';
      const md = markdownit();
      Shiny.addCustomMessageHandler('chat_new_reply', function(_) { chunks = ''; });
      Shiny.addCustomMessageHandler('chat_update_reply', function(chunk) {
        chunks = chunks + chunk;
        const items = document.querySelectorAll('.chat-reply');
        if (items.length > 0) items[items.length - 1].innerHTML = md.render(chunks);
        const output = document.getElementById('chat_output');
        if (output) output.scrollTop = output.scrollHeight;
      });
    "))
  ),
  nav_panel(
    "Roster Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        card(
          class = "comic-panel",
          card_header("Roster Controls"),
          card_body(
            radioButtons("source_mode", "Roster Source", choices = c("Upload CSV file" = "upload", "Load from CSV URL" = "url"), selected = "upload"),
            conditionalPanel("input.source_mode === 'upload'", fileInput("file", "Upload roster CSV", accept = ".csv")),
            conditionalPanel("input.source_mode === 'url'", textInput("csv_url", "Roster CSV URL", placeholder = "https://.../roster.csv"), actionButton("load_url", "Load URL")),
            selectizeInput("focus_player", "Player Focus", choices = NULL, selected = "All Players", options = list(placeholder = "Type a player name...")),
            tags$details(
              class = "fold",
              tags$summary("Open Filters"),
              uiOutput("filters"),
              textOutput("validationError")
            )
          )
        )
      ),
      card(
        class = "comic-panel",
        card_header("Alliance + Player Metrics"),
        card_body(
          uiOutput("roster_kpis_ui"),
          uiOutput("player_compare_kpis_ui"),
          fluidRow(
            column(8, plotlyOutput("plot_total_power", height = 320)),
            column(4, plotlyOutput("plot_iso_matrix", height = 320))
          ),
          tabsetPanel(
            tabPanel("Roster Table", DTOutput("charTable")),
            tabPanel("Gear Distribution", plotlyOutput("plot_gear_dist", height = 340)),
            tabPanel("ISO Class Mix", plotlyOutput("plot_iso_class_mix", height = 340)),
            tabPanel("Focused Player Top 30", plotlyOutput("plot_player_top30", height = 420)),
            tabPanel("Focused Player Level vs Power", plotlyOutput("plot_player_scatter", height = 380)),
            tabPanel("Character Power Leaders",
              sliderInput("n_chars", "Show top N characters", min = 10, max = 100, value = 40, step = 5),
              plotlyOutput("plot_char_avg_power", height = 420)
            ),
            tabPanel("Shared Depth",
              sliderInput("depth_gt", "Gear Tier floor", min = 10, max = 20, value = 13, step = 1),
              plotlyOutput("plot_shared_depth", height = 360)
            )
          )
        )
      )
    )
  ),
  nav_panel(
    "Chat",
    fluidRow(
      column(
        width = 4,
        card(
          class = "comic-panel",
          card_header("Chat Setup"),
          card_body(
            div(class = "howto-box",
              strong("Start Here"),
              tags$ol(
                tags$li("Load a roster in Roster Dashboard."),
                tags$li("Select your player focus."),
                tags$li("Refresh live web data and ask your question.")
              )
            ),
            selectizeInput("chat_player_focus", "Chat Player Focus", choices = NULL, selected = "All Players", options = list(placeholder = "Type a player name...")),
            actionButton("sync_chat_player", "Use Dashboard Selection"),
            textInput("model_name", "Model", value = "gpt-4o"),
            textInput("github_pat_env", "Token env var", value = "GITHUB_PAT"),
            textAreaInput("manual_updates", "Manual updates notes (optional)", rows = 4, placeholder = "Paste patch note snippets or event changes."),
            actionButton("refresh_web_data", "Refresh live MSF web data"),
            uiOutput("web_context_status_ui"),
            textInput("updates_url", "Extra context URL (optional)", placeholder = "https://..."),
            actionButton("load_updates", "Import URL Context"),
            tags$p("Use this only to inject a specific article/page you want chat to reference.", class = "small-note"),
            tags$p(textOutput("updates_status"), class = "small-note"),
            tags$p(textOutput("character_fetch_status"), class = "small-note"),
            div(class = "context-box", strong("Roster context currently used by chat"), textOutput("roster_context_status"))
          )
        )
      ),
      column(
        width = 8,
        div(
          class = "chat-wrap",
          div(id = "chat_output", class = "chat-output", div(class = "chat-reply", "Ask about your focused player roster, lineup priorities, counters, raids, or upgrade paths.")),
          div(
            class = "starter-row",
            actionButton("prompt_arena", "Arena Team", class = "starter-btn"),
            actionButton("prompt_raid", "Raid Team", class = "starter-btn"),
            actionButton("prompt_war", "War Offense", class = "starter-btn"),
            actionButton("prompt_upgrade", "Upgrade Priority", class = "starter-btn")
          ),
          div(
            class = "d-flex",
            style = "border-top:1px solid #cfd8dc;",
            textAreaInput("textarea_chat_input", label = NULL, width = "80%", height = "90px", resize = "none", placeholder = "Ask strategy questions using your focused roster..."),
            actionButton("send_text", "Send", width = "20%")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  roster_store <- reactiveVal(NULL)
  roster_error <- reactiveVal("")
  updates_store <- reactiveVal("")
  web_context_store <- reactiveVal("")
  character_web_context_store <- reactiveVal("")
  web_context_status <- reactiveVal("Live web context not loaded yet.")
  character_fetch_status <- reactiveVal("Live character data not loaded yet.")
  web_context_ok <- reactiveVal(FALSE)

  observeEvent(TRUE, {
    fetched <- fetch_msf_web_context()
    chars_fetched <- fetch_msf_character_context()
    web_context_store(fetched$text)
    character_web_context_store(chars_fetched$text)
    web_context_status(fetched$status)
    character_fetch_status(chars_fetched$status)
    web_context_ok(isTRUE(fetched$ok))
  }, once = TRUE)

  observeEvent(input$refresh_web_data, {
    fetched <- fetch_msf_web_context()
    chars_fetched <- fetch_msf_character_context()
    web_context_store(fetched$text)
    character_web_context_store(chars_fetched$text)
    web_context_status(fetched$status)
    character_fetch_status(chars_fetched$status)
    web_context_ok(isTRUE(fetched$ok))
    shiny::showNotification(
      if (isTRUE(fetched$ok) || isTRUE(chars_fetched$ok)) {
        "Live MSF context refreshed (updates and/or character pages)."
      } else {
        "Live MSF refresh failed. Check status details below."
      },
      type = if (isTRUE(fetched$ok) || isTRUE(chars_fetched$ok)) "message" else "warning",
      duration = 4
    )
  })

  observeEvent(input$file, {
    req(input$file$datapath)
    loaded <- tryCatch(read_roster_csv(input$file$datapath), error = function(e) list(ok = FALSE, error = e$message, data = NULL))
    if (isTRUE(loaded$ok)) {
      roster_store(loaded$data)
      roster_error("")
    } else {
      roster_store(NULL)
      roster_error(loaded$error)
    }
  })

  observeEvent(input$load_url, {
    req(nzchar(input$csv_url))
    loaded <- tryCatch(read_roster_csv(input$csv_url), error = function(e) list(ok = FALSE, error = e$message, data = NULL))
    if (isTRUE(loaded$ok)) {
      roster_store(loaded$data)
      roster_error("")
    } else {
      roster_store(NULL)
      roster_error(loaded$error)
    }
  })

  observeEvent(input$load_updates, {
    req(nzchar(input$updates_url))
    updates <- tryCatch(paste(readLines(input$updates_url, warn = FALSE), collapse = "\n"), error = function(e) "")
    updates_store(updates)
  })

  observeEvent(roster_store(), {
    df <- roster_store()
    req(!is.null(df), nrow(df) > 0)
    players <- sort(unique(df$Player))

    current_dash <- isolate(input$focus_player)
    current_chat <- isolate(input$chat_player_focus)
    dash_selected <- if (!is.null(current_dash) && current_dash %in% c("All Players", players)) current_dash else "All Players"
    chat_selected <- if (!is.null(current_chat) && current_chat %in% c("All Players", players)) current_chat else dash_selected

    freezeReactiveValue(input, "focus_player")
    freezeReactiveValue(input, "chat_player_focus")
    updateSelectizeInput(session, "focus_player", choices = c("All Players", players), selected = dash_selected, server = TRUE)
    updateSelectizeInput(session, "chat_player_focus", choices = c("All Players", players), selected = chat_selected, server = TRUE)
  }, ignoreInit = FALSE)

  observeEvent(input$sync_chat_player, {
    req(!is.null(input$focus_player))
    updateSelectizeInput(session, "chat_player_focus", selected = input$focus_player, server = TRUE)
  })

  output$updates_status <- renderText({
    if (nzchar(updates_store())) "Imported extra context from URL." else ""
  })
  output$character_fetch_status <- renderText({ character_fetch_status() })
  output$web_context_status_ui <- renderUI({
    if (isTRUE(web_context_ok())) {
      tags$p(class = "small-note status-ok", paste0("Success: ", web_context_status()))
    } else {
      tags$p(class = "small-note status-warn", paste0("Warning: ", web_context_status()))
    }
  })

  output$roster_context_status <- renderText({
    df <- roster_store()
    if (is.null(df) || nrow(df) == 0) return("No roster loaded. Chat is using generic + web update context only.")
    if (!is.null(input$chat_player_focus) && input$chat_player_focus != "All Players") {
      df <- df[df$Player == input$chat_player_focus, , drop = FALSE]
    }
    if (nrow(df) == 0) return("Selected chat player has no rows after current selections.")
    top <- df %>%
      arrange(desc(Power)) %>%
      head(5) %>%
      pull(Character.Id)
    paste0(
      "Chat focus: ", ifelse(is.null(input$chat_player_focus), "All Players", input$chat_player_focus), "\n",
      "Loaded ", nrow(df), " roster rows across ",
      length(unique(df$Player)), " player(s).\n",
      "Top 5 by power: ", paste(top, collapse = ", "), "\n",
      "This roster context is injected into chat prompts."
    )
  })

  output$validationError <- renderText({
    roster_error()
  })

  output$roster_kpis_ui <- renderUI({
    df <- filtered_data()
    req(!is.null(df), nrow(df) > 0)
    total_power <- sum(df$Power, na.rm = TRUE)
    avg_power <- round(mean(df$Power, na.rm = TRUE))
    avg_gear <- round(mean(df$Gear.Tier, na.rm = TRUE), 1)
    max_level <- max(df$Level, na.rm = TRUE)

    div(
      class = "kpi-row",
      div(class = "kpi-card", div(class = "kpi-label", "Characters"), div(class = "kpi-value", format(nrow(df), big.mark = ","))),
      div(class = "kpi-card", div(class = "kpi-label", "Total Power"), div(class = "kpi-value", format(total_power, big.mark = ","))),
      div(class = "kpi-card", div(class = "kpi-label", "Avg Power"), div(class = "kpi-value", format(avg_power, big.mark = ","))),
      div(class = "kpi-card", div(class = "kpi-label", "Avg Gear / Max Level"), div(class = "kpi-value", paste0(avg_gear, " / ", max_level)))
    )
  })

  output$player_compare_kpis_ui <- renderUI({
    all_df <- filtered_data()
    req(!is.null(all_df), nrow(all_df) > 0)
    req(!is.null(input$focus_player), input$focus_player != "All Players")

    p_df <- all_df %>% filter(Player == input$focus_player)
    req(nrow(p_df) > 0)

    per_player <- all_df %>%
      group_by(Player) %>%
      summarise(
        total_power = sum(Power, na.rm = TRUE),
        char_count = n(),
        avg_power = mean(Power, na.rm = TRUE),
        avg_gear = mean(Gear.Tier, na.rm = TRUE),
        avg_level = mean(Level, na.rm = TRUE),
        gt20 = sum(Gear.Tier == 20, na.rm = TRUE),
        avg_red = mean(Red.Stars, na.rm = TRUE),
        .groups = "drop"
      )

    p_stat <- per_player %>% filter(Player == input$focus_player)
    req(nrow(p_stat) == 1)
    a_stat <- per_player %>%
      summarise(
        total_power = mean(total_power, na.rm = TRUE),
        char_count = mean(char_count, na.rm = TRUE),
        avg_power = mean(avg_power, na.rm = TRUE),
        avg_gear = mean(avg_gear, na.rm = TRUE),
        avg_level = mean(avg_level, na.rm = TRUE),
        gt20 = mean(gt20, na.rm = TRUE),
        avg_red = mean(avg_red, na.rm = TRUE)
      )

    fmt_delta <- function(x, digits = 1) {
      if (is.na(x)) return("n/a")
      paste0(ifelse(x >= 0, "+", ""), format(round(x, digits), big.mark = ",", nsmall = digits))
    }
    card <- function(label, p_val, a_val, digits = 1, big = FALSE) {
      delta <- p_val - a_val
      delta_text <- if (big) fmt_delta(delta, 0) else fmt_delta(delta, digits)
      val_text <- if (big) format(round(p_val), big.mark = ",") else format(round(p_val, digits), big.mark = ",", nsmall = digits)
      div(
        class = "kpi-card",
        div(class = "kpi-label", paste0(label, " vs alliance avg")),
        div(class = "kpi-value", val_text),
        div(class = "small-note", paste0("Delta: ", delta_text))
      )
    }

    div(
      class = "kpi-row",
      card("Total Power", p_stat$total_power[1], a_stat$total_power[1], big = TRUE),
      card("Characters", p_stat$char_count[1], a_stat$char_count[1], big = TRUE),
      card("Avg Power", p_stat$avg_power[1], a_stat$avg_power[1], big = TRUE),
      card("Avg Gear", p_stat$avg_gear[1], a_stat$avg_gear[1], digits = 2),
      card("Avg Level", p_stat$avg_level[1], a_stat$avg_level[1], digits = 1),
      card("GT20 Count", p_stat$gt20[1], a_stat$gt20[1], digits = 1),
      card("Avg Red Stars", p_stat$avg_red[1], a_stat$avg_red[1], digits = 2)
    )
  })

  output$filters <- renderUI({
    df <- roster_store()
    if (is.null(df)) return(tags$p("Load a roster to enable filters.", class = "small-note"))

    safeSlider <- function(inputId, label, var, step = NULL) {
      if (!var %in% names(df) || all(is.na(df[[var]]))) return(NULL)
      sliderInput(
        inputId, label,
        min = min(df[[var]], na.rm = TRUE),
        max = max(df[[var]], na.rm = TRUE),
        value = range(df[[var]], na.rm = TRUE),
        step = step
      )
    }

    tagList(
      selectInput("character", "Character", choices = c("All", sort(unique(df$Character.Id))), selected = "All"),
      safeSlider("gear", "Gear Tier", "Gear.Tier", step = 1),
      if ("Level" %in% names(df)) {
        min_lvl <- min(df$Level, na.rm = TRUE)
        max_lvl <- max(df$Level, na.rm = TRUE)
        tagList(
          numericInput("level_min", "Min Level", value = min_lvl, min = min_lvl, max = max_lvl, step = 1),
          numericInput("level_max", "Max Level", value = max_lvl, min = min_lvl, max = max_lvl, step = 1)
        )
      },
      safeSlider("stars", "Stars", "Stars", step = 1),
      safeSlider("redstars", "Red Stars & Diamonds", "Red.Stars", step = 1),
      checkboxGroupInput("iso", "ISO-8 Class", choices = iso_classes)
    )
  })

  filtered_data <- reactive({
    df <- roster_store()
    req(!is.null(df), nrow(df) > 0)

    if (is.null(input$gear) || is.null(input$stars) || is.null(input$redstars) ||
      is.null(input$level_min) || is.null(input$level_max)) {
      return(df)
    }

    if (!is.null(input$character) && input$character != "All") df <- df[df$Character.Id == input$character, ]

    df <- df %>%
      filter(
        Level >= input$level_min,
        Level <= input$level_max,
        Gear.Tier >= input$gear[1],
        Gear.Tier <= input$gear[2],
        Stars >= input$stars[1],
        Stars <= input$stars[2],
        Red.Stars >= input$redstars[1],
        Red.Stars <= input$redstars[2]
      )

    if (!is.null(input$iso) && length(input$iso) > 0) {
      df <- df[rowSums(df[, input$iso, drop = FALSE]) > 0, ]
    }
    df
  })

  output$charTable <- renderDT({
    df <- filtered_data()
    if (!is.null(input$focus_player) && input$focus_player != "All Players") {
      df <- df[df$Player == input$focus_player, , drop = FALSE]
    }
    req(nrow(df) > 0, all(required_cols %in% names(df)))
    datatable(df, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$plot_total_power <- renderPlotly({
    d <- filtered_data() %>%
      group_by(Player) %>%
      summarise(total_power = sum(Power, na.rm = TRUE), .groups = "drop") %>%
      arrange(total_power)
    req(nrow(d) > 0)
    p <- plot_ly(
      d,
      x = ~total_power,
      y = ~reorder(Player, total_power),
      type = "bar",
      orientation = "h",
      marker = list(color = PALETTE$blue),
      hovertemplate = "<b>%{y}</b><br>Power: %{x:,.0f}<extra></extra>"
    )
    dark_layout(p, xlab = "Total Collection Power", ylab = "")
  })

  output$plot_iso_matrix <- renderPlotly({
    d0 <- filtered_data()
    matrix_col <- if ("ISO Matrix" %in% names(d0)) d0$`ISO Matrix` else rep("none", nrow(d0))
    d <- tibble::tibble(`ISO Matrix` = tolower(ifelse(is.na(matrix_col) | matrix_col == "", "none", matrix_col))) %>%
      count(`ISO Matrix`)
    req(nrow(d) > 0)
    cols <- unname(MATRIX_COLOURS[d$`ISO Matrix`])
    cols[is.na(cols)] <- PALETTE$muted
    p <- plot_ly(
      d,
      labels = ~`ISO Matrix`,
      values = ~n,
      type = "pie",
      hole = 0.55,
      marker = list(colors = cols),
      textinfo = "label+percent",
      hovertemplate = "%{label}: %{value} chars<extra></extra>"
    )
    dark_layout(p)
  })

  output$plot_gear_dist <- renderPlotly({
    d <- filtered_data() %>%
      mutate(gear_bucket = case_when(
        Gear.Tier <= 5 ~ "GT 1-5",
        Gear.Tier <= 10 ~ "GT 6-10",
        Gear.Tier <= 14 ~ "GT 11-14",
        Gear.Tier <= 17 ~ "GT 15-17",
        Gear.Tier <= 19 ~ "GT 18-19",
        Gear.Tier == 20 ~ "GT 20",
        TRUE ~ "Unknown"
      )) %>%
      filter(!is.na(gear_bucket)) %>%
      count(Player, gear_bucket) %>%
      group_by(Player) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
    req(nrow(d) > 0)
    p <- plot_ly()
    buckets <- unique(d$gear_bucket)
    cols <- colorRampPalette(c("#2c3e50", "#3498db", "#9b59b6", "#e74c3c", "#f1c40f"))(length(buckets))
    for (i in seq_along(buckets)) {
      b <- buckets[i]
      dd <- d[d$gear_bucket == b, , drop = FALSE]
      p <- add_trace(
        p, data = dd, x = ~Player, y = ~pct, type = "bar", name = b,
        marker = list(color = cols[i]), hovertemplate = paste0(b, ": %{y:.0%}<extra></extra>")
      )
    }
    p <- layout(p, barmode = "stack", yaxis = list(tickformat = ".0%"))
    dark_layout(p, ylab = "Proportion")
  })

  output$plot_iso_class_mix <- renderPlotly({
    d0 <- filtered_data()
    iso <- if ("ISO Class" %in% names(d0)) tolower(ifelse(is.na(d0$`ISO Class`) | d0$`ISO Class` == "", "none", d0$`ISO Class`)) else rep("none", nrow(d0))
    d <- tibble::tibble(Player = d0$Player, `ISO Class` = iso) %>%
      filter(`ISO Class` != "none") %>%
      count(Player, `ISO Class`)
    req(nrow(d) > 0)
    p <- plot_ly()
    for (cls in names(ISO_COLOURS)) {
      dd <- d[d$`ISO Class` == cls, , drop = FALSE]
      p <- add_trace(
        p, data = dd, x = ~Player, y = ~n, type = "bar",
        name = tools::toTitleCase(cls), marker = list(color = ISO_COLOURS[[cls]]),
        hovertemplate = paste0(tools::toTitleCase(cls), ": %{y}<extra></extra>")
      )
    }
    p <- layout(p, barmode = "stack")
    dark_layout(p, ylab = "# Characters")
  })

  focused_player_data <- reactive({
    df <- filtered_data()
    req(!is.null(df), nrow(df) > 0)
    if (!is.null(input$focus_player) && input$focus_player != "All Players") {
      return(df[df$Player == input$focus_player, , drop = FALSE])
    }
    top_player <- df %>%
      group_by(Player) %>%
      summarise(tp = sum(Power, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(tp)) %>%
      slice_head(n = 1) %>%
      pull(Player)
    df[df$Player == top_player, , drop = FALSE]
  })

  output$plot_player_top30 <- renderPlotly({
    d <- focused_player_data() %>%
      arrange(desc(Power)) %>%
      slice_head(n = 30)
    req(nrow(d) > 0)
    p <- plot_ly(
      d, x = ~Power, y = ~reorder(Character.Id, Power), type = "bar", orientation = "h",
      marker = list(color = PALETTE$purple),
      customdata = ~paste0("GT: ", Gear.Tier, " | Stars: ", Stars, " | Red: ", Red.Stars, " | Player: ", Player),
      hovertemplate = "<b>%{y}</b><br>Power: %{x:,.0f}<br>%{customdata}<extra></extra>"
    )
    dark_layout(p, xlab = "Power", ylab = "")
  })

  output$plot_player_scatter <- renderPlotly({
    d <- focused_player_data() %>% filter(!is.na(Level), !is.na(Power), !is.na(Gear.Tier))
    req(nrow(d) > 0)
    p <- plot_ly(
      d, x = ~Level, y = ~Power, type = "scatter", mode = "markers",
      marker = list(size = ~pmax(6, Gear.Tier * 1.2), color = ~Gear.Tier, colorscale = "Viridis", showscale = TRUE),
      text = ~Character.Id,
      hovertemplate = "<b>%{text}</b><br>Level: %{x}<br>Power: %{y:,.0f}<br>GT: %{marker.color}<extra></extra>"
    )
    dark_layout(p, xlab = "Level", ylab = "Power")
  })

  output$plot_char_avg_power <- renderPlotly({
    d <- filtered_data() %>%
      group_by(Character.Id) %>%
      summarise(avg_power = mean(Power, na.rm = TRUE), n_players = n_distinct(Player), avg_gt = mean(Gear.Tier, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg_power)) %>%
      slice_head(n = input$n_chars)
    req(nrow(d) > 0)
    p <- plot_ly(
      d, x = ~avg_power, y = ~reorder(Character.Id, avg_power), type = "bar", orientation = "h",
      marker = list(color = ~avg_gt, colorscale = "Viridis", showscale = TRUE),
      customdata = ~paste0("Players: ", n_players, " | Avg GT: ", round(avg_gt, 1)),
      hovertemplate = "<b>%{y}</b><br>Avg Power: %{x:,.0f}<br>%{customdata}<extra></extra>"
    )
    dark_layout(p, xlab = "Average Power Across Selection", ylab = "")
  })

  output$plot_shared_depth <- renderPlotly({
    thresh <- input$depth_gt
    d <- filtered_data() %>%
      filter(Gear.Tier >= thresh) %>%
      group_by(Character.Id) %>%
      summarise(n_players = n_distinct(Player), .groups = "drop") %>%
      arrange(desc(n_players)) %>%
      slice_head(n = 30)
    req(nrow(d) > 0)
    p <- plot_ly(
      d, x = ~n_players, y = ~reorder(Character.Id, n_players), type = "bar", orientation = "h",
      marker = list(color = PALETTE$green),
      customdata = rep(thresh, nrow(d)),
      hovertemplate = "<b>%{y}</b><br>Players at GT>=%{customdata}: %{x}<extra></extra>"
    )
    dark_layout(p, xlab = "Number of Players", ylab = "")
  })

  output$isoPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    iso_counts <- colSums(df[, iso_classes, drop = FALSE])
    iso_df <- data.frame(ISO = names(iso_counts), Count = iso_counts)
    p <- ggplot(iso_df, aes(x = reorder(ISO, Count), y = Count)) +
      geom_col(aes(fill = ISO), color = "#0f172a", linewidth = 0.6, show.legend = FALSE) +
      geom_text(aes(label = scales::comma(Count)), hjust = -0.1, size = 4, fontface = "bold", color = "#0f172a") +
      coord_flip() +
      scale_fill_manual(values = c(
        "Striker" = "#ef4444",
        "Fortifier" = "#f59e0b",
        "Healer" = "#22c55e",
        "Raider" = "#3b82f6",
        "Skirmisher" = "#8b5cf6"
      )) +
      expand_limits(y = max(iso_df$Count, na.rm = TRUE) * 1.2) +
      labs(title = "ISO-8 Class Distribution", x = NULL, y = "Count") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#fff7ed", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "#fecaca"),
        plot.title = element_text(face = "bold", color = "#7f1d1d")
      )
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$summaryText <- renderPrint({
    df <- filtered_data()
    req(nrow(df) > 0)
    total_power <- sum(df$Power, na.rm = TRUE)
    avg_power <- round(mean(df$Power, na.rm = TRUE), 1)
    cat("Squad Power Summary\n")
    cat("-------------------\n")
    cat("Total Characters:", nrow(df), "\n")
    cat("Total Squad Power:", format(total_power, big.mark = ","), "\n")
    cat("Average Power per Character:", format(avg_power, big.mark = ","), "\n")
  })

  output$gearPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = as.factor(Gear.Tier))) +
      geom_bar(aes(fill = after_stat(count)), color = "#111827", linewidth = 0.4) +
      scale_fill_gradient(low = "#fde68a", high = "#f97316", guide = "none") +
      labs(title = "Gear Tier Distribution", x = "Gear Tier", y = "Count") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#eff6ff", color = NA),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#1e3a8a")
      )
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$powerPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = Power)) +
      geom_histogram(binwidth = 1000, fill = "#06b6d4", color = "#0f172a", linewidth = 0.2) +
      geom_density(aes(y = after_stat(count) * 1000), color = "#7c3aed", linewidth = 1.2) +
      labs(title = "Power Distribution", x = "Power", y = "Characters") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#ecfeff", color = NA),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#155e75")
      )
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$starsPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    p <- ggplot(df, aes(x = as.factor(Stars), fill = as.factor(Stars))) +
      geom_bar(color = "#111827", linewidth = 0.35, show.legend = FALSE) +
      scale_fill_manual(values = c(
        "1" = "#93c5fd", "2" = "#60a5fa", "3" = "#3b82f6", "4" = "#2563eb",
        "5" = "#facc15", "6" = "#f59e0b", "7" = "#ef4444"
      )) +
      labs(title = "Stars Distribution", x = "Stars", y = "Count") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#fff7ed", color = NA),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#9a3412")
      )
    ggplotly(p, tooltip = c("x", "y"))
  })

  output$comparisonPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    comparison <- df %>%
      group_by(Player) %>%
      summarise(TotalPower = sum(Power, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalPower))
    p <- ggplot(comparison, aes(x = reorder(Player, TotalPower), y = TotalPower)) +
      geom_col(aes(fill = TotalPower), color = "#111827", linewidth = 0.4, show.legend = FALSE) +
      coord_flip() +
      scale_fill_gradient(low = "#93c5fd", high = "#1d4ed8") +
      scale_y_continuous(labels = comma) +
      labs(title = "Total Squad Power by Player", x = "Player", y = "Total Power") +
      theme_minimal(base_size = 13) +
      theme(
        plot.background = element_rect(fill = "#eff6ff", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color = "#1e3a8a")
      )
    ggplotly(p, tooltip = c("x", "y"))
  })

  build_roster_context <- reactive({
    df <- roster_store()
    if (is.null(df) || nrow(df) == 0) return("No roster loaded.")
    if (!is.null(input$chat_player_focus) && input$chat_player_focus != "All Players") {
      df <- df[df$Player == input$chat_player_focus, , drop = FALSE]
    }
    if (nrow(df) == 0) return("No rows available for selected chat player focus.")
    top_chars <- df %>%
      arrange(desc(Power)) %>%
      select(Character.Id, Power, Gear.Tier, Stars, Red.Stars, Player) %>%
      head(20)
    paste(
      "Roster rows:", nrow(df),
      "\nPlayers:", paste(sort(unique(df$Player)), collapse = ", "),
      "\nTop characters by power:\n",
      paste(
        apply(top_chars, 1, function(row) {
          paste0(row["Character.Id"], " | Pwr ", row["Power"], " | G", row["Gear.Tier"], " | ",
            row["Stars"], "Y/", row["Red.Stars"], "R | ", row["Player"])
        }),
        collapse = "\n"
      )
    )
  })

  chat_state <- reactiveVal(NULL)

  observeEvent(
    list(
      roster_store(), input$model_name, input$manual_updates, updates_store(),
      web_context_store(), character_web_context_store(), input$chat_player_focus
    ),
    {
      chat_state(NULL)
    }
  )

  observeEvent(input$prompt_arena, {
    updateTextAreaInput(
      session, "textarea_chat_input",
      value = "Build my best Arena offense and defense teams from my current roster. Include 2 alternates and explain target order."
    )
  })
  observeEvent(input$prompt_raid, {
    updateTextAreaInput(
      session, "textarea_chat_input",
      value = "Using my loaded roster and latest updates, suggest my best raid teams by section and call out weak links to replace first."
    )
  })
  observeEvent(input$prompt_war, {
    updateTextAreaInput(
      session, "textarea_chat_input",
      value = "Give me 5 high-value War offense attacks from my roster, including matchup guidance and what to avoid."
    )
  })
  observeEvent(input$prompt_upgrade, {
    updateTextAreaInput(
      session, "textarea_chat_input",
      value = "Rank my top 15 upgrade priorities for the next 2 weeks based on current meta relevance and my roster power distribution."
    )
  })

  observeEvent(input$send_text, {
    req(nzchar(trimws(input$textarea_chat_input)))
    if (!requireNamespace("ellmer", quietly = TRUE) || !requireNamespace("coro", quietly = TRUE)) {
      insertUI(
        "#chat_output",
        where = "beforeEnd",
        ui = div(class = "chat-reply", "Required packages missing: install.packages(c('ellmer','coro'))."),
        immediate = TRUE
      )
      return()
    }

    pat <- Sys.getenv(input$github_pat_env)
    if (!nzchar(pat)) {
      insertUI(
        "#chat_output",
        where = "beforeEnd",
        ui = div(class = "chat-reply", paste0("Missing token in env var: ", input$github_pat_env)),
        immediate = TRUE
      )
      return()
    }

    char_context <- read_characters_context(file.path(getwd(), "characters.csv"))
    updates_context <- paste(c(input$manual_updates, updates_store(), web_context_store(), character_web_context_store()), collapse = "\n")

    if (is.null(chat_state())) {
      system_prompt <- paste(
        "You are an expert Marvel Strike Force roster analyst and strategy assistant.",
        "Use current update context when available. Prioritize entries with concrete dates and explicitly mention dates in recommendations.",
        "If context is missing, be explicit about uncertainty.",
        "Treat live character digest entries as highest priority for character-specific traits/stats when present.",
        "\n\nCharacter reference:\n", char_context,
        "\n\nRoster context:\n", build_roster_context(),
        "\n\nRecent updates context:\n", updates_context
      )
      chat_state(
        ellmer::chat_github(
          system_prompt = system_prompt,
          base_url = "https://models.inference.ai.azure.com/",
          api_key = pat,
          model = input$model_name,
          echo = FALSE
        )
      )
    }

    user_text <- input$textarea_chat_input
    updateTextAreaInput(session, "textarea_chat_input", value = "")

    insertUI(
      "#chat_output",
      where = "beforeEnd",
      ui = div(class = "chat-input-bubble", user_text),
      immediate = TRUE
    )
    insertUI(
      "#chat_output",
      where = "beforeEnd",
      ui = div(class = "chat-reply", ""),
      immediate = TRUE
    )

    stream <- chat_state()$stream(user_text)
    new_msg <- TRUE
    coro::loop(for (chunk in stream) {
      if (new_msg) session$sendCustomMessage("chat_new_reply", 1)
      session$sendCustomMessage("chat_update_reply", chunk)
      new_msg <- FALSE
    })
  })
}

shinyApp(ui, server)
