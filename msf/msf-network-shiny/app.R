library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(stringr)
library(plotly)
library(igraph)

source_colors <- list(
  team = "#63d4ff",
  synergy = "#ff8f69",
  inferred = "#b89fff"
)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

pretty_name <- function(id) {
  out <- gsub("_", " ", id)
  out <- gsub("([a-z])([A-Z])", "\\1 \\2", out)
  out <- gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", out)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}

hash_color <- function(text) {
  ints <- utf8ToInt(text)
  hue <- sum(ints * seq_along(ints)) %% 360
  grDevices::hcl(h = hue, c = 70, l = 65)
}

alpha_color <- function(color, alpha = 1) {
  rgb <- grDevices::col2rgb(color)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1], rgb[2], rgb[3], alpha)
}

pairs_from_members <- function(chars, type, group) {
  chars <- unique(chars)
  if (length(chars) < 2) {
    return(tibble(source = character(), target = character(), type = character(), group = character()))
  }
  cmb <- utils::combn(chars, 2)
  tibble(
    source = cmb[1, ],
    target = cmb[2, ],
    type = type,
    group = group
  )
}

build_network <- function(characters, team_memberships, synergy_pairs) {
  valid_ids <- unique(characters$character)

  trait_cols <- setdiff(names(characters), c("ID", "character", "real"))

  nodes <- characters %>%
    transmute(
      id = character,
      name = pretty_name(character),
      real = trimws(coalesce(real, "")),
      traits = lapply(seq_along(character), function(i) {
        trait_cols[vapply(trait_cols, function(tc) nzchar(trimws(coalesce(characters[[tc]][i], ""))), logical(1))]
      })
    )

  clean_memberships <- team_memberships %>%
    transmute(
      character = trimws(character),
      group = trimws(group),
      type = trimws(type)
    ) %>%
    filter(character %in% valid_ids, nzchar(group), nzchar(type))

  group_map <- split(clean_memberships$character, clean_memberships$group)
  team_edges <- bind_rows(lapply(names(group_map), function(g) {
    pairs_from_members(group_map[[g]], type = "team", group = g)
  }))

  inferred_prefixes <- c("Aim", "Hydra", "Kree", "Hand", "Shield", "Merc", "Ravager")
  inferred_groups <- lapply(inferred_prefixes, function(prefix) {
    members <- nodes$id[str_detect(nodes$id, paste0("^", prefix, "_"))]
    list(group = paste(prefix, "Minions"), members = members)
  })
  inferred_groups <- Filter(function(x) length(x$members) > 1, inferred_groups)

  inferred_edges <- bind_rows(lapply(inferred_groups, function(info) {
    pairs_from_members(info$members, type = "inferred", group = info$group)
  }))

  synergy_edges <- synergy_pairs %>%
    transmute(
      source = trimws(source),
      target = trimws(target),
      group = trimws(group),
      type = "synergy"
    ) %>%
    filter(source %in% valid_ids, target %in% valid_ids, source != target)

  all_edges <- bind_rows(team_edges, inferred_edges, synergy_edges) %>%
    mutate(
      a = pmin(source, target),
      b = pmax(source, target)
    ) %>%
    group_by(a, b) %>%
    summarise(
      types = list(unique(type)),
      groups = list(unique(group)),
      .groups = "drop"
    ) %>%
    rename(source = a, target = b)

  degree_tbl <- table(c(all_edges$source, all_edges$target))
  degree_df <- tibble(id = names(degree_tbl), degree = as.integer(degree_tbl))

  teams_map <- split(clean_memberships$group, clean_memberships$character)
  nodes <- nodes %>%
    rowwise() %>%
    mutate(teams = list(sort(unique(teams_map[[id]] %||% character())))) %>%
    ungroup() %>%
    left_join(degree_df, by = "id") %>%
    mutate(
      degree = coalesce(degree, 0L),
      color = vapply(teams, function(tg) if (length(tg)) hash_color(tg[[1]]) else "#8ca0b8", character(1))
    )

  graph_obj <- igraph::graph_from_data_frame(
    all_edges %>% select(source, target),
    directed = FALSE,
    vertices = nodes %>% select(name = id)
  )

  layout <- igraph::layout_with_fr(graph_obj, dim = 3, niter = 1200)
  coords <- tibble(
    id = igraph::V(graph_obj)$name,
    x = layout[, 1],
    y = layout[, 2],
    z = layout[, 3]
  )

  spread <- max(abs(c(coords$x, coords$y, coords$z)), na.rm = TRUE)
  if (is.finite(spread) && spread > 0) {
    coords <- mutate(coords, x = x / spread, y = y / spread, z = z / spread)
  }

  list(nodes = nodes, edges = all_edges, coords = coords)
}

edge_primary_type <- function(types) {
  if ("synergy" %in% types) return("synergy")
  if ("team" %in% types) return("team")
  if ("inferred" %in% types) return("inferred")
  "team"
}

build_segments <- function(edge_df, coord_df) {
  if (nrow(edge_df) == 0) return(tibble(x = numeric(), y = numeric(), z = numeric()))
  loc <- setNames(split(coord_df, coord_df$id), coord_df$id)
  rows <- vector("list", nrow(edge_df))
  for (i in seq_len(nrow(edge_df))) {
    s <- edge_df$source[[i]]
    t <- edge_df$target[[i]]
    if (is.null(loc[[s]]) || is.null(loc[[t]])) next
    rows[[i]] <- tibble(
      x = c(loc[[s]]$x, loc[[t]]$x, NA_real_),
      y = c(loc[[s]]$y, loc[[t]]$y, NA_real_),
      z = c(loc[[s]]$z, loc[[t]]$z, NA_real_)
    )
  }
  bind_rows(rows)
}

build_initial_network <- function(characters) {
  nodes <- characters %>%
    transmute(
      id = character,
      name = pretty_name(character),
      real = trimws(coalesce(real, "")),
      traits = vector("list", dplyr::n()),
      teams = vector("list", dplyr::n()),
      degree = 0L,
      color = "#8ca0b8"
    )

  n <- nrow(nodes)
  if (n == 0) {
    coords <- tibble(id = character(), x = numeric(), y = numeric(), z = numeric())
  } else if (n == 1) {
    coords <- tibble(id = nodes$id, x = 0, y = 0, z = 0)
  } else {
    idx <- seq_len(n)
    phi <- pi * (3 - sqrt(5))
    y <- 1 - (idx - 1) * (2 / (n - 1))
    r <- sqrt(pmax(0, 1 - y * y))
    theta <- phi * (idx - 1)
    coords <- tibble(
      id = nodes$id,
      x = cos(theta) * r,
      y = y,
      z = sin(theta) * r
    )
  }

  edges <- tibble(
    source = character(),
    target = character(),
    types = list(),
    groups = list()
  )

  list(nodes = nodes, edges = edges, coords = coords)
}

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  tags$head(
    tags$style(HTML(
      ":root{--bg:#070d15;--panel:#0f1f2fcc;--text:#f4f2ea;--muted:#9fb3c8;}
      html,body{background:radial-gradient(circle at 20% 20%,#12243a 0%,#070d15 48%,#050a12 100%);font-family:'Helvetica Neue',Arial,sans-serif;}
      .title-bar{display:flex;justify-content:space-between;align-items:flex-end;padding:.9rem 1rem .4rem;}
      .kicker{margin:0;font-size:.72rem;letter-spacing:.18em;color:#fdce5c;text-transform:uppercase;}
      h1{margin:0;font-size:2rem;letter-spacing:.03em;}
      .stats{color:var(--muted);font-size:.9rem;}
      .layout-grid{display:grid;grid-template-columns:280px minmax(320px,1fr) 300px;gap:.8rem;height:calc(100vh - 78px);padding:0 1rem 1rem;}
      .panel{background:linear-gradient(170deg,#162a3eb5,#0b1321cf);border:1px solid #3b516922;border-radius:14px;padding:.9rem;overflow-y:auto;}
      .caption{margin-top:.8rem;color:var(--muted);font-size:.78rem;line-height:1.4;}
      #network_plot{height:100% !important;}
      .legend-row{font-size:.85rem;color:var(--muted);margin-top:.6rem;}
      .dot{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:.4rem;}
      .detail-title{font-size:1.3rem;margin:0 0 .5rem 0;}
      .detail-muted{color:var(--muted);}
      @media (max-width:1100px){.layout-grid{grid-template-columns:1fr;grid-template-rows:auto minmax(420px,60vh) auto;height:auto;}}")
  )),
  div(
    class = "title-bar",
    div(
      tags$p(class = "kicker", "Marvel Strike Force"),
      tags$h1("Character Synergy Atlas (Shiny)")
    ),
    div(class = "stats", textOutput("stats"))
  ),
  div(
    class = "layout-grid",
    div(
      class = "panel",
      tags$label("Find Character"),
      selectizeInput(
        "search",
        NULL,
        choices = NULL,
        selected = NULL,
        options = list(
          placeholder = "Type to search characters...",
          maxOptions = 25,
          closeAfterSelect = TRUE
        )
      ),
      actionButton("find_node", "Find / Focus"),
      tags$hr(),
      sliderInput("edge_opacity", "Link Opacity", min = 0.05, max = 1, value = 0.42, step = 0.01),
      sliderInput("node_size", "Node Size", min = 3, max = 14, value = 7),
      checkboxInput("show_team", "Team Links", TRUE),
      checkboxInput("show_synergy", "Synergy Pairs", TRUE),
      checkboxInput("show_inferred", "Inferred Minion Links", TRUE),
      actionButton("clear_select", "Clear Selection"),
      tags$p(class = "caption", "Click a node to inspect its teams and direct links. Rotate/zoom the network with mouse or trackpad.")
    ),
    div(class = "panel", plotlyOutput("network_plot", height = "100%")),
    div(
      class = "panel",
      uiOutput("detail_panel")
    )
  )
)

server <- function(input, output, session) {
  chars_path <- "../characters.csv"
  teams_path <- "data/team_memberships.csv"
  synergies_path <- "data/synergy_pairs.csv"

  characters <- readr::read_csv(chars_path, show_col_types = FALSE)
  net_data <- reactiveVal(build_initial_network(characters))

  selected_node <- reactiveVal(NULL)
  focus_nonce <- reactiveVal(0L)

  resolve_character_id <- function(query, nodes_df) {
    q <- trimws(as.character(query %||% ""))
    if (!nzchar(q)) return(NULL)

    ids <- as.character(nodes_df$id)
    names <- as.character(nodes_df$name)
    labels <- paste0(names, " (", ids, ")")

    if (q %in% ids) return(q)
    if (q %in% labels) {
      m <- regmatches(q, regexec(".*\\(([^()]+)\\)$", q))[[1]]
      if (length(m) >= 2 && nzchar(m[[2]]) && m[[2]] %in% ids) return(m[[2]])
    }

    exact_name <- which(tolower(names) == tolower(q))
    if (length(exact_name) > 0) return(ids[[exact_name[[1]]]])

    partial <- which(grepl(q, ids, ignore.case = TRUE, fixed = TRUE) |
      grepl(q, names, ignore.case = TRUE, fixed = TRUE))
    if (length(partial) > 0) return(ids[[partial[[1]]]])

    NULL
  }

  observe({
    net <- net_data()
    opts <- as.list(setNames(net$nodes$id, paste0(net$nodes$name, " (", net$nodes$id, ")")))
    updateSelectizeInput(
      session,
      "search",
      choices = opts,
      selected = character(0),
      server = TRUE
    )
  })

  observeEvent(TRUE, {
    withProgress(message = "Loading MSF Network", detail = "Reading team data...", value = 0, {
      incProgress(0.2)
      team_memberships <- readr::read_csv(teams_path, show_col_types = FALSE)
      synergy_pairs <- readr::read_csv(synergies_path, show_col_types = FALSE)

      incProgress(0.35, detail = "Building character links...")
      full_net <- build_network(characters, team_memberships, synergy_pairs)

      incProgress(0.45, detail = "Finalizing 3D layout...")
      net_data(full_net)
    })
  }, once = TRUE)

  filtered_edges <- reactive({
    net <- net_data()
    active <- c(
      if (isTRUE(input$show_team)) "team",
      if (isTRUE(input$show_synergy)) "synergy",
      if (isTRUE(input$show_inferred)) "inferred"
    )
    if (!length(active)) return(net$edges[0, , drop = FALSE])
    keep <- vapply(net$edges$types, function(tp) any(tp %in% active), logical(1))
    net$edges[keep, ]
  })

  neighbors <- reactive({
    sel_raw <- selected_node()
    sel <- if (length(sel_raw) == 1 && nzchar(sel_raw)) sel_raw else NULL
    if (is.null(sel) || !nzchar(sel)) return(character())
    e <- filtered_edges()
    linked <- e %>% filter(source == sel | target == sel)
    unique(c(sel, ifelse(linked$source == sel, linked$target, linked$source)))
  })

  visible_node_ids <- reactive({
    net <- net_data()
    sel_raw <- selected_node()
    sel <- if (length(sel_raw) == 1 && nzchar(sel_raw)) sel_raw else NULL
    if (is.null(sel)) return(net$nodes$id)

    e <- filtered_edges()
    if (nrow(e) == 0) return(sel)

    g <- igraph::graph_from_data_frame(e %>% select(source, target), directed = FALSE)
    vnames <- igraph::V(g)$name
    if (!(sel %in% vnames)) return(sel)

    comp <- igraph::components(g)$membership
    names(comp)[comp == comp[[sel]]]
  })

  output$stats <- renderText({
    net <- net_data()
    visible_ids <- visible_node_ids()
    active_edges <- filtered_edges()
    visible_edges <- active_edges %>% filter(source %in% visible_ids, target %in% visible_ids)
    n_groups <- length(unique(unlist(net$nodes$teams)))
    paste0(length(visible_ids), " visible nodes • ", nrow(visible_edges), " visible links • ", n_groups, " groups")
  })

  observeEvent(input$find_node, {
    net <- net_data()
    chosen <- resolve_character_id(input$search, net$nodes)
    if (is.null(chosen)) return()
    selected_node(chosen)
    focus_nonce(focus_nonce() + 1L)
    updateSelectizeInput(session, "search", selected = chosen, server = TRUE)
  })

  observeEvent(input$search, {
    net <- net_data()
    chosen <- resolve_character_id(input$search, net$nodes)
    if (is.null(chosen)) return()
    selected_node(chosen)
  })

  observeEvent(input$clear_select, {
    selected_node(NULL)
  })

  observeEvent(event_data("plotly_click", source = "msfnet"), {
    ed <- event_data("plotly_click", source = "msfnet")
    clicked <- ed$customdata %||% character(0)
    clicked <- as.character(clicked)
    if (!length(clicked) || !nzchar(clicked[[1]])) return()
    selected_node(clicked[[1]])
  })

  output$detail_panel <- renderUI({
    sel <- selected_node()
    if (is.null(sel)) {
      return(tagList(
        tags$h3(class = "detail-title", "Select A Character"),
        tags$p(class = "detail-muted", "Click a node to inspect teams, pair synergies, and direct links."),
        tags$div(class = "legend-row", tags$span(class = "dot", style = paste0("background:", source_colors[["team"]], ";")), "Team links"),
        tags$div(class = "legend-row", tags$span(class = "dot", style = paste0("background:", source_colors[["synergy"]], ";")), "Pair synergies"),
        tags$div(class = "legend-row", tags$span(class = "dot", style = paste0("background:", source_colors[["inferred"]], ";")), "Inferred minion links")
      ))
    }

    net <- net_data()
    node <- net$nodes %>% filter(id == sel)
    if (nrow(node) == 0) {
      return(tagList(
        tags$h3(class = "detail-title", "Character Not Found"),
        tags$p(class = "detail-muted", "Selected character is not available in the current graph.")
      ))
    }
    e <- filtered_edges() %>% filter(source == sel | target == sel)
    conn <- sort(unique(ifelse(e$source == sel, e$target, e$source)))

    tagList(
      tags$h3(class = "detail-title", node$name[[1]]),
      tags$p(class = "detail-muted", paste0(node$degree[[1]], " total links across all sources.")),
      tags$p(HTML(paste0("<strong>Teams/Groups</strong><br>", if (length(node$teams[[1]])) paste(node$teams[[1]], collapse = "<br>") else "No curated team tags yet."))),
      tags$p(HTML(paste0("<strong>Connected Characters (active filters)</strong><br>", if (length(conn)) paste(pretty_name(conn), collapse = ", ") else "No links with active filters.")))
    )
  })

  output$network_plot <- renderPlotly({
    focus_tick <- focus_nonce()
    net <- net_data()
    req(nrow(net$nodes) > 0)
    edges <- filtered_edges()
    coords <- net$coords

    visible_ids <- visible_node_ids()
    nodes <- net$nodes %>%
      filter(id %in% visible_ids) %>%
      left_join(coords, by = "id")
    edges <- edges %>% filter(source %in% visible_ids, target %in% visible_ids)
    sel_raw <- selected_node()
    sel <- if (length(sel_raw) == 1 && nzchar(sel_raw)) sel_raw else NULL
    near <- neighbors()
    node_size <- input$node_size %||% 7
    edge_opacity <- input$edge_opacity %||% 0.42

    if (is.null(sel)) {
      nodes <- nodes %>%
        mutate(plot_color = color, plot_size = node_size)
    } else {
      nodes <- nodes %>%
        mutate(
          plot_color = if_else(id %in% near, color, "#35455a"),
          plot_size = case_when(
            id == sel ~ node_size + 5,
            id %in% near ~ node_size + 1,
            TRUE ~ node_size
          )
        )
    }

    nodes <- nodes %>%
      mutate(
        x = coalesce(x, 0),
        y = coalesce(y, 0),
        z = coalesce(z, 0),
        plot_size = as.numeric(plot_size)
      )

    edges <- edges %>%
      mutate(
        primary = vapply(types, edge_primary_type, character(1)),
        connected = if (is.null(sel)) rep(TRUE, n()) else (source == sel | target == sel)
      )

    p <- plot_ly(source = "msfnet")

    for (tp in c("team", "synergy", "inferred")) {
      subset <- edges %>% filter(primary == tp)
      if (nrow(subset) == 0) next

      if (!is.null(sel)) {
        seg_dim <- build_segments(subset %>% filter(!connected), coords)
        if (nrow(seg_dim) > 0) {
          p <- p %>% add_trace(
            data = seg_dim,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d", mode = "lines",
            line = list(color = alpha_color(source_colors[[tp]], 0.08), width = 1),
            hoverinfo = "skip", showlegend = FALSE, inherit = FALSE
          )
        }
      }

      seg <- build_segments(subset %>% filter(connected), coords)
      if (nrow(seg) == 0) next
      p <- p %>% add_trace(
        data = seg,
        x = ~x, y = ~y, z = ~z,
        type = "scatter3d", mode = "lines",
        line = list(
          color = alpha_color(source_colors[[tp]], if (is.null(sel)) edge_opacity else 0.95),
          width = if (is.null(sel)) 1 else 2
        ),
        name = str_to_title(tp),
        hoverinfo = "skip",
        showlegend = FALSE,
        inherit = FALSE
      )
    }

    marker_size <- as.numeric(nodes$plot_size)
    marker_size[!is.finite(marker_size)] <- as.numeric(node_size)

    camera <- list(eye = list(x = 1.3, y = 1.2, z = 1.1))
    if (!is.null(sel) && nrow(nodes) > 0) {
      target <- nodes %>% filter(id == sel) %>% slice_head(n = 1)
      if (nrow(target) == 1) {
        camera <- list(
          center = list(
            x = -as.numeric(target$x) * 0.65,
            y = -as.numeric(target$y) * 0.65,
            z = -as.numeric(target$z) * 0.65
          ),
          eye = list(x = 0.45, y = 0.40, z = 0.38)
        )
      }
    }

    p <- p %>% add_markers(
      data = nodes,
      x = ~x, y = ~y, z = ~z,
      type = "scatter3d",
      marker = list(
        size = marker_size,
        opacity = 0.95,
        line = list(color = "#0d1624", width = 0.7)
      ),
      color = ~plot_color,
      customdata = ~id,
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "ID: %{customdata}<br>",
        "Links: %{meta}<extra></extra>"
      ),
      text = ~name,
      meta = ~degree,
      name = "Characters",
      showlegend = FALSE,
      inherit = FALSE
    )

    p <- p %>% layout(
      scene = list(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        zaxis = list(visible = FALSE),
        bgcolor = "#060d17",
        camera = camera
      ),
      paper_bgcolor = "#060d17",
      plot_bgcolor = "#060d17",
      margin = list(l = 0, r = 0, t = 0, b = 0),
      showlegend = FALSE
    )

    invisible(focus_tick)
    plotly::event_register(p, "plotly_click")
  })
}

shinyApp(ui, server)
