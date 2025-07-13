library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)
library(plotly)

required_cols <- c("Character.Id", "Level", "Gear.Tier", "Stars", "Red.Stars", "Power", "Player")
iso_classes <- c("Striker", "Fortifier", "Healer", "Raider", "Skirmisher")
all_required <- c(required_cols, iso_classes)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .header-logo {
        background-color: black;
        padding: 10px 15px;
        display: flex;
        align-items: center;
        margin-bottom: 10px;
      }
      .header-logo img {
        height: 60px;
        margin-right: 20px;
      }
      .header-title h1 {
        color: white;
        margin: 0;
        font-size: 28px;
      }
      .description {
        margin-bottom: 20px;
        font-size: 16px;
      }
      .hideable-panel {
        background-color: #f9f9f9;
        padding: 10px;
        border: 1px solid #ccc;
        border-radius: 6px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  tags$script(HTML("
  Shiny.addCustomMessageHandler('clickToggle', function(message) {
    const btn = document.getElementById('togglePanel');
    if (btn) btn.click();
  });
")),
  
  # Header and description
  div(class = "header-logo",
      img(src = "https://cdn-msf.marvelstrikeforce.com/static/logo-en.png"),
      div(class = "header-title", h1("MSF Roster Explorer-er"))
  ),
  
  div(class = "description",
      p("Upload your MSF roster and explore. Still under construction - more updates soon.")
  ),
  
  # Always-visible Upload button
  fileInput("file", "📂 Upload Your Roster CSV", accept = ".csv"),
  
  # Toggle Sidebar Panel
  uiOutput("toggleBtn"),
  
  fluidRow(
    column(
      width = 3,
      conditionalPanel(
        condition = "output.showFiltersUI",
        div(class = "hideable-panel",
            uiOutput("filters"),
            textOutput("validationError")
        )
      )
    ),
    column(
      width = 9,
      tabsetPanel(
        tabPanel("Data Table",
                 actionButton("copyBtn", "📋 Copy Table to Clipboard"),
                 DTOutput("charTable")),
        tabPanel("ISO Class Distribution", plotlyOutput("isoPlot")),
        tabPanel("Squad Power Summary", verbatimTextOutput("summaryText")),
        tabPanel("Gear Tier Distribution", plotlyOutput("gearPlot")),
        tabPanel("Power Distribution", plotlyOutput("powerPlot")),
        tabPanel("Stars Distribution", plotlyOutput("starsPlot")),
        tabPanel("Player Comparison", plotlyOutput("comparisonPlot"))
      )
    )
  ),
  
  # JS for Copy-to-Clipboard
  tags$script(HTML("
    document.getElementById('copyBtn')?.addEventListener('click', function() {
      const table = document.querySelector('#charTable table');
      if (!table) return;
      let text = '';
      const rows = table.querySelectorAll('tr');
      rows.forEach(row => {
        const cells = row.querySelectorAll('th, td');
        const rowText = Array.from(cells).map(cell => cell.innerText).join('\\t');
        text += rowText + '\\n';
      });
      navigator.clipboard.writeText(text);
      alert('✅ Table copied to clipboard!');
    });
  "))
)

server <- function(input, output, session) {
  
  # Track whether filters are shown or hidden
  showFilters <- reactiveVal(FALSE)
  
  # Toggle visibility when the button is clicked
  observeEvent(input$togglePanel, {
    showFilters(!showFilters())
  })
  
  # Auto-show filters when file is uploaded
  observeEvent(input$file, {
    showFilters(TRUE)
  })
  
  output$toggleBtn <- renderUI({
    label <- if (showFilters()) "Hide Filters" else "Show Filters"
    actionButton("togglePanel", label)
  })
  
  output$showFiltersUI <- reactive({
    showFilters()
  })
  outputOptions(output, "showFiltersUI", suspendWhenHidden = FALSE)
  
  roster_data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    if ("Name" %in% names(df)) names(df)[names(df) == "Name"] <- "Player"
    if (!all(all_required %in% names(df))) return(NULL)
    df[iso_classes] <- lapply(df[iso_classes], function(x) !is.na(x))
    df
  })
  
  observeEvent(input$file, {
    updateActionButton(session, "togglePanel", label = "Hide Filters")
  })
  
  output$validationError <- renderText({
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    if ("Name" %in% names(df)) names(df)[names(df) == "Name"] <- "Player"
    missing <- setdiff(all_required, names(df))
    if (length(missing) > 0) paste("❌ Missing required columns:", paste(missing, collapse = ", ")) else ""
  })
  
  output$filters <- renderUI({
    df <- roster_data()
    req(df)
    
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
      selectInput("player", "Player", choices = sort(unique(df$Player)), selected = NULL, multiple = TRUE),
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
    df <- roster_data()
    req(df)
    
    # Prevent evaluation if filters aren't mounted yet
    if (is.null(input$gear) || is.null(input$stars) || is.null(input$redstars)) return(df)
    
    if (!is.null(input$player) && length(input$player) > 0) {
      df <- df[df$Player %in% input$player, ]
    }
    if (!is.null(input$character) && input$character != "All") {
      df <- df[df$Character.Id == input$character, ]
    }
    
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
    req(nrow(df) > 0, all(required_cols %in% colnames(df)))
    isolate({
      datatable(df, options = list(pageLength = 15))
    })
  })
  
  output$isoPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    iso_counts <- colSums(df[, iso_classes, drop = FALSE])
    iso_df <- data.frame(ISO = names(iso_counts), Count = iso_counts)
    
    p <- ggplot(iso_df, aes(x = reorder(ISO, Count), y = Count)) +
      geom_col(fill = "#3498db") +
      coord_flip() +
      labs(title = "ISO-8 Class Distribution", x = "ISO-8 Class", y = "Count") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$summaryText <- renderPrint({
    df <- filtered_data()
    req(nrow(df) > 0)
    total_power <- sum(df$Power, na.rm = TRUE)
    avg_power <- round(mean(df$Power, na.rm = TRUE), 1)
    char_count <- nrow(df)
    
    cat("🧠 Squad Power Summary\n")
    cat("----------------------\n")
    cat("Total Characters:", char_count, "\n")
    cat("Total Squad Power:", format(total_power, big.mark = ","), "\n")
    cat("Average Power per Character:", format(avg_power, big.mark = ","), "\n")
  })
  
  output$gearPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = as.factor(Gear.Tier))) +
      geom_bar(fill = "#e67e22") +
      labs(title = "Gear Tier Distribution", x = "Gear Tier", y = "Count") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$powerPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)

    p <- ggplot(df, aes(x = Power)) +
      geom_histogram(binwidth = 1000, fill = "#2ecc71", color = "black") +
      labs(title = "Power Distribution", x = "Power", y = "Characters") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$starsPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = as.factor(Stars))) +
      geom_bar(fill = "#9b59b6") +
      labs(title = "Stars Distribution", x = "Stars", y = "Count") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$comparisonPlot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0)
    
    comparison <- df %>%
      group_by(Player) %>%
      summarise(TotalPower = sum(Power, na.rm = TRUE)) %>%
      arrange(desc(TotalPower))
    
    p <- ggplot(comparison, aes(x = reorder(Player, TotalPower), y = TotalPower)) +
      geom_col(fill = "#1abc9c") +
      coord_flip() +
      scale_y_continuous(labels = comma) +  # full numbers with commas
      labs(title = "Total Squad Power by Player", x = "Player", y = "Total Power") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y"))
  })
}

shinyApp(ui, server)