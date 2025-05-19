#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    https://ellmer.tidyverse.org/


library(shiny)
library(shinychat)
library(ellmer)

# Define UI for application that draws a histogram
ui <- bslib::page_fluid(
  titlePanel("My First MSF Chatbot"),
  "Ask this slightly-below-average model from late 2023 questions about MSF...",
  chat_ui("chat")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  chat.msf <- chat_github(
    system_prompt = "You are a data scientist that uses statistics to make decisions and has played the mobile game Marvel Strike Force since it's global launch",
    base_url = "https://models.inference.ai.azure.com/",
    api_key = Sys.getenv('GITHUB_PAT'),
    model = "gpt-4o",
    seed = NULL,
    api_args = list(),
    echo = "all"
  )
  
  observeEvent(input$chat_user_input, {
    stream <- chat.msf$stream_async(input$chat_user_input)
    chat_append("chat", stream)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
