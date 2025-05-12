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
  titlePanel("Crappy MSF Chatbot using Gemini"),
  chat_ui("chat")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  chat.msf <- chat_gemini(
    system_prompt = "You are a data scientist that has played the mobile game Marvel Strike Force since it's global launch",
    turns = NULL,
    base_url = "https://generativelanguage.googleapis.com/v1beta/",
    api_key = Sys.getenv('GOOGLE_API_KEY'),
    model = "gemini-2.0-flash",
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
