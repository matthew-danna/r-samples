# literally nothing without this blog: https://3mw.albert-rapp.de/p/ai-chat-bots-with-r-shiny-elmer

library(shiny)

ui <- bslib::page_fluid(
  titlePanel("Chat MSF 0.1"),
  div(
    tags$style(
      '.chat_reply {
        min-height: 50px;
        width: 700px;
        background: #143642;
        color: white;
        border: "1px solid black";
        border-radius: 5px;
        padding: 10px;
        margin: 5px;
        margin-right: auto;
        animation: moveUpFadeIn 0.5s ease-in-out forwards;
      }'
    ),
    tags$style(
      '.chat_input {
        min-height: 50px;
        width: 700px;
        background: #4C7A65;
        color: white;
        border: "1px solid black";
        border-radius: 5px;
        padding: 10px;
        margin: 5px;
        margin-left: auto;
        animation: moveUpFadeIn 0.5s ease-in-out forwards;
      }'
    ),
    tags$style('.form-group {margin: 0px}'),
    id = 'chat_container',
    style = htmltools::css(
      min_height = '600px',
      max_height = '900px',
      max_width = '1200px',
      margin_left = 'auto',
      width = '100%',
      border = '1px solid black',
      border_radius = '5px',
      background = '#EAE6E5',
      margin_top = '10px'
    ),
    div(
      id = 'chat_output',
      style = htmltools::css(
        height = '600px',
        overflow_y = 'auto',
        scrollbar_width = 'thin'
      ),
      div(
        class = 'chat_reply',
        "Hello! I'm your nerdy chatbot for all things Marvel Strike Force!
        How can I help you?
        Please remember I'm only as current as my free release allows..."
      )
    ),
    div(
      id = 'chat_input',
      class = 'd-flex',
      style = htmltools::css(
        height = '70px',
        width = '100%',
        border = '1px solid black',
        border_radius = '5px'
      ),
      textAreaInput(
        'textarea_chat_input',
        label = NULL,
        width = '85%',
        height = '100%',
        resize = 'none',
        placeholder = 'Type your questions here'
      ),
      actionButton(
        'send_text',
        label = 'Hit Me',
        width = '15%',
        border_radius = '0px 0px 0px 0px'
      )
    )
  )
)

server <- function(input, output, session) {
  chat <- ellmer::chat_github(
    system_prompt = "You are a data scientist that uses statistics to make decisions and has played the mobile game Marvel Strike Force since it's global launch",
    base_url = "https://models.inference.ai.azure.com/",
    api_key = Sys.getenv('GITHUB_PAT'),
    model = "gpt-4o",
    seed = NULL,
    api_args = list(),
    echo = "all"
  )
  
  observe({
    stream <- chat$stream(input$textarea_chat_input)
    
    insertUI( # Insert text box
      '#chat_output',
      where = 'beforeEnd',
      ui = div(
        class = 'chat_input',
        input$textarea_chat_input # Use inputs
      ),
      immediate = TRUE
    )
    updateTextAreaInput(
      inputId = 'textarea_chat_input',
      value = ''
    )
    
    insertUI( # insert response text box
      '#chat_output',
      where = 'beforeEnd',
      ui = div(
        class = 'chat_reply',
        ''
      ),
      immediate = TRUE
    )
    
    # stream response
    coro::loop(for (chunk in stream) {
      insertUI(
        '.chat_reply:last',
        where = 'beforeEnd',
        ui = chunk,
        immediate = TRUE
      )
    })
  }) |> bindEvent(input$send_text)
}

# Run the application 
shinyApp(ui = ui, server = server)



