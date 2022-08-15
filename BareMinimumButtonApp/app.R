#
# Define UI for application that draws a histogram
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("The absolute bare minimum (with a button!)"),
  
  fluidRow(
    textInput(
      inputId = "answer", 
      label = "How do you define a salad??", width = "100%"
    ), 
    actionButton(
      inputId = "genText", 
      label = "Generate Output!"
    )
  ), 
  fluidRow(
    textOutput(
      outputId = "sassyOut"
    )
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  textOut <- reactiveValues(result = NULL)
  
  observeEvent(input$genText, {
    # create the text object we would like the app to render 
    textOut$result <- paste0(
      "I asked you what defines a salad, and you answered with '", 
      input$answer, 
      "'. Surely you can come up with something more robust!")
  })
  
  output$sassyOut <- renderText({
    textOut$result 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
