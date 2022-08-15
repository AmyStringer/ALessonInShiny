## Basic Basic 
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The absolute bare minimum"),
    
    fluidRow(
      textInput(
        inputId = "answer", 
        label = "How do you define a salad??", width = "100%"
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

  output$sassyOut <- renderText({
    paste0("I asked you what defines a salad, and you answered with '", 
           input$answer, 
           "'. Surely you can come up with something more robust!")
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
