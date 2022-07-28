### NOTE FOR AMYS
# for the teaching session use
# runApp(display.mode = "showcase")

## Search for "some bullshit" to fix up hacky shit later on

## A template shiny app as part of a Clinical Brain Network teaching session
## Created by Amy Stringer
#
# The goal of the template is to provide code demonstrating all possible input/
# output options within R shiny, as well as the basic layout options.
#
# For a copy of the code used in the face to face teaching session (complete with
# functioning interactions between inputs and outputs, and some level of
# reactivity) see the shinyDemo repo
#
# This template is definitely not exhaustive, there are so many possibilities
# for design and functionality within shiny
# For another non-exhaustive list of things shiny can do, there is a cheat sheet!
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf

# good r coding practise - add all package calls to top of script
# prior to writing app contents
library(shiny)
library(shinythemes)      # contains shiny themes
library(DT)               # interactive data tables
library(tidyverse)        # a neat mega-package containing helpful data tools
library(plotly)

# here I create a subset of a built in dataframe for use in demonstration
# this subset was constructed purely for good visuals
diaSub <- diamonds %>%
  filter(price > 2000) %>%
  group_by(cut, color) %>%
  slice(1:60) %>%
  ungroup

## thoughts. I want to redo the outputs to include the mtcars dataset instead, and show the datatable to plot filter like shown in here
## https://rstudio.github.io/DT/shiny.html
mtcarsNew <- tibble(mtcars) %>%
  mutate(
    type = rownames(mtcars),
    cyl = as.factor(cyl),
    gear = as.factor(gear),
    am = as.factor(am),
    vs = as.factor(vs),
    am = fct_recode(am, "Automatic" = "0", "Manual" = "1"),
    vs = fct_recode(vs, "V-shaped" = "0", "straight" = "1")
  ) %>%
  rename(
    `Horse Power` = "hp",
    "Cylinders" = "cyl",
    "Weight" = "wt",
    "Transmission" = "am",
    "Engine" = "vs"
  )


# Define UI for application
ui <- navbarPage(
  title = "A Template (using a nav bar page)",
  # a particular theme can be added by uncommenting the below line
  # and changing "yourtheme" to you preferred theme name
  # theme = shinythemes::shinytheme("flatly"),
  
  # navbarPage is a tabbed UI, add as many tabs as you like...
  tabPanel(title = "A day in the life",
           
           # tab contents go in here
           # adding text and images
           h1("Amy, what are we doing here? "),
           h5("You\'re probably wondering,",
              tags$b('"What are we doing here? What is this nonsense? Why is Amy forcing R on us?"')
           ),
           h5("I don't have a good answer for that other than I was invited because I have a fair bit of expertise on the topic. It'll be fun I promise."),
           h5("So far, there's 2 sessions booked so sit tight. Maybe you'll even learn something!"),
           
           tags$br(),
           
           fluidRow(
             column(style = 'border-right: 1px solid gray',
                    width = 6,
                    h2("Session 1 From R to the UN: A shiny user interface"),
                    h4("Session one will hopefully provide a gentle introduction has two main parts:"),
                    # we can created an ordered list
                    tags$ol(
                      # that contains multiple list items
                      tags$li("A gallery component where I can flex a little bit and show off some thigns I have made"),
                      tags$li("A demonstration on how to set up a workable user interface in shiny")
                    ),
                    h5("A workable user interface includes elements such as:"),
                    # sometimes we prefer an un-ordered list
                    tags$ul(
                      tags$li("Available inupts and outputs"),
                      tags$li("Some of the posisble layouts"),
                      tags$li("Themes! So you can make an app that looks a lot better than this one...")
                    )
             ),
             column(width = 6,
                    h2("Session 2 From R to the UN: A shiny back end"),
                    h4("This session (if I'm not chased out with pitchforks after today) will cover some of the juicier stuff. For example:"),
                    tags$ul(
                      tags$li("The relationship between the User interface and server components"),
                      tags$li("Interactive vs static outputs"),
                      tags$li("Reactivity within a shiny app"),
                      tags$li("Application deployment - the easier options at least")
                    )
             )
           ),
           tags$br(),
           fluidRow(
             column(width = 3,
                    # i just want to centre my text - this is empty fluff 
             ),
             column(width = 6,
                    h2("These sessions will NOT cover: "),
                    tags$ul(
                      tags$li("How to code in R"),
                      tags$li("How to use RStudio/ggplot/whatever other R package your second cousin twice removed told you about"),
                      tags$li("Good coding practise for R or Shiny"),
                      tags$li("Installing R, RStudio or any R packages"),
                      tags$li("Every single way you can do every single thing")
                    ),
                    h5("If you happen to be interested in any of these additional things, we can arrange separate teaching sessions, just let me know!")
             ),
             column(width = 3,
                    
                    # i just want to centre my text - this is empty fluff 
             )
           ),
           hr(),
           fluidRow(
             column(width = 2,
                    
                    # i just want to centre my images - this is empty fluff
                    
             ),
             column(width = 8,
                    div(style = "display: inline-block;", 
                        img(src = "app.png",
                            height = 400,
                            width = 300)
                    ),
                    div(style = "display: inline-block;", 
                         img(src = "tasks.png",
                             height = 400,
                             width = 300)
                    ),
                    div(style = "display: inline-block;", 
                        img(src = "goodcode.png",
                            height = 400, 
                            width = 300)
                    )
             ),
             column(width = 2,
                    
                    # i just want to centre my images - this is empty fluff 
                    
             )
           )
  ),
  tabPanel(
    
    title = "What's a shiny?",
    h1("R to the UN? huh?"),
    h4("The easiest way to tell you wat a shiny app is, is to show you (but really it's just a web application that you can use RStudio to make)."),
    
    fluidRow(
      column(width = 3, 
             
             # this is just empty fluff to create some space 
             
      ),
      column(width = 9,
             h5("Here's a list of some of the apps I've made that are actually deployed... "),
             h5("Most of them have been targetting to schools across Queensland"),
             tags$ul(
               tags$li(
                 tags$a(href = "https://virtualreef.shinyapps.io/vrdpractical/", "An app for when COVID-19 took away school excursions.")
               ),
               tags$li(
                 tags$a(href = "https://qutschoolofmaths.shinyapps.io/mathematicalmethods/", 'An app for when "Maths B" caught up with the rest of the country.')
               ),
               tags$li(
                 tags$a(href = "https://qutschoolofmaths.shinyapps.io/specialistmaths/", 'An app for when "Maths C" did the same!')
               )
             ),
             h5("Others still have me pinching myself"),
             tags$ul(
               tags$li(
                 tags$a(href = "https://qutschoolofmaths.shinyapps.io/uncoursetoolapp/", "An top secret prototype app for REDACTED to teach the REDACTED how to use REDATED for REDACTED. ")
               ), 
             ),
             h5("If you hold off on the pitchforks for near the full hour, I'll even show you how the prototype turned out in the final deployment stage."),
             actionButton("startLesson", "On with the lesson!")
      ),
      uiOutput("OnWithTheLesson")
    )
  ),
  tabPanel(title = "Inputs!",
           
           # row 1 - info on using inputs in R
           fluidRow(
             h1("Shiny Inputs!"),
             h4("Below are a few examples of different kinds of inputs that can be used in a R shiny web application."),
             tags$br()
           ),
           # rule some horizontal lines, because why not? They look nice.
           tags$hr(),
           h4("Different kinds of select inputs"),
           h5("Some of these have additional inputs that allow a user to select multiple items, or to force a default selection."),
           
           ### input options
           ## row 2
           fluidRow(
             # radio buttons
             column(width = 4,
                    radioButtons(
                      inputId = "radButton",
                      label = "Select an x axis variable",
                      choices = names(mtcarsNew %>% select(where(is.numeric)))
                    )
             ),
             # select input
             column(width = 4,
                    selectInput(
                      inputId = "selectIn",
                      label = "Select a y axis variable",
                      choices = names(mtcarsNew %>% select(where(is.numeric))),
                      multiple = FALSE
                    )
             ),
             # checkbox input
             column(width = 4,
                    checkboxGroupInput(
                      inputId = "checkboxIn",
                      label = "Select a colour variable",
                      choices = names(mtcarsNew %>% select(where(is.factor)))
                   )
             )
           ),
           
           tags$br(),
           tags$hr(),
           
           h4("Inputs for dates, files or (the dreaded) text"),
           h5("Here we can select a date range, though there is an input that allows for a single date selection also. A file input can be configured to allow multiple file uploads at once, and the text input is (predictably) unconstrained."),
           
           ## row 3
           fluidRow(
             # date range input
             column(width = 4,
                    dateRangeInput(
                       inputId = "dateRange",
                       label = "Select a date range to view",
                       start = min(economics$date),
                       end = max(economics$date)
                     )
             ),
             # file input
             column(width = 4,
                    fileInput(
                      inputId = "fileIn",
                      label = "You can upload files",
                      multiple = FALSE
                    )
             ),
             # text input
             column(width = 4,
                    textInput(inputId = "textIn",
                              label = "A risky text input")
             )
           ),
           
           tags$br(),
           tags$hr(),
           
           h4("Inputs for, uh, Other things..."),
           h5("Got some super secret login portal inside your app? Shiny can hide your password from lurkers in the office!"),
           h5("Numbers! When are these not useful?!"),
           h5("Shiny auto produces input reliant outputs IMMEDIATELY as they change. This is super annoying most of the time, and for those times, there are action buttons!"),
           
           ## row 4
           fluidRow(
             # password input
             column(width = 4,
                    passwordInput(inputId = "passIn",
                                  label = "Super secret password input")
             ),
             # numeric input
             column(width = 4,
                    numericInput(
                      inputId = "numIn",
                      label = "An example numeric input",
                      min = -100000,
                      max = 100000,
                      value = 0
                    )
             ),
             # action button
             column(width = 4,
                    h5("Want to trigger an action? Use an action button..."),
                    actionButton(inputId = "action",
                                 label = "Do a thing!")
             ),
             tags$hr()
           )
  ),
  tabPanel(title = "Outputs!",
           
           # possible outputs - maybe multiple tabs with different iterations
           # or just rows with iterative interactivity in the cols
           
           h1("Some example outputs"),
           h3("There are many possible outputs, here I focus one just two and the different ways to make them interactive. "),
           tabsetPanel(
             tabPanel("Table Outputs - Basic",
                      
                      fluidRow(
                        column(width = 6,
                               h2("Table 1 - A Basic Table"),
                               tableOutput(outputId = "simpleTable")
                        ),
                        
                        tags$br(),
                        
                        column(width = 5,
                               h2("Cons"),
                               tags$ul(
                                 tags$li("It's an eye sore"),
                                 tags$li("There is no way to interact with it"),
                                 tags$li('"As seen in the R console"'),
                                 tags$li("More suited for a static report"),
                                 tags$li("If we were to show more than 10 rows, it would not fit to a page i.e. no scroll feature")
                               ),
                               h2("Pros"),
                               tags$ul(
                                 tags$li("It presents data in a readable table format")
                               )
                        )
                      )
             ),
             tabPanel("Table Outputs - Fancy",
                      fluidRow(
                        column(width = 6,       
                               h2('Table 2 - A "Shiny" table'),
                               DT::DTOutput(outputId = "fancyDataTable")
                        ),
                        column(width = 6,
                               h2("Table 2.1 - Selected rows only"),
                               DT::DTOutput(outputId = "filteredDTOutput")
                        )
                      )
             ),
             tabPanel("Plot/Table Linked Outputs",
                      
                      h2("Interactive and Linked Outputs"),
                      fluidRow(
                        column(width = 6,
                                      DTOutput(outputId = "mtcarstable")
                        ),
                        column(width = 6,
                               plotlyOutput(outputId = "plotSheFancy")
                        )
                      )
             )
           )
  ),
  tabPanel(title = "Layouts!",
    # add a theme browser to the UI to test available themes
    shinythemes::themeSelector(),
    
    # a tabbed layout, containing examples of other layouts
    tabsetPanel(
      tabPanel("Tab Layouts!",
          tabsetPanel(
              tabPanel("Tab 1",
                       # you could put stuff in here          
              ),
              tabPanel("Tab 2",            
                  tabsetPanel(
                      tabPanel("Tab 1",             
                               # you could put stuff in here too 
                      ),
                      tabPanel("Tab 2",               
                               # you could put stuff in here too 
                      ),
                      tabPanel("Tab 3",
                          tabsetPanel(
                              tabPanel("Tab 1", 
                                       # you could put stuff in here          
                              ),
                              tabPanel("Tab 2", 
                                       # you could put stuff in here too 
                              ),
                              tabPanel("Tab 3", 
                                       # you could put stuff in here too 
                              ),
                              tabPanel("Tab 4",
                                       fluidRow(
                                         column(width = 4, 
                                                # some more blank stuff  
                                         ),
                                         column(width = 4,         
                                                div(
                                                  style = "display: inline-block;", 
                                                  img(src = "recursion.gif",
                                                      height = 800,
                                                      width = 700
                                                  )
                                                )
                                          ),
                                          column(width = 4, 
                                                 # some more blank stuff 
                                          )
                                        )
                              )
                          )
                      )
                  )
              )
          )
      ),
      tabPanel("Sidebar Layouts!",
               sidebarLayout(
                 sidebarPanel(
                   h2("Sidebar panel 1"),
                   h5("Typically side bar panels will contain various inputs"),
                   h5("That relate to the outputs displayed in the main panel"),
                   h5("For Example:"),
                   sliderInput(
                     inputId = "slider",
                     label = "A slider input",
                     min = 1,
                     max = 32,
                     value = 10
                   )
                 ),
                 mainPanel(
                   h2("Main panel 1"),
                   DT::DTOutput(outputId = "recursiveTab"),
                   
                   sidebarLayout(
                     sidebarPanel(
                       h3("Sidebar panel 2"),
                       sliderInput(
                         inputId = "slider2",
                         label = "A slider input",
                         min = 1,
                         max = 32,
                         value = 10
                       )
                     ),
                     mainPanel(
                       h3("Main panel 2"),
                       DT::DTOutput(outputId = "recursiveTab2"),
                       
                       sidebarLayout(
                         sidebarPanel(
                           h4("Sidebar panel 3"),
                           sliderInput(
                             inputId = "slider3",
                             label = "A slider input",
                             min = 1,
                             max = 32,
                             value = 10
                           )
                         ),
                         mainPanel(
                           h4("Main panel 3"),
                           DT::DTOutput(outputId = "recursiveTab3"),
                           
                           sidebarLayout(
                             sidebarPanel(
                               h5("Sidebar panel 4"),
                               sliderInput(
                                 inputId = "slider4",
                                 label = "A slider input",
                                 min = 1,
                                 max = 32,
                                 value = 10
                               )
                             ),
                             mainPanel(
                               h5("Main panel 4"),
                               DT::DTOutput("recursiveTab4"),
                               
                               sidebarLayout(
                                 sidebarPanel(
                                   h6("Sidebar panel 5"),
                                   sliderInput(
                                     inputId = "slider5",
                                     label = "A slider input",
                                     min = 1,
                                     max = 32,
                                     value = 10
                                   )
                                 ),
                                 mainPanel(
                                   h6("Main panel 5"),
                                   
                                   div(style = "display: inline-block;", 
                                       img(
                                         src = "recursion.gif",
                                         height = 300,
                                         width = 200
                                       )
                                   )
                                 )
                               )
                             )
                           )
                         )
                       )
                     )
                   )
                 )
               )
      ), 
      tabPanel("Grid Layouts", 
        hr(),        
        fluidRow(
          column(width = 4, 
                 style = 'border-right: 1px solid gray', 
                 h1("A little texty text"),
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br()
          ), 
          column(width = 4, 
                 style = 'border-right: 1px solid gray', 
                 h1("A little texty text"), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br()
          ), 
          column(width = 4, 
                 # style = 'border-right: 1px solid gray',
                 h1("A little texty text"), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br()
          ), 
        ), 
        hr(),
        fluidRow(
          column(width = 4, 
                 # style = 'border-right: 1px solid gray',
                 h1("A little texty text"), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br()
          ), 
          column(width = 4, 
                 style = 'border-right: 1px solid gray; border-left: 1px solid gray',
                 fluidRow(
                   # hr(), 
                    column(width = 6, 
                           style = 'border-right: 1px solid gray',
                           h1("A little texty text"), 
                           br(), 
                           br(), 
                           br()
                    ), 
                    column(width = 6,
                           h1("A little texty text"), 
                           br(), 
                           br(), 
                           br()
                    ), 
                   hr()
                 ), 
                 hr(),
                 fluidRow(
                   column(width = 6, 
                          style = 'border-right: 1px solid gray',
                          h1("A little texty text"), 
                          br(), 
                          br(), 
                          br()
                   ), 
                   column(width = 6,
                          h1("A little texty text"), 
                          br(), 
                          br(), 
                          br()
                   )
                 )
          ), 
          column(width = 4, 
                 # style = 'border-left: 1px solid gray',
                 fluidRow(
                   style = 'padding-left: 20px',
                   h1("A little texty text")
                   
                 ),
                 br(), 
                 br(), 
                 br(),
                 hr(),
                 fluidRow(
                   style = 'padding-left: 20px',
                   h1("A little texty text")
                   
                 ), 
                 br(), 
                 br(), 
                 br()
          )
        ), 
        hr(), 
        fluidRow(
          column(width = 4, 
                 style = 'border-right: 1px solid gray',
                 h1("A little texty text"), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(), 
                 br(),
                 br(), 
                 br(), 
                 br(),
                 br(), 
                 br(), 
                 br(),
                 br(),
                 br(), 
                 br(),
                 br()
          ), 
          column(width = 4, 
                 # style = 'border-right: 1p solid gray',
                 
                 column(width = 6,
                        # style = 'border-right: 1p solid gray',
                        h1("A little texty text"), 
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(),
                        br(), 
                        br(), 
                        br(),
                        br(), 
                        br(), 
                        br(),
                        br(),
                        br(), 
                        br(),
                        br()
                 ), 
                 column(width = 6, 
                        style = 'border-left: 1px solid gray',
                        h1("A little texty text"),
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        br(),
                        br(), 
                        br(), 
                        br(),
                        br(), 
                        br(), 
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()
                 )
                 
          ), 
          column(width = 4, 
                 style = 'border-left: 1px solid gray',
                 fluidRow(
                   column(width = 4,
                     h1("A little texty text")
                     
                   ), 
                   column(width = 4,
                          style = 'border-left: 1px solid gray',
                     h1("A little texty text")
                     
                   ), 
                   column(width = 4,
                          style = 'border-left: 1px solid gray',
                     h1("A little texty text")
                     
                   )
                 ), 
                 hr(),
                 fluidRow(
                   column(width = 4,
                     h1("A little texty text")
                     
                   ), 
                   column(width = 4,
                          style = 'border-left: 1px solid gray',
                     h1("A little texty text")
                     
                   ), 
                   column(width = 4,
                          style = 'border-left: 1px solid gray',
                     h1("A little texty text")
                     
                   )
                 ), 
                 hr(),
                 fluidRow(
                   column(width = 4,
                     h1("A little texty text")
                     
                   ), 
                   column(width = 4,
                          style = 'border-left: 1px solid gray',
                     h1("A little texty text")
                     
                   ), 
                   column(width = 4,
                          style = 'border-left: 1px solid gray',
                     # h1("A little testy test")
                     div(
                       style = "display: inline-block;", 
                       img(src = "recursion.gif",
                           height = 150,
                           width = 150
                       )
                     )
                     
                     
                   )
                 )
                 
          )
        ), 
        hr()
      )
    )
  ), 
  tabPanel("Back to the UN", 
  
           h1("The Final REDACTED"), 
           tags$a(href = "https://unstats.un.org/bigdata/task-teams/training/catalog/"), "The final training catalouge"
                    
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  #### Outputs for the "what's a shiny" tab
  observeEvent(input$startLesson, {
    output$OnWithTheLesson <- renderUI({
      tagList(
              fluidRow(
                column(
                  width = 6,
                  h1("The Basics"),
                  h2("Adding text to a shiny app"),
                  tags$br(),
                  h1("For headings:", tags$code('h1("For headings:")')),
                  tags$br(),
                  h2("Subheading 1:",  tags$code('h2("Subheading 1:")')),
                  tags$br(),
                  h3("Subheading 2:", tags$code('h3("Subheading 2:")')),
                  tags$br(),
                  h4("Subheading 3:", tags$code('h4("Subheading 3:")')),
                  tags$br(),
                  h5("Paragraph text:", tags$code('h5("Paragraph text:")')),
                  tags$br(),
                  h6("Little text:", tags$code('h6("Little text:")'))
                ), 
                column(width = 6,
                       div(
                         style = "display: inline-block;", 
                         img(src = "tags.png",
                             height = 500,
                             width = 500
                         )
                       )  
                )
              ))
    })
  })
  
  #### Outputs for the "Outputs/Simple table" tab ####
  tableData <- reactive({
    # tableData <- 
      economics %>%
      filter(between(date, input$dateRange[1], input$dateRange[2]))
    
  })
  
  output$simpleTable <- renderTable({
    tableData() %>%
      slice(1:10)
  })
  
  #### Outputs for the "Outputs/Table Outputs (fancy)" tab ####
  output$fancyDataTable <- DT::renderDT({
    datatable(tableData())
  })
  
  output$filteredDTOutput <- renderDT({
    datatable(tableData()[input$fancyDataTable_rows_selected, ])
  })
  
  #### Outputs for the "Outputs/Plot/table linked outputs" tab ####
  output$mtcarstable <- renderDT({
    datatable(data = mtcarsNew,
              options = list(scrollX = TRUE))
  })
  
  output$plotSheFancy <- renderPlotly({
    s1 = input$mtcarstable_rows_current  # rows on the current page
    s2 = input$mtcarstable_rows_all      # rows on all pages (after being filtered)
    
    basic <- ggplot() +
      geom_point(data = mtcarsNew,
                 aes(x = Weight, y = mpg, shape = "All Data"),
                 size = 2) +
      theme_bw() +
      labs(y = "Miles per Gallon")
    
    if (length(s1) <= 0) {

      new <- basic

    } else if ((length(s1) > 0) && !(length(s2) < nrow(mtcarsNew))) {
      
      new <- basic +
        geom_point(
          data = mtcarsNew[s1, ],
          aes(x = Weight, y = mpg, shape = "Current Page Data"),
          size = 3
        ) +
        scale_shape_manual(
          name = "Legend",
          values = c(1, 16),
          labels = c("All Data", "Current Page Data")
        )
      
    } else if (length(s1) &&
               (length(s2) > 0) &&
               (length(s2) < nrow(mtcarsNew))) {
      
        ## YET TO DO: add legend for this one - need to find a way to legend the colour and shape
        s = input$mtcarstable_search
        txt = if (is.null(s) || s == '') 'Filtered data'
        else {
          sprintf('Data matching "%s"', s)
        }
        
        new <- basic +
          geom_point(data = mtcarsNew[s1, ], aes(x = Weight, y = mpg, shape = "Current Page Data"), size = 3) +
          geom_point(
            data = mtcarsNew[s2, ],
            aes(x = Weight, y = mpg, shape = txt),
            size = 5,
            color = "red"
          ) +
          scale_shape_manual(
            name = "legend",
            values = c(1, 16, 0),
            labels = c("All Data", "Current Page Data", txt)
          )
    }
    
    new
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
