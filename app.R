### NOTE FOR AMYS
# for the teaching session use
# runApp(display.mode = "showcase")

## Search for "some bullshit" to fix up hacky shit later on

## A "template" shiny app as part of a teaching session - template is being used VERY loosely here. 
## Created by Amy Stringer
#
# The goal of the template is to provide code demonstrating some of the possible input/
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

# fixed up one of the built in dataset to use as demonstration for outputs 
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
  ) %>% 
  relocate(type)


# Define UI for application
ui <- shinyUI(
  
  # this is exactly as it sounds, a page that is fluid - you can put whatever you want in there 
  fluidPage(
    list(
      # to start with a want to add a page header with a logo and title 
    div(style="padding: 1px 0px; width: '100%'",
        # you can do this with a title panel 
        titlePanel(windowTitle = "It's a Teaching Session!",
                   title =
                     # i want to divide the header into text and image
                     div(
                       # image first 
                       img(
                         src = "logo.png",
                         height = 100,
                         width = 100,
                         style = "margin:10px 10px"
                       ),
                       # then text 
                       "It's a Teaching Session!"
                     )
        )
    )),
  # navbarPage is a tabbed layout that give page tabs across the top - slightly different to tab set which you can also see throughout this app
  navbarPage(
    # every navbarPage needs a title 
  title = "A Demo Shiny App",
  
  # a particular theme can be added by uncommenting the below line
  # and changing "yourtheme" to you preferred theme name
  # theme = shinythemes::shinytheme("flatly"),
  
  # navbarPage is a tabbed UI, add as many tabs as you like...
  tabPanel(title = "Agenda",
           
           # tab contents go in here
           # adding text and images
           h1("Amy, what are we doing here? "),
           h5("You\'re probably wondering,",
              # tags$b give bold text. This is also a HTML wrapper 
              tags$b('"What are we doing here? What is this nonsense? Why is Amy forcing R on us?"')
           ),
           h5("I don't have a good answer for that other than I was invited because I'm great. It'll be fun I promise."),
           h5("So far, there's 2 sessions booked so sit tight. Maybe you'll even learn something!"),
           
           # page breaks. Another HTML wrapper 
           tags$br(),
           
           # fluidRow is like fluidPage but for rows within a page 
           fluidRow(
             # we can break rows up into columns
             # just make sure the sum of column widths does not exceed 12 (a full row width)
             column(style = 'border-right: 1px solid gray',                    # there are many style tags, this one adds a border to the column. Fairly sure this is CSS styling
                    width = 6,
                    # text is added using these HTML wrappers 
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
                    # I'm sure there is a better way, but this works just fine 
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
           # this rules a horizontal line under the last element
           # it's another HTML wrapper. tags$hr() would do the same thing 
           hr(),
           fluidRow(
             column(width = 2,
                    
                    # i just want to centre my images - this is empty fluff
                    
             ),
             column(width = 8,
                    # divide the space into three images 
                    div(style = "display: inline-block;", 
                        img(src = "app.png",
                            height = 400,
                            width = 300), 
                        img(src = "tasks.png",
                            height = 400,
                            width = 300), 
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
  # next panel in the navbar page 
  tabPanel(title = "What's a shiny?",
    # the above title will be visible inside the tab selector 
    # but maybe we also want a little inside the tab 
    h1("R to the UN? huh?"),
    h4("The easiest way to tell you wat a shiny app is, is to show you (but really it's just a web application that you can use RStudio to make)."),
    
    fluidRow(
      # I only have a little bit of text here, so I want it to start closer to the centre of the row 
      column(width = 3, 
             
             # this is just empty fluff to create some space 
             
      ),
      column(width = 9,
             h5("Here's a list of some of the apps I've made that are actually deployed... "),
             h5("Most of them were made for use in schools across Queensland"),
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
             h5("I made one that was a bit mroe spicy"),
             tags$ul(
               tags$li(
                 tags$a(href = "https://qutschoolofmaths.shinyapps.io/uncoursetoolapp/", "An top secret prototype app for REDACTED to teach the REDACTED how to use REDATED for REDACTED. ")
               ), 
             ),
             h5("If you hold off on the pitchforks for near the full hour, I'll even show you how the prototype turned out in the final deployment stage."),
             ### oooh what's this? A button???? (this links to an output item in the server section of the script
             actionButton(inputId = "startLesson", label = "On with the lesson!)")
      ),
      # UI output allows you to add dynamic user interface elements. This is going to be linked with the above action button. 
      # read on in the server code to see how 
      uiOutput(outputId = "OnWithTheLesson")
    )
  ),
  # A new tab for the navbar 
  tabPanel(title = "Inputs!",
           # this tab is all about the different kinds of inputs you can have 
           
           # row 1 - some text 
           fluidRow(
             h1("Shiny Inputs!"),
             h4("Below are a few examples of different kinds of inputs that can be used in a R shiny web application."),
             tags$br()
           ),
           # rule some horizontal lines, because why not? They look nice.
           tags$hr(),
           h4("Different kinds of select inputs"),
           h5("Some of these have additional options that allow a user to select multiple items, or to force a default selection."),
           
           # row 2 - the first set of input options 
           fluidRow(
             # row 2 col 1
             # radio buttons
             column(width = 4,
                    radioButtons(
                      # inputId is what is used to reference the input within the source code 
                      inputId = "radButton",
                      # but the label is what the user sees
                      label = "Select an x axis variable",
                      # the choices here are the numerical cols for the dataframe i created before the UI code 
                      choices = names(mtcarsNew %>% select(where(is.numeric)))
                    )
             ),
             # row 2 col 2
             # select input
             column(width = 4,
                    selectInput(
                      inputId = "selectIn",
                      label = "Select a y axis variable",
                      # the choices here are the numerical cols for the dataframe i created before the UI code 
                      choices = names(mtcarsNew %>% select(where(is.numeric))),
                      multiple = FALSE
                    )
             ),
             # row 2 col 3
             # checkbox input 
             column(width = 4,
                    checkboxGroupInput(
                      inputId = "checkboxIn",
                      label = "Select a colour variable",
                      choices = names(mtcarsNew %>% select(where(is.factor)))
                   )
             )
           ),
           
           # add some space an rule a line 
           tags$br(),
           tags$hr(),
           
           # some more text before the next row 
           h4("Inputs for dates, files or (the dreaded) text"),
           h5("Here we can select a date range, though there is an input that allows for a single date selection also. A file input can be configured to allow multiple file uploads at once, and the text input is (predictably) unconstrained."),
           
           ## row 3 - the second row of input options 
           fluidRow(
             # row 3 col 1
             # date range input
             column(width = 4,
                    dateRangeInput(
                       inputId = "dateRange",
                       label = "Select a date range to view",
                       start = min(economics$date),
                       end = max(economics$date)
                     )
             ),
             # row 3 col 2
             # file input
             column(width = 4,
                    fileInput(
                      inputId = "fileIn",
                      label = "You can upload files",
                      multiple = FALSE
                    )
             ),
             # row 3 col 3
             # text input
             column(width = 4,
                    textInput(inputId = "textIn",
                              label = "A risky text input")
             )
           ),
           
           # add some space and rule a line 
           tags$br(),
           tags$hr(),
           
           h4("Inputs for, uh, Other things..."),
           h5("Got some super secret login portal inside your app? Shiny can hide your password from lurkers in the office!"),
           h5("Numbers! When are these not useful?!"),
           h5("Shiny auto produces input reliant outputs IMMEDIATELY as they change. This is super annoying most of the time, and for those times, there are action buttons!"),
           
           ## row 4 - the final row of possible inputs 
           fluidRow(
             # rows 4 col 1 
             # password input
             column(width = 4,
                    passwordInput(inputId = "passIn",
                                  label = "Super secret password input")
             ),
             # row 4 col 2
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
             # row 4 col 3
             # action button
             column(width = 4,
                    h5("Want to trigger an action? Use an action button..."),
                    actionButton(inputId = "action",
                                 label = "Do a thing!")
             ),
             tags$hr()
           )
  ),
  # another tab in the navbarpage 
  tabPanel(title = "Outputs!",
           
           # possible outputs - maybe multiple tabs with different iterations
           # or just rows with iterative interactivity in the cols
           
           h1("Some example outputs"),
           h3("There are many possible outputs, here I focus one just two and the different ways to make them interactive. "),
           # tabsetPanel creates a new set up tabs inside the navbarpage tab 
           tabsetPanel(
             # and just to make things super readable, those tabs are also created with tabPanel 
             # outputs tab 1
             tabPanel("Table Outputs - Basic",
                      # this tab ehas just one row, but I want to add columns 
                      # and columns can only be added within a fluidRow 
                      fluidRow(
                        # output tab 1 column 1 
                        column(width = 6,
                               h2("Table 1 - A Basic Table"),
                               tableOutput(
                                 # the output ID is effectively the variable name for the output object. 
                                 # this is how we link items between the user interface and the backend 
                                 outputId = "simpleTable")
                        ),
                        
                        tags$br(),
                        # output tab 1 column 2 
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
                                 tags$li("It presents data in a readable table format (but why use an app for that?)") 
                               )
                        )
                      )
             ),
             # outputs tab 2
             tabPanel("Table Outputs - Fancy",
                      # as above - one row but i want two columns! 
                      fluidRow(
                        column(width = 6,       
                               h2('Table 2 - A "Shiny" table'),
                               # DTOutput is from the DT package for interactive data tables 
                               DT::DTOutput(outputId = "fancyDataTable")
                        ),
                        column(width = 6,
                               h2("Table 2.1 - Selected rows only"),
                               # a new output, with a new output ID 
                               DT::DTOutput(outputId = "filteredDTOutput")
                        )
                      )
             ),
             # outputs tab 3
             tabPanel("Plot/Table Linked Outputs",
                      # this tab will contains two outputs that appear to talk to each other within the app 
                      # updating in real time 
                      h2("Interactive and Linked Outputs"),
                      # one row with two columns 
                      fluidRow(
                        column(width = 6,
                               DTOutput(outputId = "mtcarstable")
                        ),
                        column(width = 6,
                               # plotlyOutput is from the plotly package 
                               # it make interactive plots from ggplot2 objects 
                               # I'm fairly sure there is a plotly python package also 
                               plotlyOutput(outputId = "plotSheFancy")
                        )
                      )
             )
           )
  ),
  # a new tabe for the navbar page 
  tabPanel(title = "Layouts!",
           # this tab is going to look gross 
           # because there is a lot of nested layouts 
    
    # while we are on layouts, maybe we also want to think of ways to make an app prettier       
    # add a theme browser to the UI to test available themes
    shinythemes::themeSelector(),
    
    # a tabbed layout (inside the nave bar page tab), containing examples of other layouts
    # call this tabsetpanel1
    tabsetPanel(
      tabPanel("Tab Layouts!",
          # tabsetpanel2: a tabset panel inside tabsetpanel1
          tabsetPanel(
              tabPanel("Tab 1",
                       # you could put stuff in here          
              ),
              tabPanel("Tab 2",  
                  # tabsetpanel3: a tabsetpanel inside tabsetpanel2 inside tabsetpanel1     
                  tabsetPanel(
                      tabPanel("Tab 1",             
                               # you could put stuff in here too 
                      ),
                      tabPanel("Tab 2",               
                               # you could put stuff in here too 
                      ),
                      tabPanel("Tab 3",
                          # tabsetpanel4: a tabsetpanel inside tabsetpanel3 inside tabsetpanel2 inside tabsetpanel1
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
      ), # make sure all the brackets are closed!!!! 
      # this is tab 2 of tabsetpanel 1 - containing sidebar layouts!! 
      tabPanel("Sidebar Layouts!",
               # sidebar layouts have two parts:
                # a sidebarPanel() and 
                # a mainPanel()
               # call this sidebarLayout1 
               sidebarLayout(
                 sidebarPanel(
                   # some text 
                   h2("Sidebar panel 1"),
                   h5("Typically side bar panels will contain various inputs"),
                   h5("That relate to the outputs displayed in the main panel"),
                   h5("For Example:"),
                   # and an input 
                   sliderInput(
                     inputId = "slider",
                     label = "A slider input",
                     min = 1,
                     max = 32,
                     value = 10
                   )
                 ),
                 mainPanel(
                   # lets add some outputs to the main panel 
                   h2("Main panel 1"),
                   DT::DTOutput(outputId = "recursiveTab"),
                   # and another side bar layout! 
                   # sidebarlayout 2 
                   sidebarLayout(
                     sidebarPanel(
                       # another input? 
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
                       # another output? 
                       h3("Main panel 2"),
                       DT::DTOutput(outputId = "recursiveTab2"),
                       # oh no, not another sidebar layout !! 
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
           tags$a(href = "https://unstats.un.org/bigdata/task-teams/training/catalog/", "The final training catalouge"), 
           fluidRow(
             column(width = 2, 
                    # empty fluff
             ), 
             column(width = 8,
             div(
               style = "display: inline-block;", 
               img(src = "bigboy.png",
                   height = 800,
                   width = 1150
               )
             )
             ), 
             column(width = 2, 
                    # more empty fluff         
             )
           )
                    
  )
)
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
  
  #### Outputs for the sidebar layout tab 
  
  output$recursiveTab <- renderDT({
    
    numRows <- input$slider 
    datatable(mtcarsNew[1:numRows, c("type", "Weight", "mpg", "Cylinders")])
    
  })
  
  output$recursiveTab2 <- renderDT({
    numRows <- input$slider2
    datatable(mtcarsNew[1:numRows, c("type", "Weight", "mpg", "Cylinders")])
  })
  
  output$recursiveTab3 <- renderDT({
    numRows <- input$slider3
    datatable(mtcarsNew[1:numRows, c("type", "Weight", "mpg", "Cylinders")])    
  })

  output$recursiveTab4 <- renderDT({
    numRows <- input$slider4
    datatable(mtcarsNew[1:numRows, c("type", "Weight", "mpg", "Cylinders")])    
  })
    
}

# Run the application
shinyApp(ui = ui, server = server)
