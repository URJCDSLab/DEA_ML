library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(markdown)

   sidebar <-  dashboardSidebar(
   sidebarMenu(id = "sidebar",
                HTML(paste0(
                  "<br>",
                  "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='DEA2.png' width = '186'>",
                  "<br>"
                )),
                menuItem("Home", tabName = "home", icon = icon("home"), selected = TRUE),
                # menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = FALSE),
                menuItem("File", icon = icon("file"), tabName = "file", selected = FALSE),
                menuItem("Transform", icon = icon("wrench"),tabName = "transform", selected = FALSE),
               menuItem("Analysis", tabName = "analysis", icon = icon("chart-bar"), selected = FALSE),
               menuItem("Model", tabName = "model", icon = icon("chart-bar"), selected = FALSE),
                menuItem("About", tabName = "about", icon = icon("question"), selected = FALSE)
    )
)
  
  body <- dashboardBody(
   # conditionalPanel(
    tabItems(
      tabItem(tabName = "home",
              uiOutput("doc_to_display")
      ),
      tabItem(tabName = "file",
              h2("Reading the data set"),
              box(width = NULL, status = "primary", solidHeader = TRUE, title="Data",
                  fileInput('file1', 'CSV File',
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                  #csvFileInput("datafile", "User data (.csv format)"),
              checkboxInput('header', 'Header', TRUE),
              radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ','),
              radioButtons('quote', 'Quote',
                         c(None='',
                           'Double Quote'='"',
                           'Single Quote'="'"),
                         '"'),
              br(),
              tableOutput("table1"),
              tabPanel(
                "Data Summary",
                box(withSpinner(verbatimTextOutput("Summ_old")), width = 6),
                box(withSpinner(verbatimTextOutput("Summ")), width = 6)
              ),
              )),
      tabItem(tabName = "transform",
              h2("Cox transformation"),
              pageWithSidebar(
                headerPanel(''),
                sidebarPanel(                            
                  uiOutput("SelectX"),
                  #selectInput('xcol', 'Variable:',""),
                  sliderInput("Lambda",
                              "Lambda",
                              value = 1,
                              min = 0.0,
                              max = 2.0,round=TRUE,step=.01),
                  br(),
                  actionButton("addButton", strong("Add!")),
                  width = 3            
             
                ),
              mainPanel(
                plotOutput('MyPlot5'),
                tableOutput("table3")
                #,                plotOutput('MyPlot2')
              )
              )),
      tabItem(tabName = "analysis",
              h2("Correlation Analysis"),
              pageWithSidebar(
                headerPanel(''),
                sidebarPanel(                            
                  uiOutput("SelectY1"),
                  uiOutput("SelectY2"),
                                #selectInput('y1col', 'Variable:',""),
                  #selectInput('y2col', 'Variable:',""),
                ),
                mainPanel(
                  plotOutput('MyPlot3')
                )
              )
              ),         
      tabItem(tabName = "model",
              h2("Model"),
              box(
              uiOutput("SelectXX"),
              solidHeader = TRUE,
              width = "3",
              status = "primary",
              title = "X variables"
              ),
              #box(
              #  selectInput(
              #    "SelectX",
              #    label = "Select variables:",
              #    choices = names(data()),
              #    multiple = TRUE
              #  ),
              #  solidHeader = TRUE,
              #  width = "3",
              #  status = "primary",
              #  title = "X variables"
              #),

              box(
                #selectInput("SelectY", label = "Select variable to predict:", choices = names(data())),
                uiOutput("SelectYY"),
                solidHeader = TRUE,
                width = "3",
                status = "primary",
                title = "Y variable"
              ),
              box(
                sliderInput(
                  "Slider",
                  label = h3(" "),
                  min = 0,
                  max = 100,
                  value = 70
                ),
                textOutput("cntTrain"),
                textOutput("cntTest"),
                solidHeader = TRUE,
                width = "3",
                status = "primary",
                title = "Train/Test Split %",
                br()
              ),
              tabPanel(
                "Model",
                box(
                  withSpinner(verbatimTextOutput("Model")),
                  width = 6,
                  title = "Model Summary"
                ),
                box(
                  withSpinner(verbatimTextOutput("ImpVar")),
                  width = 5,
                  title = "Variable Importance"
                )
              )
              ),
         tabItem(tabName = "about",
             includeMarkdown("www/about.md")
     )
    )
  #)
  )
  
  dashboardPage(
    dashboardHeader(title = "DSLAB"),
    sidebar,
    body
  )



