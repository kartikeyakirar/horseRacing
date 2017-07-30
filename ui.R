


# my data
# changing date to categorical data 
#my_data$Year=factor(my_data$Year)

## Preparing sidebar items
sidebar <- dashboardSidebar(
  width = 275,
  sidebarMenu(
    menuItem("Horse Racing Analysis", tabName = "dashbd", icon = icon("dashboard")),
    uiOutput("choose_race"),uiOutput("choose_racemak"),
    menuItem("Data", tabName = "datafile", icon = icon("th"),
             menuSubItem("Dataset for analysis", tabName = "datafile1", icon = icon("table")),
             menuSubItem("Data from odds", tabName = "datf", icon = icon("table")),
             menuSubItem("Other", tabName = "datafile2", icon = icon("table"))),
    menuItem("Visualization", icon = icon("navicon"), tabName = "graphs", 
             menuSubItem("coming soon", tabName = "vsplot", icon = icon("pie-chart")),
             menuSubItem("coming soon", tabName = "vstext", icon = icon("pie-chart")),
             menuItem("coming soon", tabName = "play", icon = icon("bar-chart-o"))
    ),
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
    hr(),
    # email sharing link
    menuItem("Feedback & suggestion", icon = icon("envelope-o"),
             href = "mailto:?shaheed.feb@gmail.com?subject=Feedback on VAS app")
  )
)
## Preparing body items
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashbd",
            fluidRow(
              valueBoxOutput("vbox1", width = 7),
              
            h2("Introduction",  align = "center", style = "font-family: 'times'; color:red"),
            p("Horse Racing Analysis, (more content coming soon).", style = "font-family: 'times'"),
            fluidPage(
              fluidRow(
                column(
                  h2("About this app ...", align = "center", style = "font-family: 'times'; color:red"),
                  p("This app helps you to explore and visualize the Horse racing price dependecy over other  data which is key ingredient of analysis.", 
                    style = "font-family: 'times'"),
                  width = 4,
                  align = "left"
                  
                ),
                column(
                  h2("How to use!", style = "font-family: 'times'; color:red"),
                  p("This app contains Three major sections; Data ,Graphs  and Play . Here we are trying to establish relationship between Commodity price
                      Price and its contributing(ingredient) commodity price .and predict the price of commodity over fluctuation in contributing commodity", 
                    style = "font-family: 'times'"), 
                  p("Section for", strong("data"), "presents the database, which is provided with copying, printing and 
                    downloading options. Section for", strong("visualization"), "shows commparing graphs between commodity prices ", 
                    style = "font-family: 'times'"),
                  width = 8,
                  align = "left"
                ),
                br(),
                br()
                  )
                ),
            br(),
            br(),
            p()
    )),  
    
    
    
    
    tabItem(tabName = "datafile1",
          
            mainPanel(
              tabsetPanel(
                tabPanel("Event Table",
                         box(title = "Race Event Table",
                             width = 12, 
                             DT::dataTableOutput('da.tab'))),
                tabPanel("Book Maker", 
                         box(title = "Bookie Table",
                             width = 12, 
                             DT::dataTableOutput('da.tab1'))
                         , width = "auto")
              )
            )
            
               
    ),
    
    
    tabItem(tabName = "datf",
            
                         box(title = "Data from Odd table",
                             width = 12, 
                             DT::dataTableOutput('da.tab2')), width = "auto")
    ,
    
    tabItem(tabName = "datafile2",
            box(title = "Correlation table",
                width = 12, 
                DT::dataTableOutput('da.tab.cor'))  
    ),
    
    tabItem(tabName = "vstext",
            mainPanel(
              tabsetPanel(
                tabPanel("Graphical",
                         plotlyOutput("vsplot2"), width = "auto"),
                tabPanel("Summary", 
                         verbatimTextOutput("vstext1", placeholder = TRUE)
                         , width = "auto")
              )
            )),
    tabItem(tabName = "vsplot",
            fluidPage(
              plotlyOutput("vsplot1")
            )),
    
    
    
    
    tabItem(tabName = "play",
            mainPanel(
              tabsetPanel(
                tabPanel("Mannual Weights",
                         uiOutput("widget1"),
                         uiOutput("widget2"),
                         br(),
                         tags$head(tags$style(HTML(".small-box {length: 1000px}"))),
                         valueBoxOutput("vbox3", width = 12)),
                tabPanel("percentage Compliment", 
                         fluidRow( column(6,uiOutput("widget3")),
                                   column(5,valueBoxOutput("vbox_1", width = 28))),
                         fluidRow( 
                           plotlyOutput("vsplot_1")
                           
                         )))))
    
    )
    
     
              
            )
## Putting them together into a dashboardPage

ui <- dashboardPage( 
  skin="blue",
  # add this -> navbarMenu()
  dashboardHeader(
    title="Effect on Commodity ",
    titleWidth = 250
    #Facebook Sharing
    
  ),
  sidebar,
  body
)