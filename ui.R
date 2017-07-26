library(shiny)
library(shinydashboard)
library(ggplot2)
library(plyr)
library(plotly)
library(DT)
library(ggthemes)
library(DataCombine)
library(readxl)
library(forecast)

# my data
my_data=read_excel("data/data_analysis.xlsx")
# changing date to categorical data 
#my_data$Year=factor(my_data$Year)

## Preparing sidebar items
sidebar <- dashboardSidebar(
  width = 275,
  sidebarMenu(
    menuItem("Horse Racing Analysis", tabName = "dashbd", icon = icon("dashboard")),
    uiOutput("choose_commodity"),
    menuItem("Data", tabName = "datafile", icon = icon("th"),
             menuSubItem("Dataset for analysis", tabName = "datafile1", icon = icon("table")),
             menuSubItem("Variation in Commodity %", tabName = "datf", icon = icon("table")),
             menuSubItem("Correlation Among commodity and Item", tabName = "datafile2", icon = icon("table"))),
    menuItem("Visualization", icon = icon("navicon"), tabName = "graphs", 
             menuSubItem("Compare commodity Price", tabName = "vsplot", icon = icon("pie-chart")),
             menuSubItem("Predictor Analysis", tabName = "vstext", icon = icon("pie-chart")),
             menuItem("Play", tabName = "play", icon = icon("bar-chart-o"))
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
            p("Commodities are extremely important as they are essential factors in the production of other goods.
             A wide of array of commodities exist, including coffee, wheat, barley, gold and oil.
              Even orange juice is traded as a commodity. These commodities are traded constantly on commodity exchanges around the world,Since commodities are traded on exchanges, their prices are not set by a single individual or entity. On the exchanges, commodities are traded via futures contracts.
              These contracts obligate the holder to buy or sell a commodity at a predetermined price on a delivery date in the future. 
              Not all futures contracts are the same - their specifics will differ depending on the respective commodity being traded.", style = "font-family: 'times'"),
            fluidPage(
              fluidRow(
                column(
                  h2("About this app ...", align = "center", style = "font-family: 'times'; color:red"),
                  p("This app helps you to explore and visualize the Commodity price dependecy over other commodity data which is key ingredient of our commodity.", 
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
            box(title = "Item data with contributing commodity",
                width = 12, 
                DT::dataTableOutput('da.tab'))  
    ),
    
    
    tabItem(tabName = "datf",
            mainPanel(
              tabsetPanel(
                tabPanel("RSqure value",
                         box(title = "Variation in percentage of available commodity shown by row name",
                             width = 12, 
                             DT::dataTableOutput('da.tab.rsq')), width = "auto"),
                tabPanel("Correlation calue", 
                         box(title = "Variation in percentage of available commodity shown by row name",
                             width = 12, 
                             DT::dataTableOutput('da.tab.corr'))
                         , width = "auto")
              )
            )  
    ),
    
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