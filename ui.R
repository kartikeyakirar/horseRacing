


# my data
# changing date to categorical data 
#my_data$Year=factor(my_data$Year)

## Preparing sidebar items
sidebar <- dashboardSidebar(
  width = 275,
  sidebarMenu(
    menuItem("Horse Racing Analysis", tabName = "dashbd", icon = icon("dashboard")),
    uiOutput("choose_race"),uiOutput("choose_timestamp"),
    menuItem("Over Round Graph", tabName = "datafile", icon = icon("th"),
             menuSubItem("Dataset for analysis", tabName = "datafile1", icon = icon("table")),
             menuSubItem("OverRound Graph", tabName = "datf", icon = icon("bar-chart-o"))),
            
    menuItem("Bookmaker Vs Market", icon = icon("navicon"), tabName = "graphs", 
             menuSubItem("Dataset for analysis", tabName = "datafile2", icon = icon("table")),
             menuSubItem("Bookmaker Comaprision", tabName = "datf1", icon = icon("bar-chart-o"))
             
    ),
    menuItem("Bookmaker Plot", icon = icon("navicon"), tabName = "graphs", 
             
             menuSubItem("Bookmaker Plot against market", tabName = "datf3", icon = icon("bar-chart-o")),
             menuSubItem("Bookmaker Plot Data", tabName = "datf4", icon = icon("bar-chart-o"))
             
             
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
                             DT::dataTableOutput('g.tab1'))),
                tabPanel("BookMaker Values", 
                         box(title = "Bookie Table",
                             width = 12, 
                             DT::dataTableOutput('g.tab1_1'))
                         , width = "auto"),
                tabPanel("OverRound", 
                         box(title = "overRound Table",
                             width = 12, 
                             DT::dataTableOutput('g.tab1_2'))
                         , width = "auto")
              )
            )
            
               
    ),
    
    
    tabItem(tabName = "datf",
            
            fluidPage(
              plotlyOutput("overroundPlot", height = "900px")
            )        
            
    )
    ,
    tabItem(tabName = "datf1",
            
            fluidPage(
              plotlyOutput("overroundPlot1", height = "900px")
            )        
            
    )
    ,
    
    tabItem(tabName = "datafile2",fluidRow(column(width=6,uiOutput("choose_racemak"))),
            fluidRow(
              mainPanel(
                tabsetPanel(
                  tabPanel("Event Table",
                           box(title = "Race Event Table",
                               width = 12, 
                               DT::dataTableOutput('g.tab2'))),
                  tabPanel("BookMaker Values", 
                           box(title = "Bookie Table",
                               width = 12, 
                               DT::dataTableOutput('g.tab2_1'))
                           , width = "auto"),
                  tabPanel("OverRound", 
                           box(title = "overRound Table",
                               width = 12, 
                               DT::dataTableOutput('g.tab2_2'))
                           , width = "auto")
                )
              ))
            
            
    ),
    
    tabItem(tabName = "datf3",
            fluidRow(
              
              
              box(
                title = "Select Event", status = "primary",solidHeader = TRUE,width = 4,height = 115,
                collapsible = TRUE,
                uiOutput("ch1")
                #  selectInput("ver", "", choices = as.list(c("All",levels(locationHPHC$Vertical))), width = 200,selected = "All")
              ),
              box(
                title = "Select Multiple Bookmaker",status = "primary",solidHeader = TRUE,width = 4,height = 115,
                collapsible = TRUE,
                uiOutput("ch2")
                #  selectInput("ver1", "", choices = as.list(c("All",levels(locationHPHC$Account))),selected = "All")
              ),
              box(
                title = "Select Horse",status = "primary",solidHeader = TRUE,width = 4,height = 115,
                collapsible = TRUE,
                uiOutput("ch3")
                # selectInput("ver2", "", choices = as.list(c("All",levels(locationHPHC$Site))),selected = "All")
                
              )
              
              
            ),
            fluidRow( (
              plotlyOutput("overroundPlot2", height = "900px")
            ))
            
    ),
    
    tabItem(tabName = "datf4",
            tabsetPanel(
              tabPanel("Event Table",
                       box(title = "Race Event Table",
                           width = 12, 
                           DT::dataTableOutput('g'))))
            
    )
    
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