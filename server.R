
# Reading data set
source("script.R")

race_name<-dbGetQuery(my_db, "SELECT DISTINCT EventName
FROM event;")

my_data<-dbGetQuery(my_db, "SELECT DISTINCT *
FROM event LIMIT 0,10;")

bookmaker_name<-dbGetQuery(my_db, "SELECT DISTINCT name
FROM bookmaker;")


server <- function(input, output) {
  
  output$choose_race <- renderUI({
    selectInput("chrace", "Race Event", as.list((race_name)))
  })
  
  output$choose_racemak <- renderUI({
    selectInput("chbookmak", "bookmaker_name", as.list((bookmaker_name)))
  })
  
  da.tab.rec<-reactive({
    (dbGetQuery(my_db, paste0("SELECT *  FROM event   WHERE EventName LIKE '",input$chrace,"' ;")))
  })
  
  
  
  
  output$da.tab <- DT::renderDataTable(datatable(da.tab.rec(), extensions = 'Buttons',
                                                 style = "bootstrap",
                                                 filter = list(position = 'top', clear = T, plain = F),
                                                 options = list(pageLength = 3000,scrollX = T, dom = 'Bfrtip', 
                                                                buttons = 
                                                                  list('copy', 'print', list(
                                                                    extend = 'collection',
                                                                    buttons = c('csv', 'excel', 'pdf'), 
                                                                    text = 'Download')
                                                                  )
                                                 )
  )
  )
  
  
  da.tab.rec1<-reactive({
    (dbGetQuery(my_db, paste0("SELECT *  FROM bookmaker   WHERE name LIKE '",input$chbookmak,"' ;")))
  })
  
  output$da.tab1 <- DT::renderDataTable(datatable(da.tab.rec1(), extensions = 'Buttons',
                                                 style = "bootstrap",
                                                 filter = list(position = 'top', clear = T, plain = F),
                                                 options = list(pageLength = 3000,scrollX = T, dom = 'Bfrtip', 
                                                                buttons = 
                                                                  list('copy', 'print', list(
                                                                    extend = 'collection',
                                                                    buttons = c('csv', 'excel', 'pdf'), 
                                                                    text = 'Download')
                                                                  )
                                                 )
  )
  )
  
  
  
  da.tab.rec2<-reactive({
    (dbGetQuery(my_db, paste0("SELECT  A.*
                    FROM  odds A 
                              INNER JOIN event AS B 
                              ON A.idevent = B.idevent 
                              INNER JOIN bookmaker AS C 
                              ON A.idbookmaker =C.idbookmaker 
                              WHERE
                              B.EventName = '",input$chrace,"' AND C.name ='",input$chbookmak,"';")))
  })
  
  
  
  
  output$da.tab2 <- DT::renderDataTable(datatable(da.tab.rec2(), extensions = 'Buttons',
                                                 style = "bootstrap",
                                                 filter = list(position = 'top', clear = T, plain = F),
                                                 options = list(pageLength = 3000,scrollX = T, dom = 'Bfrtip', 
                                                                buttons = 
                                                                  list('copy', 'print', list(
                                                                    extend = 'collection',
                                                                    buttons = c('csv', 'excel', 'pdf'), 
                                                                    text = 'Download')
                                                                  )
                                                 )
  )
  )
  
}

