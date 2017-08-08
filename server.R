
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
  # Horse Racing - Bath
  # 2017-05-26 11:10:00
  
  output$choose_timestamp <- renderUI({
    selectInput("chtimestamp", "timestamp", as.list((unique(da.tab.rec()$start_scraping))))
  })
  
  da.tab.rec1<-reactive({
    (dbGetQuery(my_db, paste0("SELECT *  FROM bookmaker   WHERE name LIKE '",input$chbookmak,"' ;")))
  })
  
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
  
  
  dat.tab1<-reactive({
    (dbGetQuery(my_db, paste0("SELECT  A.*
                              FROM  odds A 
                              INNER JOIN event AS B 
                              ON A.idevent = B.idevent 
                              WHERE
                              B.EventName = '",input$chrace,"' AND B.start_scraping ='",input$chtimestamp,"';")))
  })
  
  
  dat.tab1_1<-reactive({
    
  qw<-dat.tab1()
  options("scipen"=100, "digits"=4)
  qw<-rbind(qw[qw$idbookmaker!=31,],qw[qw$idbookmaker==31 & qw$odds_type=="L" & qw$odds_ctr== 0,])
  qwe<-reshape2::recast(qw, Timestamp+bet~idbookmaker,measure.var = "odds", id.var = c("Timestamp","bet","idbookmaker","odds"))
  
  it<-unique(qwe$bet)
  for(i in 1:length(it)){
    qwe[qwe$bet==it[i],]<-zoo::na.locf(zoo::na.locf(qwe[qwe$bet==it[i],]),fromLast=T)
  }
  
  qwe$lowest_value<-apply(qwe[,-c(1,2)],1,FUN = function(x) {min(x[!is.na(x)])})
  qwe<-qwe[qwe$lowest_value>=0,]
  qwe
  })
  
  dat.tab1_2<-reactive({
    qwe<-dat.tab1_1()
  qwe1<-reshape2::recast(qwe, Timestamp~bet,measure.var = "lowest_value", id.var = c("Timestamp","bet","lowest_value"))
    qwe1<-zoo::na.locf(zoo::na.locf(qwe1),fromLast=T)
 
  qwe1$overround<-apply(qwe1[,-1],1,FUN = function(x) {sum(as.numeric(x))})
  qwe1<-qwe1[qwe1$overround>=0,]
  
  qwe1
  })
  
  
  
  output$g.tab1 <- DT::renderDataTable(datatable(dat.tab1(), extensions = 'Buttons',
                                                 style = "bootstrap",
                                                 filter = list(position = 'top', clear = T, plain = F),
                                                 options = list(pageLength = 100,scrollX = T, dom = 'Bfrtip', 
                                                                buttons = 
                                                                  list('copy', 'print', list(
                                                                    extend = 'collection',
                                                                    buttons = c('csv', 'excel', 'pdf'), 
                                                                    text = 'Download')
                                                                  )
                                                 )
  )
  )
  
  output$g.tab1_1 <- DT::renderDataTable(datatable(dat.tab1_1(), extensions = 'Buttons',
                                                 style = "bootstrap",
                                                 filter = list(position = 'top', clear = T, plain = F),
                                                 options = list(pageLength = 100,scrollX = T, dom = 'Bfrtip', 
                                                                buttons = 
                                                                  list('copy', 'print', list(
                                                                    extend = 'collection',
                                                                    buttons = c('csv', 'excel', 'pdf'), 
                                                                    text = 'Download')
                                                                  )
                                                 )
  )
  )
  
  output$g.tab1_2<- DT::renderDataTable(datatable(dat.tab1_2(), extensions = 'Buttons',
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
  
  
  
  
  
  output$overroundPlot <- renderPlotly({
    # build graph with ggplot syntax
    qwe1<-dat.tab1_2()
    tmp<-qwe1[order(as.Date(qwe1$Timestamp)),]
    Timestamp<-seq(from = as.POSIXct(tmp$Timestamp[1]), to = as.POSIXct(tmp$Timestamp[length(tmp$Timestamp)]), by = "sec")
    tobj<-as.data.frame(Timestamp)
    tmp$Timestamp<-as.POSIXct(tmp$Timestamp)
    tmp_new<-merge(tobj,tmp,all.x = T)[,c("Timestamp","overround")]
    yp<-ts(tmp_new$overround)
    tmp_new$OverRound<-imputeTS::na.kalman(yp)
    tmp<-tmp_new[seq(1, nrow(tmp_new), 10),]
    p1 <- ggplot(tmp) + 
      geom_line(aes(y = OverRound, x = Timestamp),col="yellow") +
      ylab('Market Overround') + xlab('Timestamp')+
      ggtitle("Overround")+ 
      ggthemes::theme_gdocs() + scale_colour_gdocs()
    
    ggplotly(p1)
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
  
  
  
  
  
  da.tab.rec3<-reactive({
    (dbGetQuery(my_db, paste0("SELECT  A.*
                              FROM  odds A 
                              INNER JOIN event AS B 
                              ON A.idevent = B.idevent 
                              WHERE
                              B.EventName = '",input$chrace,"' AND B.start_scraping ='",input$chtimestamp,"';")))
  })
  
  
  
  
  
  
  
}

