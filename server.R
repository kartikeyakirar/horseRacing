
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
 
  da.tab.rec<-reactive({
    (dbGetQuery(my_db, paste0("SELECT *  FROM event   WHERE EventName LIKE '",input$chrace,"' ;")))
  })
  # Horse Racing - Bath
  # 2017-05-26 11:10:00
  
  output$choose_timestamp <- renderUI({
    selectInput("chtimestamp", "timestamp", as.list((unique(da.tab.rec()$Event_DateTime))))
  })
  
  
  dat.tab1<-reactive({
    (dbGetQuery(my_db, paste0("SELECT  A.*
                              FROM  odds A 
                              INNER JOIN event AS B 
                              ON A.idevent = B.idevent 
                              WHERE
                              B.EventName = '",input$chrace,"' AND B.Event_DateTime ='",input$chtimestamp,"';")))
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
  
  qwe$lowest_value<-apply(qwe[,-c(1,2)],1,FUN = function(x) {x<-as.numeric(x);x<-x[x>0 & !is.na(x)];min(x)})
  qwe$lowest_value[qwe$lowest_value==Inf]<-0
  qwe
  })
  
  dat.tab1_2<-reactive({
    qwe<-dat.tab1_1()
  qwe1<-reshape2::recast(qwe, Timestamp~bet,measure.var = "lowest_value", id.var = c("Timestamp","bet","lowest_value"))
    qwe1<-zoo::na.locf(zoo::na.locf(qwe1),fromLast=T)
 
  qwe1$overround<-apply(qwe1[,-1],1,FUN = function(x) {x<-as.numeric(x);x<-x[!is.na(x)];sum(as.numeric(x))})
  
  
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
    tmp_new[,-1]<-zoo::na.locf(zoo::na.locf(tmp_new[,-1]),fromLast=T)
    tmp<-tmp_new[seq(1, nrow(tmp_new), 10),]
    p1 <- ggplot(tmp) + 
      geom_line(aes(y = overround, x = Timestamp),col="yellow") +
      ylab('Market Overround') + xlab('Timestamp')+
      ggtitle("Overround")+ 
      ggthemes::theme_gdocs() + scale_colour_gdocs()
    
    ggplotly(p1)
  })
  
  
  
  output$choose_racemak <- renderUI({
    selectInput("chbookmak", "Bookmaker ID", as.list(unique(dat.tab1()$idbookmaker)),multiple = T)
  })
  
  dat.tab2<-reactive({
 tmp<-dat.tab1()
 tmp<-tmp[tmp$idbookmaker %in% input$chbookmak,]
 tmp
     })
  
  
  
  dat.tab2_1<-reactive({
    
    qw<-dat.tab2() 
    options("scipen"=100, "digits"=4)
    qw<-rbind(qw[qw$idbookmaker!=31,],qw[qw$idbookmaker==31 & qw$odds_type=="L" & qw$odds_ctr== 0,])
    timeS1<-data.frame(qw$Timestamp)
   colnames(timeS1)<-"Timestamp"
   
   
   
   
   
   
   
    it<-unique(qw$idbookmaker)
    tpp<-list() 
    for(i in it){
      qww<-qw[qw$idbookmaker == i,]
      qwe<-reshape2::recast(qww, Timestamp~bet,measure.var = "odds", id.var = c("Timestamp","bet","odds"))
      qwe<-merge(qwe,timeS1,all=T)
      qwe<-zoo::na.locf(zoo::na.locf(qwe),fromLast=T)
      q<-apply(qwe[,-1],1,FUN = function(x) {x<-as.numeric(x);x<-x[x>0 & !is.na(x)];sum(x)})
      tp<-data.frame(qwe$Timestamp,unlist(q),stringsAsFactors = F)
      colnames(tp)<-c("Timestamp",i)
      if(length(tpp)==0){
        tpp<-rbind(tpp,tp)
      }else{
        tpp<-merge(tpp,tp,all =T)
      }}
    timeS<-data.frame(dat.tab1_2()$Timestamp,dat.tab1_2()$overround)
    colnames(timeS)<-c("Timestamp","Overround from Graph1")
    
    tpp<-merge(tpp,timeS,all=T)
    tpp[,-1]<-zoo::na.locf(zoo::na.locf(tpp[,-1]),fromLast=T)
    
    tpp
    
  })
  
  
  dat.tab2_2<-reactive({
    
     
    qwe1<-dat.tab2_1()
    tmp<-qwe1[order(as.Date(qwe1$Timestamp)),]
    Timestamp<-seq(from = as.POSIXct(tmp$Timestamp[1]), to = as.POSIXct(tmp$Timestamp[length(tmp$Timestamp)]), by = "sec")
    tobj<-as.data.frame(Timestamp)
    tmp$Timestamp<-as.POSIXct(tmp$Timestamp)
    tmp_new<-merge(tobj,tmp,all.x = T)
    tmp_new[,-1]<-zoo::na.locf(zoo::na.locf(tmp_new[,-1]),fromLast=T)
    tmp_new<-tmp_new[seq(1, nrow(tmp_new), 10),]
    
    tmp_new
    
    
  })
  
  
  
  
  
  output$g.tab2 <- DT::renderDataTable(datatable(dat.tab2(), extensions = 'Buttons',
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
  
  output$g.tab2_1 <- DT::renderDataTable(datatable(dat.tab2_1(), extensions = 'Buttons',
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
  
  output$g.tab2_2<- DT::renderDataTable(datatable(dat.tab2_2(), extensions = 'Buttons',
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
  
  
  output$overroundPlot1 <- renderPlotly({
    # build graph with ggplot syntax
    temp<-dat.tab2_2()
   tmp <- melt(temp, id.vars="Timestamp")
    
     p1 <- ggplot(tmp,aes(Timestamp,value, col=variable)) + 
      geom_line() + geom_point()+
      ylab('Overround') + xlab('Timestamp')+
      ggtitle("Bookmaker comparision with Graph1 Overround")+ 
      ggthemes::theme_gdocs() + scale_colour_gdocs()
    
    ggplotly(p1)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$ch2 <- renderUI({
    selectInput("ch2", "", as.list((unique(dat.tab1()$idbookmaker))),multiple = T)
  })
  
  da.tab.ch2<-reactive({
    tmp<-dat.tab1()
    tmp<-tmp[tmp$idbookmaker %in% input$ch2,]
    tmp
    })
  
  output$ch3 <- renderUI({
    selectInput("ch3", "", as.list((unique(da.tab.ch2()$bet))))
  })
  
  da.tab.ch3<-reactive({
    tmp<-da.tab.ch2()
    qw<-tmp[tmp$bet == input$ch3,]
      options("scipen"=100, "digits"=4)
    
      
      qw<-rbind(qw[qw$idbookmaker!=31,],qw[qw$idbookmaker==31 & qw$odds_type=="L" & qw$odds_ctr== 0,])
      it<-unique(qw$idbookmaker)
      tpp<-list() 
      for(i in it){
        qww<-qw[qw$idbookmaker == i,]
        qwe<-reshape2::recast(qww, Timestamp~bet,measure.var = "odds", id.var = c("Timestamp","bet","odds"))
        qwe<-zoo::na.locf(zoo::na.locf(qwe),fromLast=T)
        colnames(qwe)<-c("Timestamp",paste0("bookmaker_",i))  
        if(length(tpp)==0){
          tpp<-rbind(tpp,qwe)
        }else{
          tpp<-merge(tpp,qwe,all =T)
        }}
      
      tpp[,-1]<-zoo::na.locf(zoo::na.locf(tpp[,-1]),fromLast=T)
      
      tpp
    
    
    
  })
  
  
  output$g<- DT::renderDataTable(datatable(da.tab.ch3(), extensions = 'Buttons',
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
  
  
  output$overroundPlot2 <- renderPlotly({
    # build graph with ggplot syntax
    qwe1<-da.tab.ch3()
    
    tmp<-qwe1[order(as.Date(qwe1$Timestamp)),]
    Timestamp<-seq(from = as.POSIXct(tmp$Timestamp[1]), to = as.POSIXct(tmp$Timestamp[length(tmp$Timestamp)]), by = "sec")
    tobj<-as.data.frame(Timestamp)
    tmp$Timestamp<-as.POSIXct(tmp$Timestamp)
    tmp_new<-merge(tobj,tmp,all.x = T)
    tmp_new[,-1]<-zoo::na.locf(zoo::na.locf(tmp_new[,-1]),fromLast=T)
    tmp_new<-tmp_new[seq(1, nrow(tmp_new), 10),]
    
    
    d <- melt(tmp_new, id.vars="Timestamp")
    d$value<-as.numeric(d$value)
    d$value[(d$value)<0]<-0
    
    p1 <- ggplot(d,aes(Timestamp,value, col=variable)) + 
      geom_line()+
      ylab('Odds') + xlab('Timestamp')+
      ggtitle("Bookmaker comparision ")+ 
      ggthemes::theme_gdocs() + scale_colour_gdocs()
    
    ggplotly(p1)
  })
  
  
  
  
  
}

