
# Reading data set
my_data=read_excel("data/data_analysis.xlsx")


items1<-levels(as.factor(unique(my_data$Item)))

listHolder<-lapply(items1,function(x){tmp<-my_data[my_data$Item==x,]
aa<-cor(tmp$ItemPrice,tmp$Com1Price)                  
bb<-cor(tmp$ItemPrice,tmp$Com2Price)                  
aa1<-summary(lm(formula = ItemPrice ~ Com1Price , data = tmp))$r.squared
bb1<-summary(lm(formula = ItemPrice ~ Com2Price, data = tmp))$r.squared                  
cc<-summary(lm(formula = ItemPrice ~ Com1Price + Com2Price, data = tmp))$r.squared                  
c(round(aa,2),round(bb,2),round(aa1,2),round(bb1,2),round(cc,2))
})

dd  <-  t(as.data.frame(matrix(unlist(listHolder), nrow=length(unlist(listHolder[1])))))
temp<-cbind(items1,dd)
colnames(temp)<-c("Commodity","Correlation(comm1)","Correlation(comm1)","R.square(comm1)","R.square(comm2)","R.square(both)")


list_items<-as.character(levels(as.factor(my_data$ASItemNo)))
mylist<-vector()


datasq<-data.frame(matrix(NA,nrow=101,ncol=6))
colnames(datasq)<-list_items


for(i in 1:length(list_items)){
  dat<-my_data[my_data$ASItemNo==list_items[i],]
  list2<-list()
  for(j in seq(0,1,0.01)){
    dat$x<-dat$Com2Price *(1-j) + dat$Com1Price*j
    list2<-cbind(list2,round(summary(lm(formula =ItemPrice ~ x , data = dat))$r.squared,3))      }
  datasq[,i]<-unlist(as.list(list2))    
}

tmpz<-datasq



# 
# z<-lapply(list_items,function(x){
#   dat<-my_data[my_data$ASItemNo==x,]
#   list1<-0
#   for(y in seq(0,1,0.01)){
#     dat$x<-dat$Com2Price *(1-y) + dat$Com1Price*y
#     list1[1+y*100]<-summary(lm(formula =ItemPrice ~ x , data = dat))$r.squared               
#     
#   }
#   mylist<-(rbind(mylist,unlist(list1)))
# })
# 
# tmpz<-plyr::ldply(z, data.frame)


datasq<-data.frame(matrix(NA,nrow=101,ncol=6))
colnames(datasq)<-list_items


for(i in 1:length(list_items)){
  dat<-my_data[my_data$ASItemNo==list_items[i],]
  list2<-list()
  for(j in seq(0,1,0.01)){
    dat$x<-dat$Com2Price *(1-j) + dat$Com1Price*j
    list2<-cbind(list2,round(cor(dat$ItemPrice,dat$x),4))      }
  datasq[,i]<-unlist(as.list(list2))    
}

tmpz1<-datasq

rownames(tmpz)<-paste0(seq(0,100,1),"%-to-%",seq(100,0,-1))
#rownames(tmpz)<-list_items
rownames(tmpz1)<-paste0(seq(0,100,1),"%-to-%",seq(100,0,-1))
#rownames(tmpz1)<-list_items




server <- function(input, output) {
  
  output$choose_commodity <- renderUI({
    selectInput("chcomm", "Item Number", as.list(levels(as.factor(my_data$ASItemNo))))
  })
  
  
  dat_set1<-reactive({
    my_data[my_data$ASItemNo==input$chcomm,]
  })
  
  
  
  ## stuffs for dashboard tab
  output$vbox1 <- renderValueBox({ 
    valueBox(
      paste0("Total number of Commodity:",length(levels(as.factor(my_data$Item)))),
      input$count,
      color = "orange",
      icon = icon("empire"))
  })
  ## stuffs for data tab
  # data table output
  
  output$da.tab.rsq <- DT::renderDataTable(datatable(t(tmpz), extensions = 'Buttons',
                                                 style = "bootstrap",
                                                 filter = list(position = 'top', clear = T, plain = F),
                                                 options = list(pageLength = 120,scrollX = T, dom = 'Bfrtip', 
                                                                buttons = 
                                                                  list('copy', 'print', list(
                                                                    extend = 'collection',
                                                                    buttons = c('csv', 'excel', 'pdf'), 
                                                                    text = 'Download')
                                                                  )
                                                 )
  )
  )
  
  
  
  output$da.tab.corr <- DT::renderDataTable(datatable(t(tmpz1), extensions = 'Buttons',
                                                 style = "bootstrap",
                                                 filter = list(position = 'top', clear = T, plain = F),
                                                 options = list(pageLength = 120,scrollX = T, dom = 'Bfrtip', 
                                                                buttons = 
                                                                  list('copy', 'print', list(
                                                                    extend = 'collection',
                                                                    buttons = c('csv', 'excel', 'pdf'), 
                                                                    text = 'Download')
                                                                  )
                                                 )
  )
  )
  output$da.tab <- DT::renderDataTable(datatable(my_data, extensions = 'Buttons',
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
  
  # data table output
  output$da.tab.cor <- DT::renderDataTable(datatable(temp, extensions = 'Buttons',
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

  
  ## stuffs for data explorer
  # for tab: Killed
  
  output$vsplot1 <- renderPlotly({
    # build graph with ggplot syntax
    p1 <- ggplot() + 
      geom_smooth(data = dat_set1(), aes(y = ItemPrice, x = Dte, color = "Item Price")) +
      geom_smooth(data = dat_set1(), aes(y = Com1Price, x = Dte, color = "Commodity1"))  + 
      geom_smooth(data = dat_set1(), aes(y = Com2Price, x = Dte, color = "Commodity2"))  + 
      ylab('Price') + xlab('Dte')+
      ggtitle("observing pattern of Item price with different commodity price")+ 
      ggthemes::theme_gdocs() + scale_colour_gdocs()
    
    ggplotly(p1)
  })
  
  
  
  
  output$vsplot2 <- renderPlotly({
    # build graph with ggplot syntax
    tmp<-dat_set1()
    
    fit<-lm(formula = ItemPrice ~ Com1Price + Com2Price, data = tmp)
  newdat<-cbind(tmp$Dte,tmp$ItemPrice,as.data.frame(predict(fit)))
  colnames(newdat)<-c("Date","Actual_Price","Predicted_Price")
  p1 <- ggplot() + 
      geom_smooth(data = newdat, aes(y = Actual_Price, x = Date, color = "Actual_Price")) +
      geom_smooth(data =newdat, aes(y = Predicted_Price, x = Date, color = "Predicted_Price"))  + 
      ylab('Price') + xlab('Date')+
      ggtitle("Price Predicted from commdity1 and commodity2")+ 
      ggthemes::theme_gdocs() + scale_colour_gdocs()
    
    ggplotly(p1)
  })
  
  
  #
  output$vstext1 <- renderPrint({  tmp<-dat_set1()
  
  fit<-lm(formula = ItemPrice ~ Com1Price + Com2Price, data = tmp)
summary(fit) })
  
  output$widget1<-renderUI({
    numericInput("num1", label = h3(dat_set1()$Commodity1[1]), value = 0)
  })
  output$widget2<-renderUI({
    numericInput("num2", label = h3(dat_set1()$Commodity2[1]), value = 0)
  })
  output$vbox3 <- renderValueBox({ 
    tmp<-dat_set1()
    
    fit<-lm(formula = ItemPrice ~ Com1Price + Com2Price, data = tmp)
  value<-  predict(fit,newdata =as.data.frame( t(c("ItemPrice"=0,"Com1Price"=input$num1,"Com2Price"=input$num2))))
    valueBox(
      paste0("Estimated Price:",round(value,2)),
      input$count,
      color = "olive",
      icon = icon("empire"))
  })
  
  output$widget3<-renderUI({
    numericInput("num3", label = h3(dat_set1()$Commodity1[1]), value = 0,min = 0, max = 100)
  })
  
  output$vbox_1 <- renderValueBox({ 
    tmp<-dat_set1()$Commodity2[1]
    
    valueBox(
      paste0(tmp,"-->",(100-input$num3)," %"),
      input$count)
  })
  
  
  output$vsplot_1 <- renderPlotly({
    # build graph with ggplot syntax
    tmp<-dat_set1()
    tmp$list_new<-input$num3*tmp$Com1Price + (100-input$num3)*tmp$Com2Price
    fit<-lm(formula = ItemPrice ~ list_new, data = tmp)
    newdat<-cbind(tmp$Dte,tmp$ItemPrice,as.data.frame(predict(fit)))
    colnames(newdat)<-c("Date","Actual_Price","Predicted_Price")
    p1 <- ggplot() + 
      geom_smooth(data = newdat, aes(y = Actual_Price, x = Date, color = "Actual_Price")) +
      geom_smooth(data =newdat, aes(y = Predicted_Price, x = Date, color = "Predicted_Price"))  + 
      ylab('Price') + xlab('Date')+
      ggtitle("Price Predicted from commdity1 and commodity2 with given Percentage weights")+ 
      ggthemes::theme_gdocs() + scale_colour_gdocs()
    
    ggplotly(p1)
  })
  
}

