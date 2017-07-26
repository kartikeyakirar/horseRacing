#for installing all require package
listofpackages<-c("plotly","readxl","forecast","shinydashboard","shiny","dplyr","plyr","ggplot2","xts","DT","ggthemes","stringr","DataCombine")  
new.pack<-listofpackages[!(listofpackages %in% installed.packages()[,"Package"])]
if(length(new.pack)) 
  install.packages(new.pack,repos='http://cran.us.r-project.org',dependencies = T)
lapply(listofpackages, require, character.only = TRUE)

# set path to working directory (path to folder i sent you). to know your current directory use command
#  getwd()
#setwd("/path/to/current/directory")
setwd("/Users/kartikeya/Desktop/commodity/")
library(shiny)
runApp()
