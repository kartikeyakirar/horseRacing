
listofpackages<-
  c("RNeo4j","tidyverse","stringr","MASS","RColorBrewer","colorspace","shiny","RMySQL","ggplot2","RODBC",
    "pool","dplyr","DBI","dplyr","reshape2","ROAuth","shiny",
    "shinydashboard",
    "ggplot2",
    "plyr",
    "plotly",
    "DT",
    "ggthemes",
    "DataCombine",
    "readxl",
    "forecast"
  )

# installing all the required packages
new.pack<-listofpackages[!(listofpackages %in% installed.packages()[,"Package"])]
if(length(new.pack)) 
  install.packages(new.pack,repos='http://cran.us.r-project.org',dependencies = T)

lapply(listofpackages, require, character.only = TRUE)
#devtools::install_github("rstudio/pool@dplyr-pre-0.7.0-compat")
# devtools::install_github("rstudio/pool")
#####################################################################################
#####################################################################################
my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "betting-copy",
  host = "scraperdb2.cr3nj1yl5iim.eu-west-1.rds.amazonaws.com",
  username = "bki_kartikeya",
  password = "0rQHbch35t7gFK42s33x"
)

