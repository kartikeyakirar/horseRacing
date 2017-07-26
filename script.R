setwd("/Users/kartikeya kirar/Desktop/ian/")
listofpackages<-
  c("RNeo4j","tidyverse","stringr","MASS","RColorBrewer","colorspace","shiny","RMySQL","ggplot2","RODBC",
    "pool","dplyr","DBI","dplyr","reshape2","ROAuth"
  )

# installing all the required packages
new.pack<-listofpackages[!(listofpackages %in% installed.packages()[,"Package"])]
if(length(new.pack)) 
  install.packages(new.pack,repos='http://cran.us.r-project.org',dependencies = T)

lapply(listofpackages, require, character.only = TRUE)
devtools::install_github("rstudio/pool@dplyr-pre-0.7.0-compat")
devtools::install_github("rstudio/pool")
#####################################################################################
#####################################################################################
my_db <- dbPool(
  RMySQL::MySQL(), 
  dbname = "betting-copy",
  host = "scraperdb2.cr3nj1yl5iim.eu-west-1.rds.amazonaws.com",
  username = "bki_kartikeya",
  password = "0rQHbch35t7gFK42s33x"
)

my_db %>% tbl("odds") %>% head(5)
dbGetQuery(my_db, "SELECT * FROM event LIMIT 5;")
dbGetQuery(my_db, "SELECT DISTINCT EventName
FROM event;")