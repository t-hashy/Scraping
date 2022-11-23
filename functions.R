## ==== BASE SETTINGS ====
# Libraries ----

## Linux: need to run 2 commands below in a consol
# sudo apt-get install libcurl4-openssl-dev
# sudo apt-get install libxml2-dev

## Install packages
# install.packages("rvest") 
# install.packages('stringi', configure.args = '--disable-cxx11')
# install.packages("tidyverse")
# install.packages("Hmisc")
# install.packages("igraph")
# install.packages("tidytext")
# install.packages("pastecs")
# install.packages("psych")
# install.packages("wordcloud")
# install.packages("GGally")
# install.packages("RMeCab", repos = "https://rmecab.jp/R", type = "source") 
# install.packages("formattable" , dependencies = T)
# install.packages("wordcloud", dependencies = T)
# install.packages("rpart.plot")
# install.packages("ROSE")

# ## Libraries
library(rvest) # Scraping
library(xml2)
library(tibble)
library(tidyverse) # String_extraction (using when shaping data), ggplot, etc..
library(Hmisc) # Describing data
library(pastecs) # Describe basic satats
library(psych) # Basic stats
library(wordcloud) #Text mining
library(igraph) # Show text mining outputs
library(tidytext) # Manage text values
library(stringr)
library(GGally)
library(ggplot2)
library(RMeCab) # Text mining
library(dplyr) # Filtering and else
library(formattable) # Table visualization
library(wordcloud) # Create wordcloud
library(data.table)
library(jsonlite)
library(purrr)
library(progress)
library(dplyr)
library(rpart) ## Decision tree
library(rpart.plot) ## Decision tree
library(caret) #Create confusion matrix in machine learning
library(unbalanced)
library(ROSE)


# Global variables ----
character_code <-  "shift-jis"
dr_path <- getwd() # Directory path

##
# Functions ----
check_data <- function(df) {
  head <- head(df, n = 2)
  str <- glimpse(df)
  summary <- summary(df)
  list <- list(str = str, summary = summary, head = head)
  return(list)
}

convert_datetime_into_str <- function(datetime = Sys.time()) {
  dt <- datetime %>%
    gsub("-", "", .) %>% # Replace a blank between date and time with an under bar
    gsub(" ", "-", .) %>%
    gsub(":", "", .) %>% # Replace ":" with "-"
    as.character()
  return(dt)
}
df_to_csv <- function(f_title, df) {
  now <- Sys.time() %>% # Get time of now
    gsub("-", "", .) %>% # Replace a blank between date and time with an under bar
    gsub(" ", "-", .) %>%
    gsub(":", "", .) %>% # Replace ":" with "-"
    as.character()
  f_name <- paste(f_title, "_", now, ".csv", sep = "" )
  write.csv(x=df, file=f_name, fileEncoding = character_code)
  return(paste("EXPORTED:", f_name))
}

new_pb <- function(total)  {
  progress_bar$new(
    format = "(:spin) [:bar] :percent [Elaspsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = total,
    complete = "=",
    incomplete = "-",
    current = ">",
    clear = FALSE,
    width = 100
  )
}

create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <-  1:total_row
  if(train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample,])
  }
}

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$hasJAS, predict_unseen)
  accuracy_test <- sum(diag(table_mat)) / sum(table_mat)
  return(accuracy_test)
}
##  
