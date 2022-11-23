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
# library(rvest) # Scraping
# library(xml2)
# library(tibble)
# library(tidyverse) # String_extraction (using when shaping data), ggplot, etc..
# library(Hmisc) # Describing data
# library(pastecs) # Describe basic satats
# library(psych) # Basic stats
# library(wordcloud) #Text mining
# library(igraph) # Show text mining outputs
# library(tidytext) # Manage text values
# library(stringr)
# library(GGally)
# library(ggplot2)
# library(RMeCab) # Text mining
# library(dplyr) # Filtering and else
# library(formattable) # Table visualization
# library(wordcloud) # Create wordcloud
# library(data.table)
# library(jsonlite)
# library(purrr)
# library(progress)
# library(dplyr)
# library(rpart) ## Decision tree
# library(rpart.plot) ## Decision tree
# library(caret) #Create confusion matrix in machine learning
# library(unbalanced)
# library(ROSE)
# 

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

## ==== SCRAPING ====
# Set Scraping basics ----
url_home <- "https://www.tabechoku.com"
url_products <- paste(url_home, "/products", sep = "")
url_products_main <- paste(url_products, "/", sep = "")
url_categories_main <- paste(url_products, "/categories/", sep = "")
vegetables_categoryNum <- 1
url_vegetables <- paste(url_categories_main, vegitables_categoryNum, sep = "")
##

# Scraping Tabechoku products pages----

#Get max page number

productsNum <- read_html(url_products_main) %>%
  html_nodes(xpath = "//p[@class='h3']") %>%
  html_text() %>%
  str_extract(., "[0-9.]+,?[0-9.]+") %>%
  gsub(",","", .) %>%
  as.numeric()
Sys.sleep(sample(2:5, 1)) # Stop 10 seconds after rolling the site
max_productsPageNum <- ceiling(productsNum/100)

#Set initial values
df_products <- NULL


# Scraping all products page

pb <- new_pb(max_productsPageNum)

for(pageNum in 1:max_productsPageNum) {
  # Show progress and remaining time
  pb$tick()
  
  # Set url for search
  url_temp <- paste(url_products , "?page=", pageNum  ,sep = "") # URL with parameter+page_number
  
  # Scraping
  tryCatch(
    {
      # Get HTML value
      html <- read_html(url_temp) # Get html
      
      # Pause scraping
      Sys.sleep(sample(2:5, 1))
      
      # Get product IDs
      item_wrap_script <- html %>%
        html_nodes(".item-wrap")
      product_ids <- item_wrap_script %>%
        str_extract(.,'data-product-id="[0-9.]+"') %>%
        str_extract(., "[0-9.]+") %>%
        as.numeric() %>%
        as.data.frame() %>%
        setNames("product_id")
      
      # Get product names
      products <- html %>%
        html_nodes(".product-name h4") %>%
        html_text() %>%
        gsub(" ", "", .) %>%
        as.character() %>%
        as.data.frame() %>%
        setNames("product")
      
      # Get product tag
      tags <- html %>%
        html_nodes(".listTag") %>%
        html_text() %>%
        gsub("\\n", "", .) %>%
        as.factor() %>%
        as.data.frame() %>%
        setNames("tag")
      
      # Get product review counts
      reviewCounts <- html %>%
        html_nodes(".reviewTag") %>%
        html_text() %>%
        str_extract(., "[0-9.]+") %>%
        as.numeric() %>%
        as.data.frame() %>%
        setNames("reviewCount")
      reviewCounts[is.na(reviewCounts$reviewCount),] <- 0
      
      # Get product prices
      prices <- html %>%
        html_nodes(".price") %>%
        html_text() %>%
        gsub(",", "", .) %>%
        str_extract(., "[0-9.]+") %>%
        as.numeric() %>%
        as.data.frame() %>%
        setNames("price")
      
      # Get producer IDs
      producer_ids <- item_wrap_script %>%
        str_extract(., 'data-producer-id="[0-9.]+"') %>%
        str_extract(., "[0-9.]+") %>%
        as.numeric() %>%
        as.data.frame() %>%
        setNames("producer_id")
      
      # Get producer areas
      producer_info_script <- html %>%
        html_nodes(".producer-info")
      areas <- producer_info_script %>%
        html_nodes(".area") %>%
        html_text() %>%
        gsub("\\n", "", .) %>%
        gsub(" ", "", .) %>%
        as.factor() %>%
        as.data.frame() %>%
        setNames("area")
      
      # Get producer name
      producers <- producer_info_script %>%
        html_nodes(".name") %>%
        html_text() %>%
        gsub("\\n", "", .) %>%
        gsub(" ", "", .) %>%
        as.factor() %>%
        as.data.frame() %>%
        setNames("producer")
      
      # Numbers of rows
      id_len <- length(product_ids$product_id)
      
      # Make date-time column
      datetime <- Sys.time() %>%
        rep(., times = id_len) %>%
        as.data.frame() %>%
        setNames("datetime")
      
      # Make page number column
      pageNum <- pageNum %>%
        rep(., times = id_len) %>%
        as.numeric() %>%
        as.data.frame() %>%
        setNames("pageNum")
      
      # Connect data frames
      df_temp <- cbind(datetime, pageNum, product_ids, products, prices, areas, tags, reviewCounts, producer_ids, producers)
      
      # Bind as one data frame
      df_products <- rbind(df_products, df_temp)
    },
    error = function(e){ # If any error occurs, just step toward.
      next
    }
  )
}

# Export
df_to_csv("products", df_products)


# Scraping Tabechoku vegitable category page ----
# #Set initial values
# df_vegetables <- NULL
# 
# pb <- new_pb(max_productsPageNum)
# 
# # Scraping all products page
# for(pageNum in 1:max_productsPageNum) {
#   
#   # show progress
#   pb$tick()
#   
#   # Set url for search
#   url_temp <- paste(url_vegetables , "?page=", pageNum  ,sep = "") # URL with parameter+page_number
#   
#   # Scraping
#   # tryCatch(
#   #   {
#   #     # Get HTML value
#   #     html <- read_html(url_temp) # Get html
#   #     
#   #     # Pause scraping
#   #     Sys.sleep(sample(2:5, 1))
#   #     
#   #     # Get product IDs
#   #     item_wrap_script <- html %>%
#   #       html_nodes(".item-wrap")
#   #     product_ids <- item_wrap_script %>%
#   #       str_extract(.,'data-product-id="[0-9.]+"') %>%
#   #       str_extract(., "[0-9.]+") %>%
#   #       as.numeric() %>%
#   #       as.data.frame() %>%
#   #       setNames("product_id")
#   #     
#   #     # Get product names
#   #     products <- html %>%
#   #       html_nodes(".product-name h4") %>%
#   #       html_text() %>%
#   #       gsub(" ", "", .) %>%
#   #       as.character() %>%
#   #       as.data.frame() %>%
#   #       setNames("product")
#   #     
#   #     # Get product tag
#   #     tags <- html %>%
#   #       html_nodes(".listTag") %>%
#   #       html_text() %>%
#   #       gsub("\\n", "", .) %>%
#   #       as.factor() %>%
#   #       as.data.frame() %>%
#   #       setNames("tag")
#   #     
#   #     # Get product review counts
#   #     reviewCounts <- html %>%
#   #       html_nodes(".reviewTag") %>%
#   #       html_text() %>%
#   #       str_extract(., "[0-9.]+") %>%
#   #       as.numeric() %>%
#   #       as.data.frame() %>%
#   #       setNames("reviewCount")
#   #     reviewCounts[is.na(reviewCounts$reviewCount),] <- 0
#   #     
#   #     # Get product prices
#   #     prices <- html %>%
#   #       html_nodes(".price") %>%
#   #       html_text() %>%
#   #       gsub(",", "", .) %>%
#   #       str_extract(., "[0-9.]+") %>%
#   #       as.numeric() %>%
#   #       as.data.frame() %>%
#   #       setNames("price")
#   #     
#   #     # Get producer IDs
#   #     producer_ids <- item_wrap_script %>%
#   #       str_extract(., 'data-producer-id="[0-9.]+"') %>%
#   #       str_extract(., "[0-9.]+") %>%
#   #       as.numeric() %>%
#   #       as.data.frame() %>%
#   #       setNames("producer_id")
#   #     
#   #     # Get producer areas
#   #     producer_info_script <- html %>%
#   #       html_nodes(".producer-info")
#   #     areas <- producer_info_script %>%
#   #       html_nodes(".area") %>%
#   #       html_text() %>%
#   #       gsub("\\n", "", .) %>%
#   #       gsub(" ", "", .) %>%
#   #       as.factor() %>%
#   #       as.data.frame() %>%
#   #       setNames("area")
#   #     
#   #     # Get producer name
#   #     producers <- producer_info_script %>%
#   #       html_nodes(".name") %>%
#   #       html_text() %>%
#   #       gsub("\\n", "", .) %>%
#   #       gsub(" ", "", .) %>%
#   #       as.factor() %>%
#   #       as.data.frame() %>%
#   #       setNames("producer")
#   #     
#   #     # Numbers of rows
#   #     id_len <- length(product_ids$product_id)
#   #     
#   #     # Make date-time column
#   #     datetime <- Sys.time() %>%
#   #       rep(., times = id_len) %>%
#   #       as.data.frame() %>%
#   #       setNames("datetime")
#   #     
#   #     # Make page number column
#   #     pageNum <- pageNum %>%
#   #       rep(., times = id_len) %>%
#   #       as.numeric() %>%
#   #       as.data.frame() %>%
#   #       setNames("pageNum")
#   #     
#   #     # Connect data frames
#   #     df_temp <- cbind(datetime, pageNum, product_ids, products, prices, areas, tags, reviewCounts, producer_ids, producers)
#   #     
#   #     # Bind as one data frame
#   #     df_vegetables <- rbind(df_vegetables, df_temp)
#   #   },
#   #   error = function(e){ # If any error occurs, just step toward.
#   #     next
#   #   }
#   # )
#   # 
# }
# 
# # Check stats
# show_basic_stats(df_vegetables)
# 
# # Save raw data
# df_vegetables_raw <- df_vegetables
# 
# # Omit title is NA
# (product_naCount <- sum(is.na(df_vegetables$product)))
# if(product_naCount > 0) {
#   df_vegetables <- df_vegetables[!is.na(df_vegetables$product),]
# }
# 
# 
# # Change character code to shift-jis for text-mining (if necessary)
# (charcode_product <- Encoding(df_vegetables$product[sample(1:max_productsPageNum, 1)]))
# if(charcode_product != "unknown") {
#   df_vegetables$product <- as.data.frame(iconv(df_vegetables$product, from = charcode_product, to = character_code))
#   Encoding(df_vegetables$product) 
# }

# Scraping Tabechoku item pages ----

# Get ids searching for
df_ids <- df_products$product_id

# Set initial values
df_products_details <- NULL

# Set rules for avoiding continuous scraping damage
pause_count <- 1 # Pause scraping counter

# Scraping all products page
for(id in df_ids) {
  # Set url for search
  url_temp <- paste(url_products_main , id, sep = "") # URL  /ptoducts/id
  
  # Scraping
  tryCatch(
    {
      # Pause scraping
      Sys.sleep(sample(2:5, 1))
      
      # Get HTML value
      html_thisPage <- read_html(url_temp) # Get html
      
      # Get sale_points
      sales_points <- html_thisPage %>%
        html_nodes("body") %>%
        html_text()
      sales_points
      ##
      # Get prefecture and the ids (from the table below part of the page)
      # Get city
      # Get producer
      # Get kodawaris
      # Get Contents
      # Get large_category and the ids
      # Get middle_category and the ids
      # Get small_category and the ids
      # Get tags
      # Get description
      
      # Make date-time column 
      datetime <- Sys.time() %>%
        as.data.frame() 
      colnames(datetime) <- c("datetime")
      
      # Connect data frames
      df_temp <- cbind(datetime, pageNum, product_ids, products, prices, areas, tags, reviewCounts, producer_ids, producers)
      
      # Bind as one data frame of df_ids
      df_products_details <- rbind(df_products_details, df_temp)
    },
    error = function(e){ # If any error occurs, just step toward.
      next
    }
  )
}

# Check data
show_basic_stats(df_products_details)
df_products_details %>%
  dplyr::select(price, reviewCount, pageNum, tag) %>%
  ggpairs(
    mapping = aes(color = tag, alpha = 0.6),
    lower = list(continuous = "smooth", combo = "facetdensity")
  )
# Export as csv
df_to_csv("product_details", df_products_details)


##

source("C:/Users/taka-/Downloads/e-commerces-analytics/scraping/scraping_tabechoku.R")

# Add dummy columns ----
df_vegetables$hasGAP <- ifelse(str_detect(df_vegetables$product, "GAP"), "GAP", "no_GAP" ) %>%
  as.factor()
df_vegetables$hasOrganic <- ifelse(str_detect(df_vegetables$product, "有機"), "Organic", "no_Organic") %>%
  as.factor()
df_vegetables$hasNoPesticide <- ifelse(str_detect(df_vegetables$product, "農薬不"), "NoPesticide", "no_NoPesticide") %>%
  as.factor()
df_vegetables$hasJAS <- ifelse(str_detect(df_vegetables$product, "JAS"),"JAS", "noJAS") %>%
  as.factor()

# Add dummy columns
df_products$hasGAP <- ifelse(str_detect(df_products$product, "GAP"), "GAP", "no_GAP" ) %>%
  as.factor()
df_products$hasOrganic <- ifelse(str_detect(df_products$product, "有機"), "Organic", "no_Organic") %>%
  as.factor()
df_products$hasNoPesticide <- ifelse(str_detect(df_products$product, "農薬不"), "NoPesticide", "no_NoPesticide") %>%
  as.factor()
df_products$hasJAS <- ifelse(str_detect(df_products$product, "JAS"),"JAS", "noJAS") %>%
  as.factor()

# Export ----
df_to_csv("vegetables", df_vegetables)

## ==== IMPORT DATA ====
# OR get exported data instead ----
df_products <- read.csv("products_20221119-172528.csv", fileEncoding = character_code) %>%
  as.data.frame() %>%
  .[,-1] ## Omit iteration column

df_products <- df_products %>%
  mutate(
    area = factor(area),
    tag = factor(tag),
    producer = factor(producer),
    hasGAP = factor(hasGAP),
    hasOrganic = factor(hasOrganic),
    hasNoPesticide = factor(hasNoPesticide),
    hasJAS = factor(hasJAS)
  ) %>%
  na.omit()
glimpse(df_products)
##
## ==== Shape the data ====
# Shape the data----

# Save raw data
df_raw <- df_products

# Change character code to shift-jis for text-mining (if necessary)
charcode_product <- Encoding(df_products$product[sample(1:nrow(df_products), 1)])
if(charcode_product != "unknown") {
  df_vegetables$product <- as.data.frame(iconv(df_vegetables$product, from = charcode_product, to = character_code))
  Encoding(df_vegetables$product) 
}
##
## ==== VISUALIZE DATA ====
# Graph data ----
df_trimmed <- df_products[df_products$price < 10000 & df_products$price > 100,] 
check_data(df_trimmed)
df_trimmed_veg <- df_vegetables[df_vegetables$price < 10000 & df_vegetables$price > 100,]
df_trimmed %>%
  dplyr::select(price, reviewCount, pageNum, hasOrganic, hasGAP, hasNoPesticide, hasJAS) %>% ##[hasOrganic, hasGAP, hasNoPesticide, hasJAS]+ factors all
  ggpairs(
    mapping = aes(color = hasGAP, alpha = 0.6),
    lower = list(continuous = "smooth", combo = "facetdensity"),
    diag = list(continuous = "barDiag")
  )
# df_trimmed_veg %>%
#   dplyr::select(price, reviewCount, pageNum, hasOrganic, hasGAP, hasNoPesticide, hasJAS) %>% ##[hasOrganic, hasGAP, hasNoPesticide, hasJAS]+ factors all
#   ggpairs(
#     mapping = aes(color = hasGAP, alpha = 0.6),
#     lower = list(continuous = "smooth", combo = "facetdensity"),
#     diag = list(continuous = "barDiag")
#   )

# df_trimmed_veg_review <- df_vegetables[df_vegetables$reviewCount < 100,]
# df_trimmed_veg_review %>%
#   dplyr::select(price, reviewCount, pageNum, hasOrganic, hasGAP, hasNoPesticide, hasJAS) %>% ##[hasOrganic, hasGAP, hasNoPesticide, hasJAS]+ factors all
#   ggpairs(
#     mapping = aes(color = hasGAP, alpha = 0.6),
#     lower = list(continuous = "smooth", combo = "facetdensity"),
#     diag = list(continuous = "barDiag")
# )


check_data(df_products)


##

## ==== TEXT MINING ====
# Check product names text fo RMECab ----

# Create temp file for RMeCabFreq
temp_file <- tempfile()

#Extract product columns and push it into temp file
product_names <- df_products$product %>%
  write(file = temp_file)

# Run Text mining
df_product_termfreq_raw <- RMeCabFreq(temp_file) 

# Check Top terms
df_product_termFreq <- df_product_termfreq_raw %>%
  filter(Info1 == "名詞" & Info2 == "一般") %>%
  arrange(desc(Freq))%>%
  head(., n = 100)
df_product_termFreq
barplot(df_product_termFreq$Freq, main = "Word Count", names.arg = df_product_termFreq$Term, xlab = "Word", ylab="Frequency")

# N-gram
ngram <-  Ngram(temp_file, type = 1, N=3, pos=c("名詞", "名詞"))
ngram_edit <- ngram %>%
  .[order(ngram[,2], decreasing = TRUE)]
head(ngram_edit, n = 50)

## ==== CLASSIFICATION ====
# Shuffle data -----
# check_data(df_products)
df_temp <- df_products %>%
  select( -c("datetime", "pageNum", "product_id", "product", "producer_id", "producer", "area"))
shuffle_index <- sample(1:nrow(df_temp))
head(shuffle_index)
df_shuffled <- df_temp[shuffle_index,]
head(df_shuffled)

# Separate data for training and testing ----
data_train <- create_train_test(df_shuffled, 0.8, train = TRUE)
data_test <- create_train_test(df_shuffled, 0.8, train = FALSE)
dim(data_train)
dim(data_test)
prop.table(table(data_train$hasGAP))
prop.table(table(data_test$hasGAP))

# Balancing training data ----
data_train_balanced <- ovun.sample(
  hasJAS~.,
  data = data_train,
  N = nrow(data_train),
  p = 0.5,
  seed = 1,
  method = "both"
)$data

# Build the model ----
fit <- rpart(hasJAS~., data = data_train_balanced, method = 'class')
rpart.plot(fit)

# Make a prediction ----
predict_unseen <- predict(fit,data_test, type = 'class')

# Check accuracy ----
confusionMatrix(predict_unseen, data_test$hasJAS, mode = "everything", positive = "JAS")

# Tune model ----
df_temp <- NULL
rownum <- 1
pb <- new_pb(10^3)
for(i in 1:10) {
  for(j in 1:10){
    for(k in 1:10) {
      pb$tick()
      control <- rpart.control(
        minsplit = i,
        minbucket = j,
        maxdepth = k,
        cp = 0
      )
      tune_fit <- rpart(hasJAS~., data = data_train, method = 'class', control = control)
      ac <- accuracy_tune(tune_fit)
      df_temp$ac[rownum] = ac
      df_temp$minsplit[rownum] = i
      df_temp$minbucket[rownum] = j
      df_temp$maxdepth[rownum] = k
      rownum <- rownum + 1
    }
  }
}
df_temp_raw <- df_temp
df_temp <- as.data.frame(df_temp)
check_data(df_temp)
ggpairs(
  df_temp,
  lower = list(continuous = "smooth", combo = "facetdensity"),
  diag = list(continuous = "barDiag")
)
max_ac <- max(df_temp$ac)
df_max <- df_temp[df_temp$ac == max_ac,]
# check_data(df_max)
for(i in nrow(df_max)) {
  minsplit <- df_max$minsplit[i]
  minbucket <- df_max$minbucket[i]
  maxdepth <- df_max$maxdepth[i]
  control <- rpart.control(
    minsplit = minsplit,
    minbucket = minbucket,
    maxdepth = maxdepth,
    cp = 0
  ) 
  tune_fit <- rpart(hasJAS~., data = data_train, method = 'class', control = control)
  (rpart.plot(tune_fit))
}

# Confusion matrix ----
pred <- predict(tune_fit, data_test, type = 'class')
confusionMatrix(pred, data_test$hasJAS, mode = 'everything', positive = "JAS")
