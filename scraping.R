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
# #

# Global variables ----
character_code <-  "shift-jis"
dr_path <- getwd() # Directory path

##
# Functions ----
show_basic_stats <- function(df) {
  head <- head(df, n = 5)
  tail <- tail(df, n = 5)
  str <- str(df)
  summary <- summary(df)
  list <- list(str = str, summary = summary, head = head, tail = tail)
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
##  

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
progress <- 0

# Scraping all products page
for(pageNum in 1:max_productsPageNum) {
  
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
  
  # Show progress
  progress <- progress + 1
  progress_ratio <- progress / max_productsPageNum
  print(paste(progress, "/", max_productsPageNum, " (", progress_ratio*100, "%)", sep = ""))
}

# Export
df_to_csv("products", df_products)

df_temp <- df_products[df_products$hasGAP == "GAP",]

# OR get exported data instead ----
df_products <- read.csv("products_20221024-105553.csv", fileEncoding = character_code) %>%
  as.data.frame() %>%
  .[,-1] ## Omit numbering column

# Scraping Tabechoku vegitable category page ----
#Set initial values
df_vegetables <- NULL
progress <- 0
pageNum <- 1

# Scraping all products page
for(pageNum in 1:max_productsPageNum) {
  
  # Set url for search
  url_temp <- paste(url_vegetables , "?page=", pageNum  ,sep = "") # URL with parameter+page_number
  
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
      df_vegetables <- rbind(df_vegetables, df_temp)
    },
    error = function(e){ # If any error occurs, just step toward.
      next
    }
  )
  
  # Show progress
  progress <- progress + 1
  progress_ratio <- progress / max_productsPageNum
  print(paste(progress, "/", max_productsPageNum, " (", progress_ratio*100, "%)", sep = ""))
}

# Check stats
show_basic_stats(df_vegetables)

# Save raw data
df_vegetables_raw <- df_vegetables

# Omit title is NA
(product_naCount <- sum(is.na(df_vegetables$product)))
if(product_naCount > 0) {
  df_vegetables <- df_vegetables[!is.na(df_vegetables$product),]
}


# Change character code to shift-jis for text-mining (if necessary)
(charcode_product <- Encoding(df_vegetables$product[sample(1:max_productsPageNum, 1)]))
if(charcode_product != "unknown") {
  df_vegetables$product <- as.data.frame(iconv(df_vegetables$product, from = charcode_product, to = character_code))
  Encoding(df_vegetables$product) 
}

# Add dummy columns
df_vegetables$hasGAP <- ifelse(str_detect(df_vegetables$product, "GAP"), "GAP", "no_GAP" ) %>%
  as.factor()
df_vegetables$hasOrganic <- ifelse(str_detect(df_vegetables$product, "有機"), "Organic", "no_Organic") %>%
  as.factor()
df_vegetables$hasNoPesticide <- ifelse(str_detect(df_vegetables$product, "農薬不"), "NoPesticide", "no_NoPesticide") %>%
  as.factor()
df_vegetables$hasJAS <- ifelse(str_detect(df_vegetables$product, "JAS"),"JAS", "noJAS") %>%
  as.factor()


# Export
df_to_csv("vegetables", df_vegetables)

# Shape the data----
# Check stats
show_basic_stats(df_vegetables)

# Save raw data
df_vegetables_raw <- df_vegetables

# Omit title is NA
product_naCount <- sum(is.na(df_vegetables$product))
if(product_naCount > 0) {
  df_vegetables <- df_vegetables[!is.na(df_vegetables$product),]
}


# Change character code to shift-jis for text-mining (if necessary)
charcode_product <- Encoding(df_vegetables$product[sample(1:max_productsPageNum, 1)])
if(charcode_product != "unknown") {
  df_vegetables$product <- as.data.frame(iconv(df_vegetables$product, from = charcode_product, to = character_code))
  Encoding(df_vegetables$product) 
}

# Add dummy columns
df_products$hasGAP <- ifelse(str_detect(df_products$product, "GAP"), "GAP", "no_GAP" ) %>%
  as.factor()
df_products$hasOrganic <- ifelse(str_detect(df_products$product, "有機"), "Organic", "no_Organic") %>%
  as.factor()
df_products$hasNoPesticide <- ifelse(str_detect(df_products$product, "農薬不"), "NoPesticide", "no_NoPesticide") %>%
  as.factor()
df_products$hasJAS <- ifelse(str_detect(df_products$product, "JAS"),"JAS", "noJAS") %>%
  as.factor()

# Export as csv
df_to_csv("products", df_products)

# Check product names text ----

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

# Graph data ----
df_trimmed <- df_products[df_products$price < 10000 & df_products$price > 100,] 
show_basic_stats(df_trimmed)
df_trimmed_veg <- df_vegetables[df_vegetables$price < 10000 & df_vegetables$price > 100,]
df_trimmed %>%
  dplyr::select(price, reviewCount, pageNum, hasOrganic, hasGAP, hasNoPesticide, hasJAS) %>% ##[hasOrganic, hasGAP, hasNoPesticide, hasJAS]+ factors all
  ggpairs(
    mapping = aes(color = hasGAP, alpha = 0.6),
    lower = list(continuous = "smooth", combo = "facetdensity"),
    diag = list(continuous = "barDiag")
  )
df_trimmed_veg %>%
  dplyr::select(price, reviewCount, pageNum, hasOrganic, hasGAP, hasNoPesticide, hasJAS) %>% ##[hasOrganic, hasGAP, hasNoPesticide, hasJAS]+ factors all
  ggpairs(
    mapping = aes(color = hasGAP, alpha = 0.6),
    lower = list(continuous = "smooth", combo = "facetdensity"),
    diag = list(continuous = "barDiag")
  )

df_trimmed_veg_review <- df_vegetables[df_vegetables$reviewCount < 100,]
df_trimmed_veg_review %>%
  dplyr::select(price, reviewCount, pageNum, hasOrganic, hasGAP, hasNoPesticide, hasJAS) %>% ##[hasOrganic, hasGAP, hasNoPesticide, hasJAS]+ factors all
  ggpairs(
    mapping = aes(color = hasGAP, alpha = 0.6),
    lower = list(continuous = "smooth", combo = "facetdensity"),
    diag = list(continuous = "barDiag")
  )


show_basic_stats(df_products)
show_basic_stats(df_vegetables)

# Export as csv
df_to_csv("products", df_products)

##

# Scraping Tabechoku item pages ----

# Get ids searching for
df_ids <- df_products$product_id

# Set initial values
df_products_details <- NULL

# Set rules for avoiding continuous scraping damage
pause_count <- 1 # Pause scraping counter
id <- 168991
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



# LATER===============


# Text mining----
