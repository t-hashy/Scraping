# ==== BASE SETTINGS ====
# ---- *Loading ----
source(paste(getwd(), "/r/functions.R", sep = ""))

library(rvest) # Scraping
library(tidyverse) # String_extraction (using when shaping data), ggplot, etc..
library(progress) # Progress bar in for-loop
 

# ---- *Set URLs ----
url_home <- "https://www.tabechoku.com"
url_productsCtg <- paste(url_home, "/products", sep = "")

# ==== SCRAPING  PRODUCTS CATEGORY PAGES ====
# ---- *Get max products-category page number----
num_productsCtg <- read_html(url_productsCtg_main) %>%
  html_nodes(xpath = "//p[@class='h3']") %>%
  html_text() %>%
  str_extract(., "[0-9.]+,?[0-9.]+") %>%
  gsub(",","", .) %>%
  as.numeric()
Sys.sleep(sample(2:4, 1)) # Stop seconds after rolling the site
maxPageNum_productsCtg <- ceiling(num_productsCtg/100)

# ---- *Crawl products-category pages one by one ----

# Set for-loop basics
pb <- create_new_pb(maxPageNum_productsCtg)
lst_html_products <- list()
min_sleep <- 1
max_sleep <- 10
print(paste("~~~~ ESTIMATED: ", ceiling(((maxPageNum_productsCtg / 100) * mean(max_sleep,min_sleep)) / 60), "min", sep = ""))

# Crawl and retrieve html nodes
for(pageNum in 1:maxPageNum_productsCtg) {
  
  # Show progress and remaining time
  pb$tick()
  
  # Set URL for search:  parameter = page_number
  url_this <- paste(url_productsCtg , "?page=", pageNum  ,sep = "")
  
  # Set for unexpected error
  tryCatch(
    {
      # Retrieve html: Get html and turn it into list
      lst_html_this <- list(read_html(url_this))
      
      # Push html into a list
      lst_html_products <- append(lst_html_products, lst_html_this)
      
      # Pause scraping
      Sys.sleep(sample(seq(from = min_sleep, to = max_sleep, by = 1), 1))
    },
    
    # If unexpected error occurred, skip for the next loop.
    error = function(e){
      next
    }
  )
}

# ---- *Extract values from html ----

# Set for-loop basics 
pb <- create_new_pb(length(lst_html_products))
df_productsCtg <- data.frame()
page <- 1

# Get values as data-frame  
for(html in lst_html_products) {
  
  # Progress bar
  pb$tick()
  
  # Extract item-wrap class
  nodes_item_wrap <- html %>%
    html_nodes(".item-wrap")
  
  # Extract product IDs from item-wrap class
  df_temp <- nodes_item_wrap %>%
    str_extract('data-product-id="[0-9.]+"') %>%
    str_extract("[0-9.]+") %>%
    as.numeric() %>%
    as.data.frame() %>%
    setNames("id_product")

  # Extract product names from item-wrap class
  df_temp$product <- html %>%
    html_nodes(".product-name h4") %>%
    html_text() %>%
    gsub(" ", "", .) %>%
    as.character()

  # Extract product tags
  df_temp$tag <- html %>%
    html_nodes(".listTag") %>%
    html_text() %>%
    gsub("\\n", "", .) %>%
    as.factor()

  # Extract product review counts and substitute na into 0
  df_temp$reviews <- html %>%
    html_nodes(".reviewTag") %>%
    html_text() %>%
    str_extract(., "[0-9.]+") %>%
    as.numeric()
  df_temp$reviews[is.na(df_temp$reviews)] <- 0

  # Extract product prices
  df_temp$price <- html %>%
    html_nodes(".price") %>%
    html_text() %>%
    gsub(",", "", .) %>%
    str_extract(., "[0-9.]+") %>%
    as.numeric()

  # Extract producer IDs
  df_temp$id_producer <- nodes_item_wrap %>%
    str_extract(., 'data-producer-id="[0-9.]+"') %>%
    str_extract(., "[0-9.]+") %>%
    as.numeric() 

  # Extract producer-info class
  nodes_producer_info <- html %>%
    html_nodes(".producer-info")
  
  # Extract areas
  df_temp$area <- nodes_producer_info %>%
    html_nodes(".area") %>%
    html_text() %>%
    gsub("\\n", "", .) %>%
    gsub(" ", "", .) %>%
    as.factor()
  
  # Add a prefecture column
  df_temp$prefecture <- extract_prefecture(df_temp$area) %>%
    as.factor()
  
  # Extract producer name
  df_temp$producer <- nodes_producer_info %>%
    html_nodes(".name") %>%
    html_text() %>%
    gsub("\\n", "", .) %>%
    gsub(" ", "", .) %>%
    as.factor()

  # Add date-time column
  df_temp$datetime <- Sys.time() %>%
    rep(times = nrow(df_temp))

  # Add page number column
  df_temp$page <- page %>%
    rep(times = nrow(df_temp))
  page = page + 1

  # Combine data-frames
  df_productsCtg <- rbind(df_productsCtg, df_temp)
}

# ---- *Export as Rds file ----
df_to_rds(df_productsCtg, file_name = "productsCtg", directory = "data/tabechoku")

# ==== SCRAPING EACH ITEM PAGES ====
# ---- *Crawl detailed pages one by one ----

# Set for-loop basics
pb <- create_new_pb(nrow(df_productsCtg))
lst_html_details <- list()
min_sleep <- 0
max_sleep <- 2
print(paste("~~~~ ESTIMATED: ", ceiling(nrow(df_productsCtg) * mean(min_sleep, max_sleep) / 60), "min", sep = "" ))

# Crawl and retrieve html nodes
for(id in df_productsCtg$id_product) {
  
  # Show progress and remaining time
  pb$tick()
  
  # Set URL for search
  url_this <- paste(url_productsCtg, "/", id, sep = "")
  
  # Set for unexpected error
  tryCatch(
    {
      # Retrieve html: Get html and turn it into list
      lst_html_this <- list(read_html(url_this)) 
      
      # Push html into a list
      lst_html_details <- append(lst_html_details, lst_html_this)
      
      # Pause scraping
      Sys.sleep(sample( seq(from = min_sleep, to = max_sleep, by = 1), 1))
    },
    
    # If unexpected error occurred, skip for the next loop.
    error = function(e){
      next
    }
  )
  
  # Checker
  if(i > 5) {
    break
  }else {
    i + 1
  }
}

lst_html_details

# ---- *Extract values from html ----
# ---- *Export as Rds file ----






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

