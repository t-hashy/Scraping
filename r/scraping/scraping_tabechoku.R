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
# lst_html_productsCtg <- list()
min_sleep <- 1
max_sleep <- 10
print(paste("~~~~ ESTIMATED: ", ceiling(((maxPageNum_productsCtg / 100) * mean(max_sleep,min_sleep)) / 60), "min", sep = ""))
i <- 0
len <- length(lst_html_productsCtg)

# Crawl and retrieve html nodes
for(pageNum in 1:maxPageNum_productsCtg) {
  
  # Show progress and remaining time
  pb$tick()
  
  # Existing check
  flag <- TRUE
  i = i + 1
  if(i <= len){
    flag <- FALSE
  } else{
    
    # Set URL for search:  parameter = page_number
    url_this <- paste(url_productsCtg , "?page=", pageNum  ,sep = "")
    # Set for unexpected error
    tryCatch(
      {
        # Retrieve html: Get html and turn it into list
        lst_html_this <- list(read_html(url_this))
        
        # Push html into a list
        lst_html_productsCtg <- append(lst_html_productsCtg, lst_html_this)
        
        # Pause scraping
        Sys.sleep(sample(seq(from = min_sleep, to = max_sleep, by = 1), 1))
      },
      
      # If unexpected error occurred, skip for the next loop.
      error = function(e){
        flag <- FALSE
      }
    )
  }
  
  # Checker
  if(!flag) next
}

# Export html data
export_data(lst_html_productsCtg, file_name = "productsCtg", directory = "data/tabechoku")

# ---- *Extract values from html ----

# Set for-loop basics 
pb <- create_new_pb(length(lst_html_productsCtg))
df_productsCtg <- data.frame()
page <- 1

# Get values as data-frame  
for(html in lst_html_productsCtg) {
  
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
export_data(df_productsCtg, file_name = "productsCtg", directory = "data/tabechoku")

# ==== SCRAPING EACH ITEM PAGES ====
# ---- *Crawl detailed pages one by one ----

# Set for-loop basics
pb <- create_new_pb(nrow(df_productsCtg))
min_sleep <- 1
max_sleep <- 2
estimated_min <- ceiling(nrow(df_productsCtg) * (((max_sleep - min_sleep) / 2) + min_sleep) / 60)
print(paste("ESTIMATED: ", estimated_min, "min (", ceiling(estimated_min / 60), "hours)", sep = "" ))
lst_html_details <- list()
len <- length(lst_html_details)
i <- 0

# Crawl and retrieve html nodes
for(id in df_productsCtg$id_product) {
  
  # Show progress and remaining time
  pb$tick()
  
  # Existing check
  flag <- TRUE
  i = i + 1
  if(i <= len){
    flag <- FALSE
  } else{
    
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
        flag <- FALSE
      }
    )
  }

  # Save as insurance
  if(i %% 10000 == 0){
    export_data(lst_html_details, file_name = "details", directory = "data/tabechoku", file_type = "RData")
  }
  
  # Checker
  if(!flag) next
}

# Save html data
export_data(lst_html_details, file_name = "details",directory = "data/tabechoku", file_type = "RData")

# ---- *Extract values from html ----

# Set for-loop basics 
pb <- create_new_pb(length(lst_html_details))
df_details <- data.frame()
df_keywards <- data.frame()
df_tags <- data.frame()
df_lctg <- data.frame()
df_mctg <- data.frame()
df_sctg <- data.frame()

# Get values as data-frame  
for(html in lst_html_details) {
  
  # Progress bar
  pb$tick()
  
  ##
  # Extract product id: from the table below part of the page
  df_temp <- html %>%
    html_nodes("") %>%
    html_text() %>%
    as.numeric() %>%
    as.data.frame() %>%
    setNames("id_product")
    
  # Extract city
  df_temp$city <- html %>%
    html_nodes("") %>%
    html_text()
  
  # Extract keywords
  df_keywords$keyword <- html %>%
    html_nodes("") %>%
    html_text()
  
  # Add id column into keywords data-frame
  df_keywards$id_product <- df_temp$id_product[1] %>%
    rep(times = nrow(df_keywards$keyword))
  
  # Extract Content
  df_temp$content <- html %>%
    html_nodes("") %>%
    html_text()
  
  # Extract large_category and the ids
  df_temp$l_ctg <- html %>%
    html_nodes("") %>%
    html_text()
  
  # Extract middle_category and the ids
  df_temp$m_ctg <- html %>%
    html_nodes("") %>%
    html_text()
  
  # Extract small_category and the ids
  df_temp$s_ctg <- html %>%
    html_nodes("") %>%
    html_text()
  
  # Extract tags
  
  # Extract description
  # Merge columns with products-category data with key of id_product
  df_temp <- merge(df_temp, df_productsCtg, by = "id_product", all.x = T)
  
  # # Extract item-wrap class
  # nodes_item_wrap <- html %>%
  #   html_nodes(".item-wrap")
  # 
  # # Extract product IDs from item-wrap class
  # df_temp <- nodes_item_wrap %>%
  #   str_extract('data-product-id="[0-9.]+"') %>%
  #   str_extract("[0-9.]+") %>%
  #   as.numeric() %>%
  #   as.data.frame() %>%
  #   setNames("id_product")
  # 
  # # Extract product names from item-wrap class
  # df_temp$product <- html %>%
  #   html_nodes(".product-name h4") %>%
  #   html_text() %>%
  #   gsub(" ", "", .) %>%
  #   as.character()
  # 
  # # Extract product tags
  # df_temp$tag <- html %>%
  #   html_nodes(".listTag") %>%
  #   html_text() %>%
  #   gsub("\\n", "", .) %>%
  #   as.factor()
  # 
  # # Extract product review counts and substitute na into 0
  # df_temp$reviews <- html %>%
  #   html_nodes(".reviewTag") %>%
  #   html_text() %>%
  #   str_extract(., "[0-9.]+") %>%
  #   as.numeric()
  # df_temp$reviews[is.na(df_temp$reviews)] <- 0
  # 
  # # Extract product prices
  # df_temp$price <- html %>%
  #   html_nodes(".price") %>%
  #   html_text() %>%
  #   gsub(",", "", .) %>%
  #   str_extract(., "[0-9.]+") %>%
  #   as.numeric()
  # 
  # # Extract producer IDs
  # df_temp$id_producer <- nodes_item_wrap %>%
  #   str_extract(., 'data-producer-id="[0-9.]+"') %>%
  #   str_extract(., "[0-9.]+") %>%
  #   as.numeric() 
  # 
  # # Extract producer-info class
  # nodes_producer_info <- html %>%
  #   html_nodes(".producer-info")
  # 
  # # Extract areas
  # df_temp$area <- nodes_producer_info %>%
  #   html_nodes(".area") %>%
  #   html_text() %>%
  #   gsub("\\n", "", .) %>%
  #   gsub(" ", "", .) %>%
  #   as.factor()
  # 
  # # Add a prefecture column
  # df_temp$prefecture <- extract_prefecture(df_temp$area) %>%
  #   as.factor()
  # 
  # # Extract producer name
  # df_temp$producer <- nodes_producer_info %>%
  #   html_nodes(".name") %>%
  #   hml_text() %>%
  #   gsub("\\n", "", .) %>%
  #   gsub(" ", "", .) %>%
  #   as.factor()
  
  # Add date-time column
  df_temp$datetime <- Sys.time() %>%
    rep(times = nrow(df_temp))
  
  # Combine data-frames
  df_details <- rbind(df_details, df_temp)
}


# ---- *Export as Rds file ----
export_data(df_details, file_name = "details", directory = "data/tabechoku")
# save(lst_html_details, file = "data/tabechoku/details_20221124.RData")




#---- OTHERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
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

