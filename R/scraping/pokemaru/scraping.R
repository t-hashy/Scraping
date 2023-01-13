## Pokemaru ##

# ===== 0. Before scraping ====
# 1. Check the robots.txt page
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User-agent: *
# Disallow: /users/
#   Allow: /users/sign_in
# Allow: /users/sign_up
# Disallow: /message/
#   Disallow: /orders/
#   Disallow: /dashboard/
#   Disallow: /specified_commercial_law
# Disallow: /products/*/purchase/
#   Disallow: /purchase_complete/
#   Sitemap: https://poke-m.com/sitemap.xml.gz
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ==== 1-1. LOADING ====
# Data handling
library(tidyverse) # Data frame handling
library(glue) # String pasting
library(dplyr) # Data frame manipulations
library(plyr) # Recursive function
library(jsonlite) # Json to data frame
library(purrr) # Recursive function of map
library(data.table) # Convert list to data frame
# Scraping
library(rvest) # Static scraping
# HTML
library(xml2) # HTML file saving
# Custome functions
source(glue("./r/functions.R")) # Get custome functions
# Recursive 
library(rrapply) # Recursive fucntion


# ==== 1-1. Set Environmental Variables ====
# URLs
url.home <- "https://poke-m.com/"
url.products <- glue("{url.home}/products")
# Scrapings
sleep.sec <- 1
sleep.sec.min <- sleep.sec - 1
sleep.sec.max <- sleep.sec + 1
# Products
products.num <- 10000 # The site shows all products are 10,000 items
products.maxpage <- ceiling(products.num/40) #40 items per page

# ==== 2. Attain product abstracts by crawling products page from 1 to max page ====
# Set variables for for-loop
lst_abst <- list()

# Crawl and retrieve html nodes
print(glue("~~~~ ESTIMATED: {ceiling(products.maxpage * sleep.sec / 60)} min ~~~~"))
pb <- create_new_pb(products.maxpage)
for(page.num in 1:products.maxpage) {
  
  # Show progress and remaining time
  pb$tick()
  
  # Set URL for search:  parameter = page_number
  url.this <- glue("{url.products}?page_no={page.num}")
  
  # Set try-catch function for unexpected error
  tryCatch(
    {
      
      # Retrieve html: Get html
      html.this <- read_html(url.this)
      
      # Extract data props as json
      lst_data.this.all <- html.this %>%
        html_elements("#__NEXT_DATA__") %>%
        html_text2() %>%
        jsonlite::parse_json()
      lst_data.this.contents <- lst_data.this.all$props$pageProps$fallback[[3]]$data
      
      # Add ts and page number
      lst_abst.this <- list(list(ts = Sys.time(), data = lst_data.this.contents))
      names(lst_abst.this) <- page.num
      
      # Push html into a list
      lst_abst <-  append(lst_abst, lst_abst.this)
      
      # Pause scraping
      Sys.sleep(sample(seq(from = min_sleep, to = max_sleep, by = 1), 1))
    },
    
    # If unexpected error occurred, skip for the next loop.
    error = function(e){
      next
    }
  )
}

# Export html data
export_data(lst_abst, file_name = "abst", directory = "./data/pokemaru", file_type = "Rds")

# ==== 3. Extract product ids ====
# Set the data frame
lst_products <- list()

# Get product ids
est.min <- ceiling((products.num - length(lst_products)) / (10*60))
message(glue("Estimated: {est.min} min | start at: {as.POSIXct(Sys.time(), format='%H:%M')}, est. end at: {as.POSIXct(Sys.time()+est.min*60, format='%H:%M')}"))
pb <- create_new_pb(products.num)
counter <- 0
for(lst_abst.this in lst_abst){
  
  # Get props
  page.this <- names(lst_abst)[counter]
  ts.this <- lst_abst.this$ts
  lst_page.data.this <- lst_abst.this$data
  
  # Crawl each page
  for(lst_product.this in lst_page.data.this){
    
    # Progress
    pb$tick()
    counter = counter + 1
    
    # Get id
    product.id.this <- lst_product.this$id
      
    # Push into main lst
    lst_product.this <- list(lst_product.this)
    names(lst_product.this) <- product.id.this
    lst_products <- append(lst_products, lst_product.this)
  }
}

# Export the data
export_data(lst_products, file_name = "products", directory = "./data/pokemaru")

# ==== 4. Attain product details by crawling products page with each id ====
# Set variables for for-loop
lst_details <- list()
est.hours <- ceiling(((products.num - length(lst_details)) * sleep.sec) / (60*60))
est.end <- as.POSIXct(Sys.time() + est.hours*60*60,format="%H:%M")
temp.directory <- "./data/pokemaru/temp"
file.title <- "details"

# Crawl and retrieve data as json
print(glue("~~~~ ESTIMATED: {est.hours} hours | start at: {as.POSIXct(Sys.time(), format='%H:%M')}, est end at: {est.end} ~~~~"))
pb <- create_new_pb(products.num)
counter <- 0
for(lst_product.this in lst_products){
  
  # Progress
  pb$tick()
  counter = counter + 1
  
  # Get product id
  product.id.this <- names(lst_products)[[counter]]
  
  # Existence check
  if(length(lst_details) == 0 || !(product.id.this %in% names(lst_details))){
    
    # Set URL
    url.this <- glue("{url.products}/{product.id.this}")
    
    tryCatch(
      {
        # Make get request
        html.this <- read_html(url.this)
        
        # Get date time
        ts.this <- Sys.time()
        
        # Get contents
        lst_details.this.all <- html.this %>%
          html_elements("#__NEXT_DATA__") %>%
          html_text2() %>%
          jsonlite::parse_json()
        
        # Extract the target content
        lst_details.this <- lst_details.this.all$props$pageProps$fallback[[1]]
        
        # Push into list
        lst_details.this <- list(list(ts=ts.this, data=lst_details.this))
        # Push to main
        names(lst_details.this) <- product.id.this
        lst_details <- append(lst_details, lst_details.this)
      },
      error = function(e){
        lst_details.this <- list(list(error = TRUE))
        names(lst_details.this) <- product.id.this
      }
    )
  }
  
  # Save for safety (along with sleeping gap)
  if(counter %% 10 == 0){
    # Remove previous files
    files <- list.files(temp.directory) %>%
      grep(file.title, ., value = TRUE) %>%
      map(function(x) glue("{temp.directory}/{x}")) %>%
      unlink()
    # Add new file
    file <- export_data(lst_details, file_name = file.title, directory = temp.directory)
  }
}

# Remove previous files
files <- list.files(temp.directory) %>%
  grep(file.title, ., value = TRUE) %>%
  map(function(x) glue("{temp.directory}/{x}")) %>%
  unlink(files)
# Add new file
export_data(lst_details, file_name = file.title, directory = "./data/pokemaru")

# ==== 5. Convert list to data frame ====

# Set list
lst_pokemaru <- lst_details

# Shape the data
lst_pokemaru <- lst_pokemaru %>%
  map(function(lst){
    lst.ts <- list(ts = lst$ts)
    lst.data <- lst$data
    lst.shaped <- append(lst.data, lst.ts)
  })

# Convert list to data frame 
df_pokemaru <- lst.to.df(lst_pokemaru)

# Export data frame
export_data(df_pokemaru, file_name = "df", directory = "./data/pokemaru")
