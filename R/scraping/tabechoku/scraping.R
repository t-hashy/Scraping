## Tabechoku ##


# ==== 0. Check robots.txt =====
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User-agent: *
# Disallow: /api/
#   
# Sitemap: https://www.tabechoku.com/system/sitemap.xml.gz
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ==== 1-1. LOADING ====
# Data handling
library(tidyverse) # Data frame handling
library(glue) # String pasting
library(dplyr) # Data frame manipulations
library(jsonlite) # Json to data frame
# Scraping
library(rvest) # Static scraping
# HTML
library(xml2) # HTML file saving
# Custome functions
source(glue("./r/functions.R")) # Get custome functions

# ==== 1-2. Set Environmental Variables ====
# URLs
url.home <- "https://www.tabechoku.com"
url.products <- glue("{url.home}/products")
# Scrapings
sleep.sec <- 1
sleep.sec.min <- sleep.sec - 1
sleep.sec.max <- sleep.sec + 1
# Directory
temp.directory <- "./data/tabechoku/temp"

# ==== 2. Get the number of products and max page number ====
products.num <- read_html(url.products) %>%
  html_nodes(xpath = "//p[@class='h3']") %>%
  html_text() %>%
  str_extract(., "[0-9.]+,?[0-9.]+") %>%
  gsub(",","", .) %>%
  as.numeric()
Sys.sleep(sleep.sec) # Stop seconds after rolling the site
products.maxpage <- ceiling(products.num/100)

# ==== 3. Get all of the product ids ====
# Set variables for for-loop
pb <- create_new_pb(products.maxpage)
lst_ids <- list()

# Crawl and retrieve html nodes
print(glue("~~~~ ESTIMATED: {ceiling(products.maxpage * sleep.sec / 60)} min ~~~~"))
counter <- 0
for(page.num in 1:products.maxpage) {
  
  # Show progress and remaining time
  pb$tick()
  counter = counter + 1
  
  # Set URL for search:  parameter = page_number
  url.this <- glue("{url.products}?page={page.num}")
  
  # Set try-catch function for unexpected error
  tryCatch(
    {
      
      # Retrieve html: Get html
      html.this <- read_html(url.this)
      
      # Extract unique ids
      lst_ids.this <- html.this %>%
        html_nodes("p-productList")
      
      # Set list to push
      lst_ids.this <- list(list(ts = Sys.time(), page = page.num, ids = lst_ids.this))
      names(lst_ids.this) <- page.num
      
      # Push to main
      lst_ids <- append(lst_ids, lst_ids.this)
      
      # Pause scraping
      Sys.sleep(sample(seq(from = sleep.sec.min, to = sleep.sec.max, by = 1), 1))
    },
    
    # If unexpected error occurred, skip for the next loop.
    error = function(e){
    }
  )
  
  if(counter %% 10 == 0){
    # Remove old temp files
    files <- list.files("./data/tabechoku/temp") %>%
      grep("ids", value = TRUE) %>%
      map(function(x) glue("./data/tabechoku/temp/{x}")) %>%
      unlist()
    
    # Export new temp file
    export_data(lst_ids, file_name = "ids", directory = "./data/tabechoku/temp")
  }
}
# Remove old temp files
files <- list.files("./data/tabechoku/temp") %>%
  grep("ids", value = TRUE) %>%
  map(function(x) glue("./data/tabechoku/temp/{x}")) %>%
  unlist()
# Export html data
export_data(lst_ids, file_name = "ids", directory = "./data/tabechoku", file_type = "Rds")


# ==== 4. Attain product details by crawling products page with each id ====

# Initialize
lst_details <- list()  

# Set env
est.hours <- ceiling((length(df_ids$product.id) -  length(lst_details)) * sleep.sec / (60*60))
est.endat <- as.POSIXct(Sys.time()+est.hours*60*60, format="%H:%M")
file.title <- "details"

# Crawl and retrieve html nodes
print(glue("~~~~ ESTIMATED: {est.hours} hours | start at :{as.POSIXct(Sys.time(), format='%H:%M')}, end at est.: {est.endat}) ~~~~"))
pb <- create_new_pb(length(df_ids$product.id))
for(id.this in df_ids$product.id) {
  
  # Progress
  pb$tick()
    
  # Existence check
  if(length(lst_details) == 0 || !(id.this %in% names(lst_details))){
    
    # Set URL
    url.this <- glue("{url.products}/{id.this}")
    
    # Retrieve data
    page.this <- df_ids$page[df_ids$product.id == id.this]
    ts.this <- df_ids$ts[df_ids$product.id == id.this]
    
    tryCatch(
      {
        # Get html
        html.this <- read_html(url.this)
        
        # Extract data props as json list
        lst_data.this <- html.this %>%
          html_nodes(".products-order div") %>%
          html_attr("data-props") %>%
          jsonlite::parse_json()
        
        # Add ts and page number
        lst_details.this <- list(list(ts = Sys.time(), page = page.this, data = lst_data.this))
        names(lst_details.this) <- id.this
        
        # Push to main
        lst_details <- append(lst_details, lst_details.this)
      },
      error = function(e){
        
        # Set error value
        lst_details.this <- list(list(ts = Sys.time(), page = page.this, data = NA))
        names(lst_details.this) <- id.this
        
        # Push to main
        lst_details <- append(lst_details, lst_details.this)
      }
    )
    
    # Save for safety
    if(sample(1:100,1) %% 100 == 0){
      
      # Remove old temp files
      list.files("./data/tabechoku/temp") %>%
        grep(pattern = "details", ., value = TRUE) %>%
        map(function(x) glue("./data/tabechoku/temp/{x}")) %>%
        unlink()
      
      # Export new temp file
      export_data(lst_details, file_name = "details", directory = "./data/tabechoku/temp")
    }
  }
}

# Remove old temp files
list.files("./data/tabechoku/temp") %>%
  grep("details", value = TRUE) %>%
  map(function(x) glue("./data/tabechoku/temp/{x}")) %>%
  unlink()
# Export new temp file
export_data(lst_details, file_name = "details", directory = "./data/tabechoku")


# ==== 5. Convert from Json to Data frame ====

# Set site specific data frame
lst_tabechoku <- lst_details

# Shape the data
ids <- names(lst_tabechoku)
ind <- 0
lst_tabechoku.shaped <- lst_tabechoku %>%
  map(function(lst){
    ind <- ind + 1
    lst.id <- list(id = ids[ind])
    lst.ts <- list(ts = lst$ts)
    lst.page <- list(page = lst$page)
    lst.data <- lst$data
    lst.shaped <- append(lst.id, lst.page) %>%
      append(lst.ts) %>%
      append(lst.data)
  })

# Covert to data frame
df_tabechoku <- list.to.df(lst_tabechoku.shaped)

# Export 
export_data(df_tabechoku, file_name = "df", directory = "./data/tabechoku")