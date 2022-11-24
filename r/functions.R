# ==== DATA FRAME ====

## ---- *Check the Data ----
check_data <- function(df, plot = FALSE) {
  glimpse(df)
  print(head(df, n = 5))
  print(summary(df))
  if(plot) {
    plot(df) 
  }
  return("DONE")
}

## ---- *Export data-frame ----
export_data <- function(data, file_name = "data", directory = "data", file_type = "Rds") {
  # Set file name
  now <- convert_datetime_into_str()
  file <- paste(directory, "/", file_name, "_", now, ".", file_type, sep = "")
  # Convert into file
  if(file_type == "Rds"){
    saveRDS(data, file = file)  
  }else if(file_type == "csv"){
    write.csv(x = data, file = file, fileEncoding = "shift-jis")
  }else if(file_type == "RData"){
    save(data, file = file)  
  }
  
  # Return
  return(file)
}


# ==== DATE TIME ====

## ---- *Convert datetime into string ----
convert_datetime_into_str <- function(datetime = Sys.time()) {
  dt <- datetime %>%
    gsub("-", "", .) %>% # Replace a blank between date and time with an under bar
    gsub(" ", "-", .) %>%
    gsub(":", "", .) %>% # Replace ":" with "-"
    as.character()
  return(dt)
}

# ==== For Loop ====

## ---- *Create progress bar on console ----
create_new_pb <- function(length)  {
  library(progress)
  progress_bar$new(
    format = "(:spin) [:bar] :percent [Elaspsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = length,
    complete = "=",
    incomplete = "-",
    current = ">",
    clear = FALSE,
    width = 100
  )
}
# 1. load package with "library(progress)"
# 2. BEFORE the for-loop, write "pb <- create_new_pb(length)"
# 3. INSIDE the for-loop, write "pb$tick()"

# ==== Machine Learning ====

## ---- *Slit data into training and test
create_train_test <- function(df, size = 0.8) {
  set.seed(42)
  sample <- sample(c(TRUE, FALSE), nrow(df), replace = TRUE, prob = c(size, 1-size))
  train <- df[sample,]
  test <- df[!sample,]
  list <- list(train = train, test = test)
  return(list)
}

# ==== Map ====

## ---- *Extract prefecture ----
extract_prefecture <- function(df) {
  pb <- create_new_pb(length(df))
  i <- 1
  df <- as.character(df)
  for(value in df) {
    pb$tick()
    if(regexpr("Œ§", value)>0){
      df[i] = substr(value, 1, regexpr("Œ§", value))
    }else if(regexpr("–kŠC“¹", value)>0){
      df[i] = "–kŠC“¹"
    }else if(regexpr("•{", value)>0){
      df[i] = substr(value, 1, regexpr("•{", value))
    }else if(regexpr("“Œ‹ž“s", value)>0){
      df[i] = "“Œ‹ž“s"
    }
    i = i + 1
  }
  return(df)
}
