# ==== PACKAGES====
# Table
library(tidyverse) # Data frame manupulation
library(data.table) # Interactive data table
library(DataExplorer) # Visualize data as summary report
library(skimr) # Skimming for checking nas
# Plot
library(ggplot2) # Rich plotting
library(GGally) # Multiple variables visualization
# String
library(glue) # Concatnate strings as natural


# ==== FUNCTIONS ====
source("./R/functions.R")

# ==== VARIABLES ====
# Files
data.dir <- "./data"
tabe.dir <- glue("{data.dir}/tabechoku")
poke.dir <- glue("{data.dir}/pokemaru")
