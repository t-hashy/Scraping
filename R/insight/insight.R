# ==== IMPORT DATA ====

# Get data frame files
dffile.tabe <- list.files("./data/tabechoku") %>%
  grep("df", ., value = TRUE)
dffile.poke <- list.files("./data/pokemaru") %>%
  grep("df", ., value = TRUE)

# Import the data
tabe <- readRDS(glue("./data/tabechoku/{dffile.tabe}"))
poke <- readRDS(glue("./data/pokemaru/{dffile.poke}"))

# ==== FIX THE DATA TYPES ====
# Get numeric column names
numcols.tabe <- append(
  grep("price", names(tabe), ignore.case = TRUE, value = TRUE),
  grep("cost", names(tabe), ignore.case = TRUE, value = TRUE)
)
numcols.poke <- append(
  grep("price", names(poke), ignore.case = TRUE, value = TRUE),
  grep("cost", names(poke), ignore.case = TRUE, value = TRUE)
) %>%
  append(grep("days", names(poke), ignore.case = TRUE, value = TRUE))

# Get numeric columns
tabe.numcols <- tabe[,numcols.tabe]
poke.numcols <- poke[,numcols.poke]

# Convert as numeric
index <- 0
col.names <- names(tabe.numcols)
for(col in tabe.numcols){
  index = index + 1
  tabe[col.names[index]] <- as.numeric(col)
}
index <- 0
col.names <- names(poke.numcols)
for(col in poke.numcols){
  index = index + 1
  poke[col.names[index]] <- as.numeric(col)
}

# Update the variabes
tabe.numcols <- tabe[,numcols.tabe]
poke.numcols <- poke[,numcols.poke]

# ==== See the data of raw ====
create_report(poke, output_file = "rep_poke", output_dir = poke.dir)
create_report(tabe, output_file = "rep_tabe", output_dir = tabe.dir)
