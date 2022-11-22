## ==== LOADING ====
library(tidyverse)
library(ggplot2)
library(GGally)
library(readr)
library(progress)

## ==== IMPORT DATA ====
data_import <- read.csv("products_20221119-172528.csv", fileEncoding = "shift-jis")  %>%
  as.data.frame()

## ==== SHAPE DATA =====
glimpse(data_import)
data_shaped <- data_import %>%
  select(-c("X")) %>%
  mutate(
    datetime = as.Date(datetime),
    area = factor(area),
    tag = factor(tag),
    producer = factor(producer),
    hasGAP = factor(hasGAP),
    hasOrganic = factor(hasOrganic),
    hasNoPesticide = factor(hasNoPesticide),
    hasJAS = factor(hasJAS)
  )

# Extract prefecture

i <- 1
create_new_pb <- function(length){
  pb <- progress_bar$new(
    format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = length,
    complete = "=",
    incomplete = "-",
    current = ">",
    clear = FALSE,
    width = 100
  )
  return(pb)
}
pb <- create_new_pb(nrow(data_shaped))
for(area in as.character(data_shaped$area)) {
  pb$tick()
  if(regexpr("Œ§", area)>0){
    data_shaped$pref[i] = substr(area, 1, regexpr("Œ§", area))
  }else if(regexpr("“¹", area)>0){
    data_shaped$pref[i] = substr(area, 1, regexpr("“¹", area))
  }else if(regexpr("•{", area)>0){
    data_shaped$pref[i] = substr(area, 1, regexpr("•{", area))
  }else if(regexpr("“s", area)>0){
    data_shaped$pref[i] = substr(area, 1, regexpr("“s", area))
  }
  
  i = i + 1
}

data_shaped$pref <- factor(data_shaped$pref)


glimpse(data_shaped)
summary(data_shaped)
data_shaped$pref

## ==== SAVE AS .Rds FILE ====
saveRDS(data_shaped, file = "tabechoku-20221119.Rds")
data_copied <- readRDS(file = "tabechoku-20221119.Rds")
head(data_copied)
glimpse(data_copied)

## ==== PLOT DATA ====
# 1. Load packages: "tidyverse" (handling df), "ggplot2" (plotting data). "dplyr" (filtering dataframe)
library(tidyverse) # Handling df
library(ggplot2) # Plotting data
library(dplyr) # Filtering df
# 2. Select columns and plot
data_copied %>%
  dplyr::select(price, tag, reviewCount, hasGAP, hasJAS, hasNoPesticide, hasOrganic) %>% #Describe WITHOUT doble quotation("). IF you include all columns, drop this row.
  ggpairs(
    mapping = aes(color = hasJAS, alpha = 0.6), # Describe factor col name that shows as color variation (also write without double-quotation)
    lower = list(continuous = "smooth", combo = "facetdensity"),
    diag = list(continuous = "barDiag")
  )

## ==== PLOT MAP ====
# Packages ----
# install.packages("maps")
# devtools::install_github("uribo/jpndistrict") #Install from github
# install.packages("countrycode")
# library(tidyverse) # Data handling
# library(ggplot2) # Plotting
# library(maps) # World map data
# library(jpndistrict) # Japanese prefectural map data
# library(countrycode) # World country code data

# Test ----
## World country data
world <- map_data("world")
glimpse(world)
world %>%
  # filter(region == "Japan") %>% # Filtering the area to show
  ggplot(aes(x = long, y = lat, group = group)) + # group: block to show as the same one
  geom_polygon(fill = "lightgray", colour = "white", size = 0.1) # fill: color to fill a map, colour: border-color, size: border-size

## Prefectural data
glimpse(jpnprefs)

## City data in jp
kyoto <- jpn_cities(26) # Get city list in the pref with prefecture code inside ()
head(kyoto)
kyoto %>%
  ggplot() +
  geom_sf() # Use geom_sf because the border line written with "simple features (sf)"

## City coloring
head(population) # Population data in tidyr package
pop2013 <- population %>%
  filter(year == 2013) %>%
  rename(region = country)
pop2013 <- pop2013 %>%
  mutate(
    iso3c = countrycode(
      sourcevar = region, origin = "country.name", destination = "iso3c"
    )
  )
world <- world %>%
  mutate(
    iso3c = countrycode(
      sourcevar = region, origin = "country.name", destination = "iso3c"  
    )
  )

world %>%
  filter(is.na(iso3c) == TRUE) %>%
  select(region) %>%
  distinct()
left_join(world, pop2013, by = "iso3c") %>%
  ggplot() +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = log(population)
    ),
    colour = "black",
    size = 0.1
  ) +
  scale_fill_continuous(
    name = "Population(log)",
    low = "white",
    high = "darkblue"
  )
kyoto %>%
  ggplot() +
  geom_sf(
    aes(
      fill = city
    ),
    show.legend = TRUE
  )
glimpse(kyoto)
##
# Plot price data----
df <- data_copied
glimpse(df)
glimpse(jpnprefs)

df_new <- merge(df, jpnprefs, by.x = "pref", by.y = "prefecture", all.x = T)
glimpse(df_new)

# 1. Load "progress" package.
library(progress)
# 2. Set create_new_pb(length) function 
create_new_pb <- function(length){
  pb <- progress_bar$new(
    format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
    total = length,
    complete = "=",
    incomplete = "-",
    current = ">",
    clear = FALSE,
    width = 100
  )
  return(pb)
}
# 3. Right before for-loops, set a progress-bar. Note that "length" usually goes like "nrow(df)".
pb <- create_new_pb(length(unique(df_new$jis_code)))
df <- NULL
for(code in unique(df_new$jis_code)){
  # 4. Inside for-loops, describe "pb$tick()"
  pb$tick()
  df_add <- jpn_pref(code)
  if(length(df) > 0){
    df <- rbind(df, df_add)
  }else {
    df <- df_add
  }
}

summary(df)
glimpse(data_copied)

df_grp <- df_new[,c("jis_code", "price", "reviewCount")] %>%
  group_by(jis_code) %>%
  summarize(
    total_price = sum(price),
    av_price = mean(price),
    stdv_price = sd(price),
    total_reviews = sum(reviewCount),
    av_reviews = mean(reviewCount),
    stdv_reviews = sd(reviewCount)
  )
glimpse(df_grp)
  
df <- merge(df, df_grp , by.x = "pref_code", by.y = "jis_code", all.x = T )
glimpse(df)

# Plotting
par(mfrow = c(3,2))
df %>%
  ggplot() +
  geom_sf(
    aes(
      group = pref_code,
      fill = total_price
    ),
    colour = "lightgray",
    size = 0,
    show.legend = TRUE
  ) + 
  scale_fill_continuous(
    name = "Total Price",
    low = "white",
    high = "darkblue"
  )
df %>%
  ggplot() +
  geom_sf(
    aes(
      group = pref_code,
      fill = total_reviews
    ),
    colour = "lightgray",
    size = 0,
    show.legend = TRUE
  ) + 
  scale_fill_continuous(
    name = "Total Reviews",
    low = "white",
    high = "darkblue"
  ) + 
  theme(
    panel.background = element_blank()
  )
##
df %>%
  ggplot() +
  geom_sf(
    aes(
      group = pref_code,
      fill = av_price
    ),
    colour = "lightgray",
    size = 0,
    show.legend = TRUE
  ) + 
  scale_fill_continuous(
    name = "Average Price",
    low = "white",
    high = "darkblue"
  ) + 
  theme(
    panel.background = element_blank()
  )
df %>%
  ggplot() +
  geom_sf(
    aes(
      group = pref_code,
      fill = av_reviews
    ),
    colour = "lightgray",
    size = 0,
    show.legend = TRUE
  ) + 
  scale_fill_continuous(
    name = "Average Reviews",
    low = "white",
    high = "darkblue"
  ) + 
  theme(
    panel.background = element_blank()
  )
##
df %>%
  ggplot() +
  geom_sf(
    aes(
      group = pref_code,
      fill = stdv_price
    ),
    colour = "lightgray",
    size = 0,
    show.legend = TRUE
  ) + 
  scale_fill_continuous(
    name = "Price STDV",
    high = "white",
    low = "darkblue"
  ) + 
  theme(
    panel.background = element_blank()
  )
##
df %>%
  ggplot() +
  geom_sf(
    aes(
      group = pref_code,
      fill = stdv_reviews
    ),
    colour = "lightgray",
    size = 0.001,
    show.legend = TRUE
  ) + 
  scale_fill_continuous(
    name = "Reviews STDV",
    high = "white",
    low = "darkblue"
  ) + 
  theme(
    panel.background = element_blank()
  )
##
par(mfrow = c(1,1))