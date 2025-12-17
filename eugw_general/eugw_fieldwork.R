# Packages
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(caret)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# Fieldwork
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# load field data
g <- st_read("Milovice_Mlada/MM_field_data.gpkg")

# focal col in field data
col <- "EUNIS_LC"

# count N and proportions of recognized cats
tab <- g %>%
  st_drop_geometry() %>%
  mutate(
    value = as.character(.data[[col]]),
    value = ifelse(is.na(value), "NA", value)
  ) %>%
  count(value, name = "n") %>%
  mutate(proportion = n / sum(n))

# cats in proportion barplot
ggplot(tab, aes(x = "All", y = proportion, fill = value)) +
  geom_col(width = 0.6) +
  labs(
    x = "",
    y = "Proportion",
    fill = col,
    title = paste("Field data, n = 111")
  ) +
  theme_bw()

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# Fieldwork with Model
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# Comparision of recent (2025) fieldwork data and most recent EUGW classification raster delivered (2023)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

v <- st_read("Milovice_Mlada/MM_field_data.gpkg")      
r  <- rast("Milovice_Mlada/EUGW_data/CZ_CON_CON_75111_20230101_20231231_GTYH_CLASS.tif")
pts <- vect(v)

# which class was predicted by EUGW model on given locality
v$predicted <- terra::extract(r, pts)[,2] # drop ID, keep GT class

# create df for subsequent analysis
df <- v %>%
  st_drop_geometry() %>%
  mutate(
    observed = .$GT,             # Grassland type ID: 21–27
    predicted = predicted
  )

# make sure, NAs as characters
df$predicted <- ifelse(is.na(df$predicted), "NA", df$predicted)
df$observed  <- ifelse(is.na(df$observed),  "NA", df$observed)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# IMPORTANT # since Sparsely wooded grassland (GT 27) cannot be found in Czechia,
#             in some analyses a treated them as NA

# df$observed  <- ifelse(df$observed == 27,  "NA", df$observed)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# results
table_cm <- table(df$observed, df$predicted)
table_cm

# picture
labels_map <- c(
  "21" = "Dry grassland",
  "22" = "Mesic grassland",
  "23" = "Wet and seasonally wet grassland",
  "24" = "Alpine and sub-alpine grassland",
  "25" = "Forest clearings",
  "26" = "Inland salt steppes",
  "27" = "Sparsely wooded grassland",
  "NA" = "Unclassified"
)
# compute proprtions and total numbers
tab <- df %>%
  count(observed, predicted) %>%
  group_by(observed) %>%
  mutate(prop = n / sum(n))# %>%
#filter(observed != "27")
count_obs <- tab %>%
  dplyr::group_by(observed) %>%
  summarise(n = sum(n))# %>%
#filter(observed != "27")

# plot it
ggplot(tab, aes(x = factor(observed), y = prop, fill = factor(predicted))) +
  geom_col() +
  geom_text(
    data = count_obs,
    aes(x = factor(observed), y = 1.02, label = paste0("n = ", n)),
    inherit.aes = FALSE,
    size = 3
  ) +
  labs(
    x = "Observed class",
    y = "Proportion of predictions",
    fill = "Predicted class",
    title = "Field data ~ EUGW typology output"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_discrete(labels = labels_map) +      
  scale_fill_manual(
    values = c(
      "21" = "#f5ca7a",
      "22" = "#a5f57a",
      "23" = "#7ab6f5",
      "24" = "#ca7af5",
      "25" = "#5c8944",
      "26" = "#f57a7a",
      "27" = "#895a44",
      "NA" = "#cccccc"
    ),
    labels = labels_map                         
  )
