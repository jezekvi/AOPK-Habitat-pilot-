library(tidyverse)
library(sf)
library(stars)
library(mapview)

# site polygons
aapa_polygons <- 
  st_read("data/reka.gpkg") |> 
  st_transform(32633)

# Sentinel-2 SCL class codes and definitions
dplyr::tibble(
  SCL = 0:11,
  SCL_name = c(
    "No data",
    "Saturated or defective",
    "Dark area pixels",
    "Cloud shadow", # in spring water pools might get this flag if there are clouds in scene
    "Vegetation",
    "Bare soils",
    "Water",
    "Cloud low probability / Unclassified", # flarks mistakenly get this flag in Finnish mires 
    "Cloud medium probability",
    "Cloud high probability",
    "Thin cirrus",
    "Snow or ice"
  )
) -> scl_code
print(scl_code)

# for testing read one of the datacubes
obj <- read_stars("S2/reka_small.nc") 

## Datacube subset of relevant season
time_vals <- st_get_dimension_values(obj, "t") # Extract dates
keep_idx <- which(as.integer(format(time_vals, "%m")) %in% 4:9) # indices where the month is between 4 and 9
st_dimensions(obj)
obj <- obj[,,,keep_idx] # datacube dimensions: obj[bands, x_coord, y_coord, time]

#
# Cloud masking
#

# Find all scenes with at least 95% of pixels classified as SCL 4, 5, or 6 (vegetation, bare soils, water)
# (In Finland: SCL 4,5,6, or 7  - Unclassified/low cloud probability. In Finnish case mask low probability clouds with CLD layer)

# Alos mask out all remaining pixels with SCL values not equal to 4, 5, or 6 
clear_dates <-
  obj |> 
  as_tibble() |> 
  group_by(t) |> 
  summarize(prop_scl = sum(if_else(SCL %in% c(4,5,6, 7), 1, 0)) / n()) |> 
  filter(prop_scl > 0.95) |> 
  pull(t) 

obj_clear <-
  obj |>
  filter(t %in% clear_dates) |>
  mutate(across(everything(), ~ if_else(SCL %in% c(4, 5, 6, 7), ., NA)))

# Mask out the polygon
obj_poly <-
  obj_clear |>
  st_crop(aapa_polygons) # 3 - index of the site used for testing

plot(obj_poly[7]) # 7 - SWIR band

#
# Calculate wetness classification and moisture indices
#

# Inundation function Jussila [11]
obj_wetness <-
  obj_poly |>
  mutate(inundation_Jussila = case_when(
    is.na(B11) ~ NA_real_,
    B11 < 1396 & B8A < 1817 ~ 1,
    B11 < 1247 & B8A >= 1817 ~ 1,
    B11 >= 1396 & B04 < 391 & ((B03 - B12) / (B03 + B12)) < -0.43 & B12 < 1496 ~ 1,
    TRUE ~ 0
  ))
# Calculate moisture indices [12, 13, 14, 15]
obj_wetness <-
  obj_wetness |>
  mutate(NDMI = (B8A - B11) / (B8A + B11), # Gao's Moisture index
         NDWI = (B03 - B8A) / (B03 + B8A), # Mcfeeters Water index
         NDPI = (B03-B11)/(B03+B11), # Pond index. from slovakia Clizsky potok example 
         STR = ((1-B11/10000)^2)/(2*B11/10000) ) # Transformed SWIR. Should be linearly correlated with soil moisture (Sadeghi et al.,2017, https://doi.org/10.1016/j.rse.2017.05.041)

# plot rasters [band, spatX, spatY, time]

pal <- grDevices::colorRampPalette(c("white", "#deebf7", "#9ecae1", "#3182bd", "#08519c"))(200)
plot(obj_wetness[12,,,174:193], col = pal)

plot(obj_wetness[10,,,174:193], col = c("#e0c072", "#7fc8d6"))

# years
plot(obj_wetness[10,,,1:9],   col = c("#e0c072", "#7fc8d6"))   # 2015
plot(obj_wetness[10,,,10:15], col = c("#e0c072", "#7fc8d6"))   # 2016
plot(obj_wetness[10,,,16:39], col = c("#e0c072", "#7fc8d6"))   # 2017
plot(obj_wetness[10,,,40:70], col = c("#e0c072", "#7fc8d6"))   # 2018
plot(obj_wetness[10,,,71:99], col = c("#e0c072", "#7fc8d6"))   # 2019
plot(obj_wetness[10,,,100:130], col = c("#e0c072", "#7fc8d6")) # 2020
plot(obj_wetness[10,,,131:150], col = c("#e0c072", "#7fc8d6")) # 2021
plot(obj_wetness[10,,,151:173], col = c("#e0c072", "#7fc8d6")) # 2022
plot(obj_wetness[10,,,174:196], col = c("#e0c072", "#7fc8d6")) # 2023
plot(obj_wetness[10,,,197:215], col = c("#e0c072", "#7fc8d6")) # 2024
plot(obj_wetness[10,,,216:248], col = c("#e0c072", "#7fc8d6")) # 2025

#ndwi
plot(obj_wetness[12,,,1:9],   col = pal)   # 2015
plot(obj_wetness[12,,,10:15], col = pal)   # 2016
plot(obj_wetness[12,,,16:39], col = pal)   # 2017
plot(obj_wetness[12,,,40:70], col = pal)   # 2018
plot(obj_wetness[12,,,71:99], col = pal)   # 2019
plot(obj_wetness[12,,,100:130], col = pal) # 2020
plot(obj_wetness[12,,,131:150], col = pal) # 2021
plot(obj_wetness[12,,,151:173], col = pal) # 2022
plot(obj_wetness[12,,,174:196], col = pal) # 2023
plot(obj_wetness[12,,,197:215], col = pal) # 2024
plot(obj_wetness[12,,,216:248], col = pal) # 2025

#
# PLOT TIME SERIES GRAPH (WITH ALL OBSERVATIONS)
#

# check dates and date indices if you want to plot a specific time period 
st_get_dimension_values(obj_poly, "t")

# summarise inundation-% and moisture index averages for site area 
table_all <- # ("prop_inundation" in the original Analyze_aapa.R)
  obj_wetness[,,,] |>  # adjust time range with 4th dimension, eg.: obj_wetness[,,,1:15] -> first 15 dates. 
  as_tibble() |>
  group_by(t) |>
  summarize(inundation = sum(inundation_Jussila, na.rm = TRUE) / n(),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture. 

ggplot(table_all) +
  aes(x = t, y = inundation) +
  geom_line() + geom_point() +
  labs(title = "Inundation %",
       x = "Date",
       y = "Inundated area (%)") +
  theme_minimal()

ggplot(table_all) +
  aes(x = t, y = NDMI) + 
  geom_line() + geom_point() +
  labs(title = "NDMI",
       x = "Date",
       y = "NDMI") +
  theme_minimal()

ggplot(table_all) +
  aes(x = t, y = -B11) + # plot SWIR negation for easier comparison with other indicators. 
  geom_line() + geom_point() +
  labs(title = "SWIR B11",
       x = "Date",
       y = "B11") +
  theme_minimal()

#
# AGGREGATION 
#

## Temporal aggregation: monthly median value

obj_month<- aggregate(obj_wetness, by = "month", FUN=median, na.rm=T) # or "week" 
plot(obj_month[11])
plot(obj_month[12])
plot(obj_month[10])
# plot(obj_month[4])

## Temporal aggregation: 2weekly median value with customized approach

# generate 2-week breaks for the observed years
time_vals <- st_get_dimension_values(obj_wetness, "t")
start <- floor_date(min(time_vals), "month")
end   <- ceiling_date(max(time_vals), "month")
# All 1st and 15th of each month between start and end
breaks <- sort(c(seq(start, end, by = "1 month"),
                 seq(start + days(14), end, by = "1 month")))
# Aggregate using 2-week intervals
obj_2week <- aggregate(obj_wetness, by = breaks, FUN = median, na.rm = TRUE)

# # remove winter months again: find indices where the month is between 4 and 9
# # (for graph plotting it actually visualises better with all months included)
# keep_idx <- which(as.integer(format(st_get_dimension_values(obj_2week, "time"), "%m")) %in% 4:9)
# obj_2week <- obj_2week[,keep_idx,,]   # adjust commas for your dimensions

#
# PLOT TIME SERIES GRAPH (aggregated)
#

# check date labels and their indices if you want to plot a specific time period 
st_get_dimension_values(obj_month, "time")

# summarise inundation-% and moisture index averages for site area 
table_m <- # ("prop_inundation" in the original Analyze_aapa.R)
  obj_month[,,,] |>  # adjust time range with 2nd dimension, eg.: obj_wetness[,1:4,,] -> first 4 months 
  as_tibble() |>
  group_by(time) |> 
  summarize(inundation = sum(inundation_Jussila, na.rm = TRUE) / n(),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture. 

ggplot(table_m) +
  aes(x = time, y = inundation) +
  geom_line() + geom_point() +
  labs(title = "Inundation %",
       x = "Date",
       y = "Inundated area (%)") +
  theme_minimal()

ggplot(table_m) +
  aes(x = time, y = NDMI) + 
  geom_line() + geom_point() +
  labs(title = "NDMI",
       x = "Date",
       y = "NDMI") +
  theme_minimal()

ggplot(table_m) +
  aes(x = time, y = -B11) + # plot SWIR negation for easier comparison with other indicators. 
  geom_line() + geom_point() +
  labs(title = "SWIR B11",
       x = "Date",
       y = "B11") +
  theme_minimal()
