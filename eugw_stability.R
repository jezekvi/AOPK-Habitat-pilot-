# Packages
library(sf)
library(dplyr)
library(ggplot2)
library(terra)
library(caret)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# Stability 1
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# Count number of cells (and their area) which do at least 1 classification
# transition versus those, which are stable (have the same class across all rasters)

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# load data, output from the senkey workflow
r <- rast("stable_pixels.tif")

# cell area
cell_area <- prod(res(r))

# number of cells with value 1 STABLE
n_1 <- global(r == 1, "sum", na.rm = TRUE)[1]

# number of cells with value 0 UNSTABLE
n_0 <- global(r == 0, "sum", na.rm = TRUE)[1]

# total number of cels
n_total <- n_1 + n_0

# area
area_total <- n_total * cell_area
area_1     <- n_1 * cell_area
area_0     <- n_0 * cell_area

area_1_ha <- area_1 / 10000
area_0_ha <- area_0 / 10000
area_total_ha <- area_total / 10000

area_1_ha # stable
area_0_ha # unstable
area_total_ha

# percent
pct_1 <- (n_1 / n_total) * 100 # stable
pct_0 <- (n_0 / n_total) * 100 # unstable
pct_1
pct_0

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# Stability 2
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# Numerical expression of pixel classification changes
# This time focusing on count of each pixel tranisitions during whole period

# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

# load data, output from the senkey workflow
r <- rast("nchanges_per_pixel.tif")

# cell size
cell_area <- prod(res(r))

# get all raster values (without NA)
vals <- sort(unique(values(r)))
vals <- vals[!is.na(vals)]

# prepare df for further analysis
res <- data.frame(
  value    = vals,
  n_cells  = NA_real_,  # numeric
  area_m2  = NA_real_,
  pct      = NA_real_
)

# for loop over all unique raster values
for (i in seq_along(vals)) {
  # vals
  v <- vals[i]
  # cell count with given value
  n_v <- as.numeric(global(r == v, "sum", na.rm = TRUE))
  res$n_cells[i] <- n_v
  res$area_m2[i] <- n_v * cell_area
}

# total number of valid cells (without NA) = sum of all cat
n_total <- sum(res$n_cells, na.rm = TRUE)

# compute percents
res$pct <- (res$n_cells / n_total) * 100

# total valid area (without NA) = sum of all cat
total_clean_area <- sum(res$area_m2, na.rm = TRUE)

# result but in ha
res$area_ha <- res$area_m2 / 10000

# results
res
total_clean_area

# informative plot
ggplot(res, aes(x = as.factor(value), y = area_ha)) +
  geom_col() +
  labs(
    x = "Number of changes",
    y = "Area [ha]"
  ) +
  theme_classic() 
