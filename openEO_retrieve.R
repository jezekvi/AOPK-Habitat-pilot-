# Load required libraries
library(openeo)
library(sf)
library(dplyr)

# Connect to openEO backend
connection <- connect("https://openeo.dataspace.copernicus.eu")
login()

# Read polygons from file

aapa_polygons <- st_read("data/reka.gpkg") %>% st_transform(crs = 4326)

# Define bands to download
bands <- c("B02", "B03", "B04", "B05", "B08", "B8A", "B11", "B12", "SCL")


# Submit all jobs and store them in a list
jobs <- list()

###############################################################
# used to be for cylcle
polygon <- aapa_polygons
bbox <- st_bbox(polygon)
  
# Get process graph builder
process <- processes()

# Define spatial extent
spatial_extent <- list(
  west = bbox["xmin"],
  south = bbox["ymin"],
  east = bbox["xmax"],
  north = bbox["ymax"],
  crs = 4326
)

# Define temporal extent (all available years)
temporal_extent <- list("2015-01-01", "2025-12-31")

# Load Sentinel-2 L2A data using process graph builder
collection <- process$load_collection(
  id = "SENTINEL2_L2A",
  spatial_extent = spatial_extent,
  temporal_extent = temporal_extent,
  bands = bands
)

# Save the result using process$save_result
result <- process$save_result(data = collection, format = "NetCDF")

# Create the batch job
job <- create_job(graph = result, title = paste0("polygon_novareka"))

# Submit the job
#job$send_job()

#jobs[[i]] <- job
#cat("Job for polygon", i, "submitted.\n")
###############################################################


list_jobs() |> names() -> job_names
start_job(job_names[1])
list_jobs()

describe_job(job_names[1])
log_job(job_names[1])


# for(i in 2:30) {
#   start_job(job_names[i])
# }

list_jobs() |> as_tibble() |> View()

list_jobs() |> as_tibble() |> count(status)

list_jobs() |> as_tibble() -> jobs_df
jobs_df

id <- unlist(jobs_df[1,"id"])
name <- paste0("S2/", gsub(" ", "_", unlist(jobs_df[1, "title"])), ".nc")
print(name)
download_results(id, folder = "S2/") -> downname
file.rename(unlist(downname), name)
