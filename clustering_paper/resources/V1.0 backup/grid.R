# Produce grid results

# Load libraries
library(tidyverse)
library(sf)
library(furrr)
library(lubridate)
library(rnaturalearth)
library(igraph)
library(here)

# From stackoverflow user 'Juan Bernabe' 
# https://stackoverflow.com/questions/47044068/get-the-path-of-current-script/47045368

getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

# Source path
clustering_source <- paste0(getCurrentFileLocation(), '/clustering.R')

source(clustering_source)

####################################################################
# Hyperparameters

# Start date and end date of hotspots data (yyyymmdd)
start_date <-  "20191001"
end_date <-  "20200331"

# Hotspots data location
hotspots_data_dir <-  paste0(here(),'/data/Himawari-8')

# Firepower filtering point
left_truncation_firepower <-  100

# Target area
target_area <-  c('Victoria',
                  'New South Wales',
                  'Northern Territory',
                  'South Australia',
                  'Western Australia',
                  'Queensland',
                  'Tasmania',
                  'Australian Capital Territory')

target_area = target_area[1:1]

# Clustering distance(m)
cl_dist <-  3000

# Clustering method
cl_method <-  c("mean_r", 'max_r', 'null')
cl_method <-  cl_method[3]

# Output dir
my_fire_mov <- "data/fire_mov.sqlite"

# Active time
cl_active_time <- 24


####################################################################



# Read in hotspots data
read_hotspots <- function(directory = "data/Himawari-8", start = '20190501', end = '20190701'){
  
  # Read in hotspots data from 'start' date to 'end' date
  
  fls_full <-  list.files(directory, full.names = TRUE)
  fls_short <-  list.files(directory, full.names = FALSE)
  
  fls_filter <-  (fls_short > paste('H08', start, sep = '_')) & (fls_short < paste('H08', end, sep = '_'))
  fls_full <-  fls_full[fls_filter]
  
  all <-  map(fls_full, read_csv, quote="'")
  d <-  bind_rows(all)
  
  return(d)
  
}



# Prepare hotspots data
data_preparation <- function(start_date = start_date,
                             end_date = end_date,
                             hotspots_data_dir = hotspots_data_dir,
                             left_truncation_firepower = left_truncation_firepower,
                             target_area = target_area){
  
  # read in data
  print('1/5 Read in hotspots data.')
  hotspots <-  read_hotspots(start = start_date, end = end_date, directory = hotspots_data_dir)
  
  # Select hotspots in Australia
  print('2/5 Select hotspots in Australia.')
  hotspots <-  hotspots %>%
    filter(between(lon, 112, 155)) %>% 
    filter(between(lat, -44, -10))
  
  
  
  # Load map of australia
  au_map <-  ne_states(country = 'Australia', returnclass = 'sf')
  
  # Filtering hotspots by firepower
  print('3/5 Filtering hotspots by firepower.')
  hotspots <-  hotspots %>%
    filter(firepower > left_truncation_firepower)
  
  
  
  # Transform hotspots to sf object
  hotspots <- st_as_sf(x = hotspots, coords = c('lon','lat'), crs = 4326)
  
  # Filtering hotspots by target states
  print('4/5 Filtering hotspots by target states.')
  states <-  st_intersects(au_map$geometry, hotspots$geometry)
  hotspots$state <-  ''
  
  for (i in seq(1,nrow(au_map))){
    
    hotspots$state[states[[i]]] <-  au_map$name[i]
    
  }
  
  hotspots <- hotspots %>%
    filter(state %in% target_area)
  
  # Convert hotspots data back to data frame
  coords <- st_coordinates(hotspots)
  st_geometry(hotspots) <- NULL
  hotspots$lon = coords[,1]
  hotspots$lat = coords[,2]
  
  # Extract time features and assign hour id
  print('5/5 Assign hour id to hotspots data.')
  hotspots$year <-  year(hotspots$`#obstime`)
  hotspots$month <-  month(hotspots$`#obstime`)
  hotspots$day <-  day(hotspots$`#obstime`)
  hotspots$week <-  week(hotspots$`#obstime`)
  hotspots$hour <-  hour(hotspots$`#obstime`)
  
  hotspots <- arrange(hotspots, year, month, day, hour)
  
  hotspots$hour_id <- difftime(hotspots$`#obstime`, min(hotspots$`#obstime`), units = "hour") %>%
    as.numeric() %>%
    round() %>%
    as.integer()
  
  hotspots$hour_id <- hotspots$hour_id + 1
  
  print('Hotspots data preprocessing finished!')
  
  return(hotspots)
  
}


# preprocess hotspots data
hotspots <- data_preparation(start_date = start_date,
                             end_date = end_date,
                             hotspots_data_dir = hotspots_data_dir,
                             left_truncation_firepower = left_truncation_firepower,
                             target_area = target_area)

# Save enitre hotspots data in database
my_db_file <- "data/hotspots.sqlite"
if (file.exists(my_db_file)) file.remove(my_db_file)
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
DBI::dbWriteTable(my_db, "HOTSPOTS", hotspots)
RSQLite::dbDisconnect(my_db)


# Select coordinates and hour_id
hotspots <- hotspots %>%
  mutate(time_id = hour_id) %>%
  mutate(id = 1:nrow(hotspots)) %>%
  select(id, lon, lat, time_id)

# Save trimmed hotspots data in database
my_db_file <- "data/hotspots-trimmed.sqlite"
if (file.exists(my_db_file)) file.remove(my_db_file) 
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
DBI::dbWriteTable(my_db, "HOTSPOTS", hotspots)
RSQLite::dbDisconnect(my_db)
rm(hotspots)

# Reconnect to the database
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)

options(dplyr.summarise.inform = FALSE)

#######################################################################################

# Test different hyperparameters

method_sets <- c("null", "mean_r", "max_r")
dist_sets <- c(1500, 3000, 6000, 12000, 24000, 48000)
active_time_sets <- c(3, 6, 12, 24, 48, 96)

temp_max <- tbl(my_db, sql(paste0("SELECT COUNT(time_id) FROM ", "HOTSPOTS"))) %>%
  collect() %>%
  .[[1]]

results_grid <- tibble(id = 1:temp_max)

rm(temp_max)

# Get results using different hyperparameters

for (cl_method in method_sets){
  for (cl_dist in dist_sets){
    for (cl_active_time in active_time_sets){
      print(paste0("Computing ", cl_method, '-', cl_dist, '-', cl_active_time))

      my_fire_mov <- paste0("data/fire_mov/", cl_method, '-', cl_dist, '-', cl_active_time, ".sqlite")
      
      # Run algorithm
      test <- hotspots_clustering(con = my_db,
                                  table_name = "HOTSPOTS",
                                  method = cl_method,
                                  adj_distance = cl_dist,
                                  active_time = cl_active_time,
                                  fire_mov_dir = my_fire_mov,
                                  memory_saving = FALSE,
                                  save_point = 1000)
      
      print(test)
      
      results_grid[[paste0(cl_method, '_', cl_dist, '_', cl_active_time)]] <- test$fire_id
      
    }
  }
}

results_grid <- tbl(my_db, "HOTSPOTS") %>%
  collect() %>%
  left_join(results_grid, by = 'id')

write.csv(results_grid, "data/grid_results.csv", row.names = FALSE)



