# Clustering

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

target_area = target_area[1:8]

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

# Run algorithm
test <- hotspots_clustering(con = my_db,
                            table_name = "HOTSPOTS",
                            method = cl_method,
                            adj_distance = cl_dist,
                            active_time = cl_active_time,
                            fire_mov_dir = my_fire_mov,
                            memory_saving = FALSE)


# *-------------------------------------------*
# Results of hotspots clustering:
#   
#   Observations:        1010794 
#   Clusters:            85848 
#   Timestamps:          4321 
#   Adjacency Distance:  3000 
#   Active Time:         24 
#   Method:              null 
#   Fire Movement DIR:   data/fire_mov.sqlite 
# 
# *-------------------------------------------*

DBI::dbDisconnect(my_db)

# Assign fire_id back to data
my_db_file <- "data/hotspots.sqlite"
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)

# Load hotspots from database
hotspots <- tbl(my_db, "HOTSPOTS") %>% collect()
hotspots$fire_id <- test$fire_id
DBI::dbDisconnect(my_db)

# Rewrite database
if (file.exists(my_db_file)) file.remove(my_db_file) 
my_db <- DBI::dbConnect(RSQLite::SQLite(), my_db_file)
DBI::dbWriteTable(my_db, "HOTSPOTS", hotspots)
DBI::dbDisconnect(my_db)
