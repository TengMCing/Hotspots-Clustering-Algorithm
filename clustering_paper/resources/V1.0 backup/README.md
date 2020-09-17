# A clustering algorithm to organize satellite hotspots data

# Reference 

## clustering.R

```r
Clustering Algorithm

Default:                                                                                

hotspots_clustering(con, method = "null", adj_distance = 3e3, active_time = 24, fire_mov_dir = "", memory_saving = FALSE, save_point = 1000)         
                                                                                         
Arguments:                                                                              
                                                                                         
  con:           a dbConnect() (SQLite) object.
  table_name:    a table name of the hotspots data. It should contains 4 and only 4 columns,
                  which are id, lon, lat and time_id
  method:        clustering method. Either "null", "mean_r", or 'max_r'.
  adj_distance:  distance (0 to 1e5 m) for two vertices to be considered as adjacent.
  active_time:   units of time a clusters remain active.
  fire_mov_dir:  a database dir to store fire movement result. Database will be overwrited.
  memory_saving: (TRUE or FALSE) occasionally save data to database to release memory.
  save_point:    only useful when memory_saving is TRUE. save data per 'save_point' number of steps.
  
Value:
  
  A clusteringResult class inherit from list with a customize print method. It contains clstuering result and other attributes used in this algorithm.
```

## main.R

Examples of hyperparameters for this algorithm can be found in this script. Combining with the algorithm, this script can handle enormous clustering task like grouping 1 million hotspots across Australia in 30 minutes. If it only works on hotspots in Victoria, running time will reduce to only 3 mins.

## grid.R

Produce all results using interested combinations of hyperparameters

## Packages

```r
tidyverse
sf
furrr
lubridate
rnaturalearth
igraph
here
DBI
RSQLite
progress
```

## Data

[ptree](https://www.eorc.jaxa.jp/ptree/index.html)

jaxa himawari-8 satellite hotspots data

