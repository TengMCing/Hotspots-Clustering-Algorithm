library(tidyverse)
library(spotoroo)

myhotspots <- read_csv("data/VIC_hotspots_raw.csv")

myhotspots <- select(myhotspots,
                     obsTime = `#obstime`, 
                     lon = lon, 
                     lat = lat)

for (adjDist in seq(1000, 6000, 1000)) {
  for (activeTime in seq(6, 24, 6)) {
    result <- hotspot_cluster(myhotspots,
                              lon = "lon",
                              lat = "lat",
                              obsTime = "obsTime",
                              activeTime = activeTime,
                              adjDist = adjDist,
                              minPts = 4,
                              minTime = 3,
                              ignitionCenter = "mean",
                              timeUnit = "h",
                              timeStep = 1)
    
    filename <- paste0("data/grid_search/result_", adjDist, "_", activeTime, ".rds")
    
    saveRDS(result, file = filename)
  }
}