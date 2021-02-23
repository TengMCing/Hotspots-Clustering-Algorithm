library(tidyverse)
library(spotoroo)

myhotspots <- read_csv("data/VIC_hotspots_raw.csv")

myhotspots <- select(myhotspots,
                     obsTime = `#obstime`, 
                     lon = lon, 
                     lat = lat)

result <- hotspot_cluster(myhotspots,
                          lon = "lon",
                          lat = "lat",
                          obsTime = "obsTime",
                          activeTime = 24,
                          adjDist = 3000,
                          minPts = 4,
                          minTime = 3,
                          ignitionCenter = "mean",
                          timeUnit = "h",
                          timeStep = 1)

saveRDS(result, file = "data/result.rds")