library(tidyverse)
library(spotoroo)

myhotspots <- read_csv(here::here("clustering_paper/data/VIC_hotspots_raw.csv"))

myhotspots <- select(myhotspots,
                     obsTime = `#obstime`, 
                     lon = lon, 
                     lat = lat)

for (adjDist in seq(1000, 10000, 1000)) {
  for (activeTime in seq(3, 36, 3)) {
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
    
    filename <- here::here(paste0("clustering_paper/data/grid_search/result_", adjDist, "_", activeTime, ".rds"))
    
    saveRDS(result, file = filename)
  }
}

tab <- expand.grid(adjDist = seq(1000, 10000, 1000) activeTime = seq(3, 36, 3), noise_prop = 1, count = 1)

for (adjDist in seq(1000, 10000, 1000)) {
  for (activeTime in seq(3, 36, 3)) {
    filename <- here::here(paste0("clustering_paper/data/grid_search/result_", adjDist, "_", activeTime, ".rds"))
    
    result <- readRDS(file = filename)
    
    tab$noise_prop[tab$adjDist == adjDist & tab$activeTime == activeTime] <- mean(result$hotspots$noise)
    tab$count[tab$adjDist == adjDist & tab$activeTime == activeTime] <- nrow(result$ignition)
    
  }
}

write_csv(tab, here::here("clustering_paper/data/grid.csv"))

