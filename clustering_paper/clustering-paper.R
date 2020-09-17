## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE, 
  cache=FALSE, 
  message=FALSE, 
  warning=FALSE, 
  fig.retina = 3, 
  out.width="80%")


## ----libraries----------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(sf)
library(ggpubr)
library(rnaturalearth)
library(lubridate)


## -----------------------------------------------------------------------------
au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]
hotspots <- read_csv("resources/data/VIC_hotspots_raw.csv")
memberships <- read_csv("resources/data/VIC_hotspots_after_clustering.csv")
hotspots$fire_id <- memberships$fire_id
rm(memberships)


## ----hotspots, fig.cap="Hotspot locations in Victoria during 2019-2020 season."----
ggplot() +
  geom_sf(data = vic_map) +
  geom_point(data = hotspots, aes(lon, lat), alpha = 0.3) +
  theme_map()

