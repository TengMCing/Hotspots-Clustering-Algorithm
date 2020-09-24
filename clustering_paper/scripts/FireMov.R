library(tidyverse)
library(rnaturalearth)
library(lubridate)
library(ggthemes)

au_map <- ne_states(country = 'Australia', returnclass = 'sf')
vic_map <- au_map[7,]

hotspots <- read_csv("data/VIC_hotspots_raw.csv")
memberships <- read_csv("data/VIC_hotspots_after_clustering.csv")
hotspots$fire_id <- memberships$fire_id


plotdata <- function(obs){
  temp <- filter(hotspots, fire_id == obs) %>%
    group_by(hour_id) %>%
    summarise(lon = mean(lon), lat = mean(lat)) %>%
    ungroup()
  
  # ggplot(filter(hotspots, fire_id == obs)) +
  #   geom_point(aes(lon ,lat, col = firepower, size = firepower), position = "jitter") +
  #   geom_path(data = temp, aes(lon, lat), col = "black", size = 1) +
  #   geom_point(data = temp2, aes(lon, lat, shape = shape), col = "red", size = 5, stroke = 2) +
  #   scale_color_viridis_c(option = "B", direction = -1) +
  #   scale_shape_manual(values = c("Ignition point" = 24, "Extinguish point" = 22)) +
  #   theme_minimal()
  
  temp3 <- NULL
  
  for (i in seq(2137, max(temp$hour_id), 6)){
    temp4 <- filter(hotspots, fire_id == obs, hour_id <= i, hour_id > i-6)
    if (nrow(temp4) == 0){
      temp4 <- filter(hotspots, fire_id == obs, hour_id <= i-6, hour_id > i-12) %>%
        mutate(firepower = NA)
    }
    temp4$label <- i
    if (is.null(temp3)){
      temp3 <- temp4
    } else {
      temp3 <- bind_rows(temp3, temp4)
    }
  }
  
  temp3 <- mutate(temp3, label = ymd_h("2019-10-01 00") + hours(label))
  temp3$label <- as.character(temp3$label)
  
  return(temp3)
  
}

obs <- 395
data1 <- plotdata(obs)

obs <- 390
data2 <- plotdata(obs)

temp2 <- filter(hotspots, fire_id %in% c(390, 395))

data1$firepower <- data1$firepower * -1

temp2$lon <- jitter(temp2$lon)
temp2$lat <- jitter(temp2$lat)

p1 <- bind_rows(data1, data2) %>%
  ggplot() +
  geom_point(data = temp2, aes(lon, lat), col = "grey") +
  geom_point(aes(lon, lat, col = firepower), position = "jitter") +
  scale_color_distiller(palette = "RdYlBu", na.value = "grey") +
  # scale_color_viridis_c(option = "B", direction = -1, na.value = "grey") +
  facet_wrap(~label) +
  theme_map() +
  theme(legend.position = "none")
  

p2 <- ggplot() +
  geom_sf(data = vic_map) +
  geom_point(data = filter(hotspots, fire_id %in% c(390, 395)),
             aes(lon, lat, col = factor(fire_id))) +
  labs(color = "fire_id") +
  scale_color_manual(values = c("390" = "#a50026", "395" = "#313695")) +
  theme_map()


p3 <- gridExtra::grid.arrange(p2, p1, ncol = 1)

ggsave(paste0("figures/", "fire_mov", ".jpeg"), plot = p3, width = 14, height = 14)