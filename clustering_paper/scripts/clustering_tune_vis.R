library(tidyverse)

grid <- read_csv("data/clustering_grid.csv")
grid <- rename(grid, AdjDist = adj_dist, ActiveTime = active_time)

p1 <- ggplot(filter(grid)) +
  geom_point(aes(AdjDist, count)) +
  geom_line(aes(AdjDist, count, group = ActiveTime, col = factor(ActiveTime))) +
  scale_x_continuous(breaks = seq(1000,10000,1000)) +
  labs(color = "ActiveTime") +
  ylab("Number of clusters") +
  theme_bw(base_size = 20)


ggsave("figures/clustering_tuning_1.jpeg", p1, width = 10, dpi = 600)

p2 <- ggplot(filter(grid, AdjDist>2000)) +
  geom_point(aes(ActiveTime, count)) +
  geom_line(aes(ActiveTime, count, group = AdjDist, col = factor(AdjDist))) +
  scale_x_continuous(breaks = seq(3,48,3)) +
  labs(color = "AdjDist") +
  ylab("Number of clusters") +
  theme_bw(base_size = 20)


ggsave("figures/clustering_tuning_2.jpeg", p2, width = 10, dpi = 600)

fileConn <- file("scripts/setting.txt")
writeLines(c("active_time = 24","adj_distance = 3000"), fileConn)
close(fileConn)

