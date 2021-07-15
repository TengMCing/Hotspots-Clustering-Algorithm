## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  echo = FALSE, 
  cache=FALSE, 
  message=FALSE, 
  warning=FALSE, 
  fig.retina = 3,
  fig.align = "center",
  out.width="80%")


## ----libraries----------------------------------------------------------------
library(tidyverse)
library(ggthemes)
library(sf)
library(ggpubr)
library(rnaturalearth)
# remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires) # do we need, compile complains without
library(sp) # do we need, compile complains without
library(spotoroo)
library(lubridate)
library(gridExtra)
library(kableExtra)
library(ggforce)


## ----step2figs, fig.cap="An example of step 2 given 20 hotspots in interval $\\boldsymbol{S}_t$. (a) A hotspot is selected randomly as the first item of list $\\boldsymbol{L}$ and the point $\\boldsymbol{P}$. Hotspots in list $\\boldsymbol{L}$ are in red. Point $\\boldsymbol{P}$ is drawn with larger marker size. (b) Nearby hotspots of the point $\\boldsymbol{P}$ are appended to the list $\\boldsymbol{L}$. (c) Let point $\\boldsymbol{P}$ be the next item of list $\\boldsymbol{L}$ and append the nearby hotspots to list $\\boldsymbol{L}$. (d) The cluster is identified via repeating substep (c). (e) Clear the list $\\boldsymbol{L}$, then randomly select an unassigned hotspot to identify another cluster. (f) The final clustering result is produced via repeating substep (d). The labels show the cluster each hotspot belongs to."----

set.seed(1256)
x <- rnorm(10, mean = 0, sd = 3)
y <- rnorm(10, mean = 0, sd = 2)
x <- c(x,rnorm(10, mean = 5, sd = 2))
y <- c(y,rnorm(10, mean = 5, sd = 3))
points <- data.frame(x=x, y=y)

x <- c(x,rnorm(10, mean = 6, sd = 2))
y <- c(y,rnorm(10, mean = 7, sd = 2))
points_2 <- data.frame(x=x, y=y)

ggplot(points) +
  geom_point(aes(x,y), shape = 21, size = 1) +
  geom_point(data = NULL, aes(points$x[3], 
                              points$y[3],
                              col = "L"),
                              size = 3) +
  geom_circle(data = NULL, aes(x0 = points$x[3], y0 = points$y[3], r = 3)) +
  coord_fixed() +
  theme_bw() +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  theme(legend.position = "none") +
  scale_color_manual(values = "red") +
  labs(col = "") +
  ggtitle("Step 2: (a)") -> p1

ggplot() +
  geom_point(data = points, aes(x, y), size = 1, shape = 21) +
  geom_segment(data = NULL, aes(x = points$x[3], 
                                y = points$y[3],
                                xend = points$x[c(2, 4, 7, 11, 18)],
                                yend = points$y[c(2, 4, 7, 11, 18)])) +
  geom_point(data = NULL, aes(points$x[c(2, 4, 7, 11, 18)], 
                              points$y[c(2, 4, 7, 11, 18)],
                              col = "L")) +
  geom_point(data = NULL, aes(points$x[3], 
                              points$y[3],
                              col = "L"),
                              size = 3) +
  geom_circle(data = NULL, aes(x0 = points$x[3], y0 = points$y[3], r = 3)) +

  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  scale_color_manual(values = "red") +
  labs(col = "", size = "") +
  ggtitle("Step 2: (b)") -> p2


ggplot() +
  geom_point(data = points, aes(x, y), size = 1, shape = 21) +
  geom_segment(data = NULL, aes(x = points$x[7], 
                                y = points$y[7],
                                xend = points$x[c(5, 15)],
                                yend = points$y[c(5, 15)])) +
  geom_point(data = NULL, aes(points$x[c(2, 3, 4, 11, 18, 5, 15)], 
                              points$y[c(2, 3, 4, 11, 18, 5, 15)],
                              col = "L")) +
  geom_point(data = NULL, aes(points$x[7], 
                              points$y[7],
                              col = "L"),
                              size = 3) +
  geom_circle(data = NULL, aes(x0 = points$x[7], y0 = points$y[7], r = 3)) +

  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  scale_color_manual(values = "red") +
  labs(col = "", size = "") +
  ggtitle("Step 2: (c)") -> p3

ggplot() +
  geom_point(data = points, aes(x, y), size = 1, shape = 21) +
  geom_point(data = NULL, aes(points$x[c(2, 3, 4, 7, 11, 18, 5, 12, 13, 14, 15, 16, 17, 18)], 
                              points$y[c(2, 3, 4, 7, 11, 18, 5, 12, 13, 14, 15, 16, 17, 18)],
                              col = "L")) +
  
  geom_point(data = NULL, aes(points$x[16], 
                              points$y[16],
                              col = "L"),
                              size = 3) +
  geom_circle(data = NULL, aes(x0 = points$x[16], y0 = points$y[16], r = 3)) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  scale_color_manual(values = "red") +
  labs(col = "", size = "") +
  ggtitle("Step 2: (d)") -> p4

ggplot() +
  geom_point(data = points, aes(x, y), size = 1, shape = 21) +
  geom_point(data = NULL, aes(points$x[-c(2, 3, 4, 7, 11, 18, 5, 12, 13, 14, 15, 16, 17, 18, 19, 20)], 
                              points$y[-c(2, 3, 4, 7, 11, 18, 5, 12, 13, 14, 15, 16, 17, 18, 19, 20)],
                              col = "L")) +
  
  geom_point(data = NULL, aes(points$x[6], 
                              points$y[6],
                              col = "L"),
                              size = 3) +
  geom_circle(data = NULL, aes(x0 = points$x[6], y0 = points$y[6], r = 3)) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  scale_color_manual(values = "red") +
  labs(col = "", size = "") +
  ggtitle("Step 2: (e)") -> p5


point_cols <- rep(1, 20)
point_cols[c(2, 3, 4, 7, 11, 18, 5, 12, 13, 14, 15, 16, 17, 18)] <- 2
point_cols[19] <- 3
point_cols[20] <- 4

ggplot() +
  geom_text(data = points, aes(x, y, label = point_cols)) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  ggtitle("Step 2: (f)") -> p6


library(patchwork)

(p1 + p2 + p3)/(p4 + p5 + p6)


## -----------------------------------------------------------------------------
point_cols2 <- rep("a", 30)
point_cols2[19] <- "b"
point_cols2[c(1, 8, 6, 9, 10)] <- "c"

ggplot() +
  geom_point(data = points_2, aes(x, y, col = paste0("Cluster ", as.character(point_cols2)), shape = c(1:30)<=20)) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "right") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  labs(col = "", shape = expression("Hotspots "*" in "*S[t-1])) +
  scale_shape_manual(values = c(1, 2)) +
  ggtitle(quote(S[t])) -> p1


ggplot() +
  geom_text(data = points, aes(x, y, label = point_cols)) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "left") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  labs(col = "") +
  ggtitle(quote(S[t-1])) -> p2





## -----------------------------------------------------------------------------
X1 <- as.data.frame(points_2[point_cols2 == "a",])
hull_1 <- chull(X1)
hull_1 <- c(hull_1, hull_1[1])

X2 <- as.data.frame(points_2[point_cols2 == "c",])
hull_2 <- chull(X2)
hull_2 <- c(hull_2, hull_2[1])

ggplot() +
  geom_text(data = points_2[1:20, ], aes(x, y, label = point_cols), col = "blue") +
  geom_point(data = points_2[21:30, ], aes(x, y), col = "red") +
  geom_polygon(data = X1[hull_1,], aes(x=x, y=y), fill = "black", alpha = 0.1) +
  geom_polygon(data = X2[hull_2,], aes(x=x, y=y), fill = "black", alpha = 0.1) +
  coord_fixed() +
  theme_bw() +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  labs(col = "", 
       shape = expression("Hotspots "*" in "*S[t-1]), 
       title = "Step 3: (a)", 
       subtitle = quote(S[t])) -> p3


## -----------------------------------------------------------------------------
ggplot(data = mutate(points_2, group = rep("", 30))) +
  geom_point(aes(x, y)) +
  geom_polygon(data = X1[hull_1,], aes(x=x, y=y), fill = "black", alpha = 0.1) +
  geom_polygon(data = X2[hull_2,], aes(x=x, y=y), fill = "black", alpha = 0.1) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "right") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  ggtitle(bquote(S[t]~"before updating")) -> p5


## ----step3figs, fig.cap = "An example of step 3. In this example, there are 30 hotspots belong to interval $\\boldsymbol{S}_t$. (a) 20 out of 30 hotspots belong to both interval $\\boldsymbol{S}_t$ and interval $\\boldsymbol{S}_{t-1}$. Let these hotspots carry over from their memberships in $\\boldsymbol{S}_{t-1}$. They are annotated in blue with membership labels. Points in red are the rest 10 hotspots that only belong to interval $\\boldsymbol{S}_t$. (b) For each red point, let it carry over from the membership of the nearest blue label which shares the same component in interval $\\boldsymbol{S}_t$ (according to the spatial clustering result of this interval). "----

final_labels <- c(point_cols, 4, 2, 4, 4, 2, 2, 2, 4, 4, 2)

X1 <- as.data.frame(points_2[final_labels == "1",])
hull_1 <- chull(X1)
hull_1 <- c(hull_1, hull_1[1])

X2 <- as.data.frame(points_2[final_labels == "2",])
hull_2 <- chull(X2)
hull_2 <- c(hull_2, hull_2[1])

X3 <- as.data.frame(points_2[final_labels == "3",])
hull_3 <- chull(X3)
hull_3 <- c(hull_3, hull_3[1])

X4 <- as.data.frame(points_2[final_labels == "4",])
hull_4 <- chull(X4)
hull_4 <- c(hull_4, hull_4[1])

ggplot() +
  geom_text(data = points_2, aes(x, y, label = final_labels, col = c(1:30)<=20)) +
  geom_polygon(data = X1[hull_1,], aes(x=x, y=y), fill = "black", alpha = 0.1) +
  geom_polygon(data = X2[hull_2,], aes(x=x, y=y), fill = "black", alpha = 0.1) +
  geom_polygon(data = X3[hull_3,], aes(x=x, y=y), fill = "black", alpha = 0.1) +
  geom_polygon(data = X4[hull_4,], aes(x=x, y=y), fill = "black", alpha = 0.1) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  labs(col = expression("Hotspots "*" in "*S[t-1]), title = "Step 3: (b)", subtitle = quote(S[t])) +
  scale_color_manual(values = c("red", "blue")) -> p4

ggplot() +
  geom_text(data = points, aes(x, y, label = point_cols)) +
  geom_point(data = mutate(points_2, group = rep("", 30)),
             aes(x,y), col = NA) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
  ggtitle(bquote(S[t-1])) -> p7
  

(p7 + p5 + p3 + p4)


## ----echo = TRUE, message = FALSE---------------------------------------------
result <- hotspot_cluster(hotspots = hotspots,
                          lon = "lon",
                          lat = "lat",
                          obsTime = "obsTime",
                          activeTime = 24,
                          adjDist = 3000,
                          minPts = 4,
                          minTime = 3,
                          timeUnit = "h",
                          timeStep = 1)


## ----echo = TRUE, message=TRUE------------------------------------------------
result


## ----echo = TRUE, message=TRUE------------------------------------------------
head(result$hotspots, 2)


## ----echo = TRUE, message=TRUE------------------------------------------------
head(result$ignition, 2)


## ----echo=TRUE, message=TRUE--------------------------------------------------
all_fires <- extract_fire(result, noise = TRUE)


## ----echo=TRUE, message=TRUE--------------------------------------------------
fire_1_and_2 <- extract_fire(result, cluster = c(1, 2), noise = FALSE)


## ----demodefplot, echo=TRUE, fig.cap="Default plot."--------------------------
plot(result)


## ----demomovplot, echo=TRUE, fig.cap="Fire movement plot."--------------------
plot(result, type = "mov", step = 12)


## ----demotimeline, echo=TRUE, fig.cap="Timeline of clusters."-----------------
plot(result, type = "timeline")


## -----------------------------------------------------------------------------
if (!file.exists("data/VIC_result.rda")) {
  him_hotspots <- read_csv("data/VIC_hotspots_before_clustering.csv")
  temptemp <- read_csv("data/VIC_hotspots_raw.csv")
  
  him_hotspots <- mutate(him_hotspots, obsTime = temptemp$`#obstime`)
  him_hotspots <- select(him_hotspots, -time_id)
  
  library(spotoroo)
  result <- hotspot_cluster(hotspots = him_hotspots,
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
  
  
  save(result, file = "data/VIC_result.rda")
  rm(temptemp)
  rm(result)
  rm(him_hotspots)

}

load("data/VIC_result.rda")
him_hotspots <- read_csv("data/VIC_hotspots_raw.csv")
him_hotspots$fire_id <- read_csv("data/VIC_hotspots_after_clustering.csv") %>% .[['fire_id']]


## ----hotspots, fig.cap="The distribution of hotspots in Victoria during 2019-2020 Australia bushfire season."----
if (!file.exists("figures/before_clustering.png")){
  ggplot() +
  geom_sf(data = vic_map, col = NA) +
  geom_point(data = him_hotspots, aes(lon, lat), alpha = 0.3) +
  theme_map() -> p

  ggsave(filename = "figures/before_clustering.png", dpi = 300, plot = p)
}


knitr::include_graphics("figures/before_clustering.png")


## ----echo=TRUE,message=TRUE---------------------------------------------------
result


## ----clusteringfinalresults, fig.cap=" The distribution of bushfire ignitions in Victoria during 2019-2020 Australian bushfire season."----
plot(result, bg = plot_vic_map(), hotspot = FALSE) 


## ----himtimeline, fig.cap="Timeline of fires observed in Victoria during the 2019-2020 Australian bushfire season."----
plot(result, type = "timeline", mainBreak = "1 month", dateLabel = "%b %d, %y")


## ----firemovem, echo = TRUE, fig.cap="Movements of 4 most intensive fires observed in Victoria during the 2019-2020 Australian bushfire season."----

plot(result, 
     type = "mov", 
     cluster = order(result$ignition$obsInCluster,
                     decreasing = TRUE)[1:4], 
     step = 12, 
     bg = plot_vic_map())


## ----vis1, fig.cap="A visualization tool for parameter tuning . It works like a scree plot. Major falls of the percentage of noise points are observed when $AdjDist < 3000$ so the reasonable choice of $AdjDist$ is 3000m."----
mygrid <- read_csv("data/grid.csv")
ggplot(mygrid) +
  geom_line(aes(adjDist, noise_prop, col = factor(activeTime))) +
  scale_x_continuous(breaks = seq(1000, 10000, 1000)) +
  labs(col = "activeTime") +
  ylab("Percentage of noise points")


## ----vis2, fig.cap="Major falls of the percentage of noise points are observed when $ActiveTime < 24$, so the reasonable choice of $ActiveTime$ is 24 hours."----
ggplot(filter(mygrid, adjDist > 2000)) +
  geom_line(aes(activeTime, noise_prop, col = factor(adjDist))) +
  scale_x_continuous(breaks = seq(6, 48, 6)) +
  labs(col = "adjDist") +
  ylab("Percentage of noise points")


## -----------------------------------------------------------------------------
pagebreak <- function() {
  if(knitr::is_latex_output())
    return("\\clearpage")
  else
    return('<div style="page-break-before: always;" />')
}

