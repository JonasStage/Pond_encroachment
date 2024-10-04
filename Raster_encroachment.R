library(tidyverse);library(sf);library(raster);library(terra)

source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/R help script/ggplot_themes.R") ### getting ggplot theme

#load("Raster_encroachment.rda")
setwd("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Pond_encroachment")

#### Retrive shapes ####
read_sf("Data/Shapes/gribskov1_2007.shp") -> grib2007
read_sf("Data/Shapes/gribskov1_2008.shp") -> grib2008
read_sf("Data/Shapes/gribskov1_reeds.shp") -> reeds
read_sf("Data/Shapes/gribskov1_sedimentcores.shp") -> cores
read_sf("Data/Shapes/gribskov1_waterreeds.shp") -> waterreeds

ggplot() + 
  geom_sf(data= waterreeds, fill = "blue") +   ### Water covered area
  geom_sf(data= reeds, fill = "orange4") +     ### Area with reeds
  geom_sf(data= cores, col = "red")

#### Creating raster ####
## https://rpubs.com/ABajcz/InsideThePolygon ##

current.raster = terra::ext(reeds)
plot(current.raster)

current.raster = terra::rast(current.raster,
                             res = .1,
                             crs = crs(reeds), 
                             vals = 1)

plot(current.raster) #The entire box is now a value of 1, as the legend suggests.

current.raster = terra::mask(current.raster, 
                             terra::vect(reeds),
                             updatevalue = NA)

plot(current.raster)

waterreeds %>% 
  st_cast("LINESTRING") %>% 
  st_line_sample(density = 1) -> water_points        ### Create points for calculating distance. 

reeds.locs = terra::cellFromXY(current.raster, 
                               st_coordinates(water_points)[,1:2])

terra::values(current.raster)[reeds.locs] = 2

which(terra::values(current.raster) == 2)

all.dists = terra::gridDist(current.raster, 
                            target = 2)

plot(all.dists)
plot(reeds, add =T)

st_difference(waterreeds,reeds) %>% 
  st_buffer(.1) -> buff

terra::mask(all.dists, 
        vect(buff),
            updatevalue = NA) -> distances

distances %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  tibble %>% 
  rename(dist = lyr.1) %>% 
  filter(dist > 1) -> dist_df ## Had to remove a part of the polygon that was setting a boundary in the northeastern corner
  
dist_df %>% 
  ggplot() + 
  geom_sf(data = waterreeds) +
  geom_point(aes(x,y, col = dist)) + 
  scale_color_viridis_c(limits = c(0,20)) 

dist_df %>% 
  reframe(across(dist, list(mean = mean, 
                            median = median, 
                            min = min, 
                            max = max))) -> avg_dist_values

avg_dist_values %>% 
  mutate(across(dist_mean:dist_max, ~.x/(2023-2007)))

dist_df %>% 
  ggplot(aes(dist)) +
  geom_density() + 
  scale_x_continuous(limits = c(0,20)) + 
  tema + 
  labs(x = "Reed encroachment distance (m)",
       y = "Observation density")

#### Slope test ####

read_sf("Data/Shapes/Bathymetri/depth_b_bathymetric.shp") %>% 
  dplyr::select(depth_m) %>% 
  st_transform(st_crs(reeds))-> depth

st_intersection(depth,reeds) %>% 
  {.->> depth_sf} %>% 
  tibble %>% 
  separate_wider_delim(geometry, " ", names = c("x","y")) %>% 
  mutate(x = str_remove_all(x,"[c(,]"),
         y = str_remove_all(y,"[,)]"),
         x = as.numeric(x),
         y = as.numeric(y)) %>% 
  dplyr::select(x,y, depth_m) -> depth_df

st_as_sf(dist_df, coords = c("x","y"), crs = st_crs(reeds)) -> dist_sf

library(data.table)
dt2 <- data.table(dplyr::select(dist_df, x:y))
dt1 <- data.table(dplyr::select(depth_df,x:y))
dt1[, index := apply(raster::pointDistance(as.matrix(dt1), 
                                                 as.matrix(dt2), 
                                                 lonlat = FALSE), 1, 
                           which.min)][]


ggplot() + 
  geom_sf(data= waterreeds, fill = "blue") +   ### Water covered area
  geom_sf(data= reeds, fill = "orange4") +     ### Area with reeds
  geom_sf(data= depth_sf) +
  geom_segment(data = connecting, aes(x = x_point, xend = x_reed, y = y_point, yend = y_reed),
               arrow = arrow(length = unit(0.03, "npc"))) + 
  scale_fill_viridis_c()

depth_sf %>% 
  mutate(dist_from_shore = st_distance(depth_sf, st_cast(waterreeds,"LINESTRING"))[,1],
         dist_from_shore = as.numeric(dist_from_shore)) %>% 
  as.data.frame() %>% 
  separate_wider_delim(geometry, " ", names = c("x","y")) %>% 
  mutate(x_point = str_remove_all(x,"[c(,]"),
         y_point = str_remove_all(y,"[,)]"),
         x_point = as.numeric(x_point),
         y_point = as.numeric(y_point)) %>% 
  dplyr::select(depth_m,dist_from_shore,x_point,y_point) -> dist_reed_point

dist_df %>% 
  tibble %>% 
  mutate(index = row_number()) %>% 
  filter(index %in% dt1$index) %>% 
  full_join(dt1,
          by = join_by(index)) %>% 
  full_join(rename(depth_df,x.y =x, y.y = y)) %>% 
  rename(x_point = x.y,
         y_point = y.y,
         x_reed = x.x,
         y_reed = y.x,
         reed_speed = dist) %>% 
  full_join(dist_reed_point,
            by = join_by(x_point,y_point,depth_m)) %>% 
  mutate(slope = depth_m/dist_from_shore,
         encroachment_rate = reed_speed/(2023-2007)) -> connecting

connecting %>% 
  ggplot(aes(slope, encroachment_rate)) + 
  geom_point(size = 3) + 
  labs(x = "Slope (m/m)",
       y = bquote("Encroachment rate (m y"^-1*")")) + 
  tema + 
  geom_smooth(method = "lm", se = F) +
  scale_x_continuous(limits = c(0,0.5), breaks = seq(0,0.5,0.1)) +
  scale_y_continuous(limits = c(0.2,0.8), breaks = seq(0.2,0.8,0.2)) -> plotS3

jpeg("Figures/Figure S3.jpeg", width = 400, height = 400)
plotS3
dev.off()

connecting %>% 
  lm(encroachment_rate~slope, data= .) %>% summary
