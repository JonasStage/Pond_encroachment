library(tidyverse);library(sf);library(raster);library(rgdal);library(terra)

source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/R help script/ggplot_themes.R") ### getting ggplot theme

load("Raster_encroachment.rda")

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

