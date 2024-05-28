library(tidyverse);library(sf);library(raster);library(rgdal);library(terra)

#### Retrive shapes ####
read_sf("Data/Shapes/gribskov1_2007.shp") -> grib2007
read_sf("Data/Shapes/gribskov1_2008.shp") -> grib2008
read_sf("Data/Shapes/gribskov1_reeds.shp") -> reeds
read_sf("Data/Shapes/gribskov1_sedimentcores.shp") -> cores
read_sf("Data/Shapes/gribskov1_waterreeds.shp") -> waterreeds

ggplot() + 
  geom_sf(data= waterreeds, fill = "blue") +   ### Water covered area
  geom_sf(data= reeds, fill = "orange4")       ### Area with reeds

#### Creating raster ####
## https://rpubs.com/ABajcz/InsideThePolygon ##

current.raster = terra::ext(reeds)
plot(current.raster)

current.raster = terra::rast(current.raster,
                             res = 1,
                             crs = crs(reeds), 
                             vals = 1)

plot(current.raster) #The entire box is now a value of 1, as the legend suggests.

current.raster = terra::mask(current.raster, 
                             terra::vect(reeds),
                             updatevalue = NA)

plot(current.raster)

waterreeds %>% 
  dplyr::select(geometry) %>% 
  st_union() %>% 
  st_cast("MULTIPOINT") -> water_points        ### Create points for calculating distance. 

reeds %>% 
  dplyr::select(geometry) %>% 
  st_union() %>% 
  st_cast("MULTIPOINT") -> reeds_points        ### Create points for calculating distance. 

#point.locs = terra::cellFromXY(current.raster,
#                               st_coordinates(water_points)[,1:2])
reeds.locs = terra::cellFromXY(current.raster, 
                               st_coordinates(water_points)[,1:2])

terra::values(current.raster)[reeds.locs] = 2

which(terra::values(current.raster) == 2)

all.dists = terra::gridDist(current.raster, 
                            target = 2)

plot(all.dists)


st_difference(waterreeds,reeds) %>% 
  st_buffer(.01) -> buff

terra::mask(all.dists, 
        vect(buff),
            updatevalue = NA) %>% plot

plot(all.dists)
plot(buff, add =T)
