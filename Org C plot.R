#### Reeds ####
org_cores  %>% 
  filter(type == "reed") %>% 
  dplyr::select(OC_g_m2_y)-> data

data %>% 
  st_coordinates() %>% 
  cbind(OC = data$OC_g_m2_y) %>% 
  as.data.frame() %>% 
  rename(x = X, y = Y)-> coords

r <- raster(reeds, res = .1) 

raster<- rasterize(data,r)
  
  gs <- gstat(formula=OC~1, locations=~y+x, data=coords, maxdist = 50, nmax = 2)
  idw <- raster::interpolate(r, gs, debug.level=0)
  
  idw <- mask(idw, slice(reeds,1))
  idw %>% 
    as.data.frame(xy = T) %>% 
    drop_na() %>% 
    rename(x = 1, y = 2, z = 3) %>% 
    tibble %>% 
    mutate(type = "reeds") -> reeds_org_C
  
  #### water ####
  org_cores  %>% 
    filter(type == "water") %>% 
    dplyr::select(OC_g_m2_y)-> data
  
  data %>% 
    st_coordinates() %>% 
    cbind(OC = data$OC_g_m2_y) %>% 
    as.data.frame() %>% 
    rename(x = X, y = Y)-> coords
  
  r <- raster(st_difference(waterreeds,reeds), res = .1) 
  
  raster<- rasterize(data,r)
  
  gs <- gstat(formula=OC~1, locations=~y+x, data=coords, maxdist = 50, nmax = 10)
  idw <- raster::interpolate(r, gs, debug.level=0)
  
  idw <- mask(idw, slice(st_difference(waterreeds,reeds),1))
  idw %>% 
    as.data.frame(xy = T) %>% 
    drop_na() %>% 
    rename(x = 1, y = 2, z = 3) %>% 
    tibble %>% 
    mutate(type = "water")-> water_org_C

  
  