library(gstat)
source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Sedimentkerner/sediment_data.R")

org_mat_m2 %>% 
  reframe(across(depth:OC_m2_y, mean),
          .by = site, type) %>% 
  distinct() %>% 
  rename(OC_g_m2_y = OC_m2_y) -> core_data

#### Reeds ####
core_data  %>% 
  arrange(site) %>% 
  filter(type == "Reed") %>% 
  dplyr::select(OC_g_m2_y)-> data

cores %>%
  arrange(id) %>% 
  filter(type == "reed") %>% 
  st_coordinates() %>% 
  {. ->> pp} %>% 
  cbind(OC = data$OC_g_m2_y) %>% 
  as.data.frame() %>% 
  rename(x = X, y = Y)-> coords

r <- raster(reeds, res = .1) 

raster<- rasterize(pp,r)
  
  gs <- gstat(formula=OC~1, locations=~y+x, data=coords, maxdist = 100, nmax = 3)
  idw <- raster::interpolate(r, gs, debug.level=0)
  
  idw <- mask(idw, slice(reeds,1))
  idw %>% 
    as.data.frame(xy = T) %>% 
    drop_na() %>% 
    rename(x = 1, y = 2, z = 3) %>% 
    tibble %>% 
    mutate(type = "reeds") -> reeds_org_C
  
  #### water ####
  core_data  %>%                  # z data
    arrange(site) %>% 
    filter(type == "Water") %>% 
    arrange(site) %>% 
    dplyr::select(OC_g_m2_y, site)-> data
  
  cores %>%                       # coordinates
    filter(type == "water") %>% 
    arrange(id) %>% 
    mutate(id = id + 10) %>% 
    st_coordinates() %>% 
    {. ->> pp} %>% 
    cbind(OC = data) %>% 
    as.data.frame() %>% 
    rename(x = X, y = Y)-> coords
  
  diff_raster <- st_difference(waterreeds,reeds)        
  
  r <- raster(diff_raster, res = .1) 
  
  raster<- rasterize(pp,r)
  
  gs <- gstat(formula=OC.OC_g_m2_y~1, locations=~y+x, data=coords, maxdist = 100, nmax = 10)
  idw <- raster::interpolate(r, gs, debug.level=0)
  
  idw <- mask(idw, slice(st_difference(waterreeds,reeds),1))
  idw %>% 
    as.data.frame(xy = T) %>% 
    drop_na() %>% 
    rename(x = 1, y = 2, z = 3) %>% 
    tibble %>% 
    mutate(type = "water")-> water_org_C

#### plot data ####  
ggplot() + 
  geom_tile(data= reeds_org_C, aes(x,y, fill = z)) + 
  geom_tile(data= water_org_C, aes(x,y, fill = z)) + 
    scale_fill_viridis_c()
  
  
  
  