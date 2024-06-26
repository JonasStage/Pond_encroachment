source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Sedimentkerner/sediment_data.R")
source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/R help script/ggplot_themes.R") ### getting ggplot theme
setwd("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Pond_encroachment")
source("Raster_encroachment.R")
library(tidyverse);library(sf);library(raster);library(rgdal);library(terra);library(terrainr);library(patchwork)
        
#Function
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

readxl::read_excel("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Sommermålinger 2022/sensor_overblik.xlsx") %>% 
  filter(location == "Gribskov 1") %>% 
  dplyr::select(st, lat,long) -> flux_stations

LongLatToUTM(flux_stations$long,flux_stations$lat, "32") -> coord_flux_stations

#### Figure 1 ####

stack("Data/Ortophoto/2023/2023.tif") -> orto2023

orto2023 %>% 
  as.data.frame(xy = T) %>% 
  filter(between(x, 704020, 704150)) %>% 
  drop_na() %>% 
  tibble %>% 
  mutate(rgb = rgb(X2023_1,X2023_2,X2023_3,X2023_4, maxColorValue = 255)) -> df_rgb

ggplot() + 
  geom_raster(data = df_rgb, aes(x=x, y=y, fill=rgb)) + 
  scale_fill_identity() + 
  coord_equal() + 
  kortbaggrund + 
  labs(x= "",
       y = "") ->fig1.1

fig1.1 + 
  geom_sf(data = waterreeds, fill = "blue") + 
  geom_sf(data = reeds, fill = "orange4") + 
  geom_sf(data = cores, col = "black", size = 4) +
  geom_point(data = coord_flux_stations, aes(X,Y),size = 4, col = "red") +
  ggsn::scalebar(waterreeds, location = "bottomright", dist = 25, dist_unit = "m",transform=F)-> fig1.2

tiff("Figures/Figure 1.tiff", height = 900, width = 1000)
fig1.1 + fig1.2
dev.off()

#### Figure 2 ####

all.dists %>% 
  as.data.frame(xy = T) %>% 
  drop_na() %>% 
  tibble %>% 
  rename(dist = lyr.1) %>% 
  filter(dist > 0) -> all_dist_df

fig1.1 + 
  geom_tile(data = all_dist_df, aes(x, y, col = dist)) +
  theme(legend.position = "right") + 
  labs(col = "Distance from 2007\nwater extent (m)\n") + 
  guides(col = guide_colourbar(theme = theme(legend.key.height  = unit(10, "lines")))) + 
  scale_color_gradient(low = "forestgreen",high = "orange", limits = c(0,20), breaks = seq(0,20,5)) -> fig2.1

dist_df %>% 
  ggplot(aes(dist)) + 
  geom_density() + 
  tema + 
  labs(x = "Distance from 2007 water extent (m)",
       y = "Density") -> fig2.2

tiff("Figures/Figure 2.tiff", height = 600, width = 600)
fig2.1 / fig2.2 + plot_layout(heights = c(4,2))
dev.off()

#### Figure 3 ####

org_mat_m2 %>% 
  reframe(OC_g_m2_y = mean(OC_m2_y, na.rm=T),
          OC_g_m2_y_sem = sem(OC_m2_y),
          .by = site) %>% 
  mutate(id = site) -> org_material_data

cores %>% 
  full_join(org_material_data) -> org_cores

source("Org C plot.R")
fig1.1 + 
  geom_tile(data = reeds_org_C, aes(x,y,col = z)) + 
  geom_tile(data = water_org_C, aes(x,y,col = z)) + 
  geom_sf(data = st_boundary(reeds)) + 
  kortbaggrund + 
  labs(x = "",y = "", col = bquote("Organic carbon deposition (g OC m"^-2*" y"^-1*")")) +
  theme(legend.position = c(.58,-.1), 
        legend.direction = "horizontal", 
        legend.key.width= unit(1.5, 'cm'),
        legend.title = element_text(size=14), #change legend title font size
        legend.text = element_text(size=14),
        plot.margin = margin(1,1,1,1, "cm"),
        legend.key = element_blank(),
        legend.background=element_blank()) +
  guides(color = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
  scale_color_gradient(low = "darkseagreen2",high = "chocolate4") -> org_C_plot
