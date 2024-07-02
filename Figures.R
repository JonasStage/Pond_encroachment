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
  ggsn::scalebar(waterreeds, location = "bottomright", dist = 20, dist_unit = "m",transform=F, st.size = 3)-> fig1.2

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
  scale_color_gradient(low = "forestgreen",high = "orange", limits = c(0,20), breaks = seq(0,20,5)) -> orto_dist

dist_df %>% 
  ggplot(aes(dist)) + 
  geom_density() + 
  tema + 
  labs(x = "Distance from 2007 water extent (m)",
       y = "Density") -> density_dist


tiff("Figures/Figure 1.tiff", height = 500, width = 700)
(fig1.2 + orto_dist) / density_dist + plot_layout(heights = c(6,2))
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
  scale_color_gradient(low = "cadetblue",high = "chocolate4", limits = c(0,420)) -> org_plot

tiff("Figures/Figure 2.tiff", height = 600, width = 600)
org_plot
dev.off()

#### Figure 4 ####
#### Målinger fra damstudie ####
rm(list = ls());invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Figures/Figurer Gribskov.R")
library(patchwork)
bind_rows(co2_data,
          ch4_data) %>% 
              filter(station %in% c(1,2,3,4)) %>% 
              mutate(date = as.Date(closest_hour)) -> flux_data
  
flux_data %>% 
  group_by(station,date) %>%
  mutate(CO2_umol = case_when(CO2_umol < -800 ~ NA_real_,
                              T ~ CO2_umol)) %>% 
  reframe(across(c(diff_umol:ebul_umol,CO2_umol), list(mean = mean), na.rm=T)) %>%
  mutate(month = month(date),
         across(diff_umol_mean:CO2_umol_mean, ~.x*24/1000)) %>% 
  rename(diff_mmol_m2_d1 = diff_umol_mean,
         ebul_mmol_m2_d1 = ebul_umol_mean,
         CO2_mmol_m2_d1 = CO2_umol_mean) -> flux_plot_data

flux_plot_data %>% 
  ggplot(aes(month, diff_mmol_m2_d1, group = month, fill = "diff")) +
  geom_boxplot() + 
  scale_x_continuous(limits = c(.5,12.5),
                     breaks = seq(1,12,1),
                     labels = rep("",12)) + 
  labs(x = "",
       y = bquote("Flux (mmol m"^-2*" d"^-1*")"),
       fill = "") + 
  scale_fill_manual(values = "darkorange", 
                    labels = "Diffusive methane flux") + 
  theme(legend.position = "bottom") + 
  scale_y_continuous(limits = c(-4, 25.2),
                     breaks = seq(0,25,5))-> diff_flux_plot

flux_plot_data %>% 
  ggplot(aes(month, ebul_mmol_m2_d1, group = month, fill = "ebul")) +
  geom_boxplot() + 
  scale_x_continuous(limits = c(.5,12.5),
                     breaks = seq(1,12,1),
                     labels = rep("",12)) + 
  labs(x = "",
       y = bquote("Flux (mmol m"^-2*" d"^-1*")"),
       fill = "") + 
  scale_y_continuous(limits = c(0,68), 
                     breaks = seq(0,60,20)) + 
  scale_fill_manual(values = "royalblue", 
                    labels = "Ebullitive methane flux") + 
  theme(legend.position = "bottom") -> ebul_flux_plot

flux_plot_data %>% 
  ggplot(aes(month, CO2_mmol_m2_d1, group = month, fill = "co2")) +
  geom_hline(aes(yintercept = 0), linetype = "dotted") +
  geom_boxplot() + 
  scale_x_continuous(limits = c(.5,12.5),
                     breaks = seq(1,12,1),
                     labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + 
  labs(x = "",
       y = bquote("Flux (mmol m"^-2*" d"^-1*")"),
       fill = "") + 
  scale_y_continuous(breaks = seq(-40,100,20)) +
  coord_cartesian(ylim = c(-20,110)) + 
  scale_fill_manual(values = "darkgreen", 
                    labels = expression(CO['2']*" flux")) + 
  theme(legend.position = "bottom") -> co2_flux_plot

ebul_flux_plot / diff_flux_plot / co2_flux_plot / guide_area() + plot_layout(guides = 'collect', 
                                                                             axis_titles = "collect") -> flux_fig
setwd("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Pond_encroachment")
tiff("Figures/Figure 3.tiff", height = 1000, width = 600)
flux_fig
dev.off()

flux_plot_data %>% 
  group_by(month) %>%
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, mean, na.rm=T)) %>%
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, mean, na.rm=T))

#### Water Temp ####

source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Pond_encroachment/water_temp_flux.R")

daily_temp %>% 
  full_join(flux_plot_data) %>%
  filter(between(CO2_mmol_m2_d1, -600, 200)) %>% 
  pivot_longer(diff_mmol_m2_d1:CO2_mmol_m2_d1) %>% 
  mutate(name_f = case_when(name == "ebul_mmol_m2_d1" ~ "Ebullitive methane",
                            name == "diff_mmol_m2_d1" ~ "Diffusive methane",
                            name == "CO2_mmol_m2_d1" ~ "CO2"),
         name_f = factor(name_f, levels = c("Ebullitive methane","Diffusive methane","CO2"))) %>% 
  ggplot(aes(wtr_temp, value, col = name_f)) + 
  geom_point() + 
  facet_wrap(~name_f, scales = "free_y", ncol = 1) + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) + 
  labs(x = "Daily water temperature (°C)",
       y = bquote("Flux (mmol m"^-2*" d"^-1*" )"),
       col = "") + 
  tema + 
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_color_manual(values = c("royalblue","darkorange","forestgreen"),
                     labels = c("Ebullitive methane","Diffusive methane",bquote("CO"[2]))) + 
  theme(strip.text = element_blank()) -> wtr_temp_flux_plot

setwd("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Pond_encroachment")
tiff("Figures/Figure 4.tiff", height = 600, width = 400)
wtr_temp_flux_plot 
dev.off()
