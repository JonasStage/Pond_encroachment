source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Figures/Figurer Gribskov.R")
library(patchwork)
bind_rows(co2_data,
          ch4_data) %>% 
  filter(station %in% c(1,2,3,4)) %>% 
  mutate(date = as.Date(closest_hour)) -> flux_data

flux_data %>% 
  group_by(station,date) %>%
  reframe(across(c(diff_umol:ebul_umol,CO2_umol), list(mean = mean), na.rm=T)) %>%
  mutate(month = month(date),
         across(diff_umol_mean:CO2_umol_mean, ~.x*24/1000)) %>% 
  rename(diff_mmol_m2_d1 = diff_umol_mean,
         ebul_mmol_m2_d1 = ebul_umol_mean,
         CO2_mmol_m2_d1 = CO2_umol_mean) -> flux_plot_data

read_csv("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Environmental variables/hobo.csv") %>% 
  mutate(date = as.Date(datetime)) %>% 
  filter(dyb_ob == 0.5) %>% 
  reframe(wtr_temp = mean(temp, na.rm=T),
          .by = date) -> daily_temp


#### August 2024 #### 

list.files("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/26_08_2024/Ilt",
           recursive = T,
           full.names = T) %>% 
  tibble(path = .) %>% 
  filter(str_detect(path, "Cat")) %>% 
  mutate(data = lapply(path, read_csv, skip = 8),
         station = c(1,2,3,4)) %>% 
  unnest(data) %>% 
  select(station,
         datetime = 4,
         wtr_temp = 6) %>% 
  filter(datetime < ymd_hms("2024-08-26 11:40:00")) %>% 
  mutate(date = as.Date(datetime)) %>% 
  reframe(wtr_temp = mean(wtr_temp, na.rm=T),
          .by = c(date))  -> august_wtr_temp

bind_rows(august_wtr_temp,
          daily_temp) -> daily_temp
