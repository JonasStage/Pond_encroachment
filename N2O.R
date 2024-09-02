library(tidyverse);library(lubridate);library(FluxSeparator)
source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/R help script/ggplot_themes.R") ### getting ggplot theme

read_delim("N2O Gribskov.data", skip =6, delim = "\t") %>% 
  mutate(datetime = ymd_hms(paste(date,time))) %>% #-713
  select(datetime,
         remark = 6,
         h2o = 9,
         n2o = 10,
         tempC = 12) %>% 
  drop_na() %>% 
  filter(n2o > 300) -> licor

licor %>% 
  filter(between(datetime, ymd_hms("2024-08-05 11:39:00"),ymd_hms("2024-08-05 11:47:00")) |
         between(datetime, ymd_hms("2024-08-05 11:47:00"),ymd_hms("2024-08-05 12:07:00")) |
         between(datetime, ymd_hms("2024-08-05 12:10:00"),ymd_hms("2024-08-05 12:24:00"))) %>% 
  mutate(PumpCycle = case_when(between(datetime, ymd_hms("2024-08-05 11:39:00"),ymd_hms("2024-08-05 11:47:00")) ~ 1,
                             between(datetime, ymd_hms("2024-08-05 11:47:00"),ymd_hms("2024-08-05 11:58:00")) ~ 2,
                             between(datetime, ymd_hms("2024-08-05 11:58:00"),ymd_hms("2024-08-05 12:15:00")) ~ 3,
                             T ~ 4)) -> licor_trim
  
licor_trim %>% 
  ggplot(aes(datetime, n2o)) + 
  geom_point() + 
  labs(y = bquote("N"[2]*"O concentration (ppb)")) + 
  scale_y_continuous(limits = c(335,350)) +
  tema


licor_trim %>% 
  mutate(station = 1) %>% 
  diffusive_flux(concentration_values = "n2o",
                 cutoff_start_value = 1000,
                 remove_observations_prior = 0,
                 number_of_observations_used = 1000000000000000,
                 look_for_bubbles = F)
  

licor_trim %>% filter(PumpCycle == 4) %>% 
  mutate(time = datetime-min(datetime),
         time = as.double(time)) %>% 
  lm(data = ., n2o ~ time)
