rm(list = ls());invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))

library(tidyverse);library(lubridate);library(zoo);library(patchwork)
#### Calculation of yearly GHG budget in different ways ####

##### As a yearly mean #####

flux_plot_data %>% 
  group_by(month) %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ mean(.x, na.rm=T))) %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ mean(.x, na.rm=T))) %>% 
  mutate(method = "Average") -> avg_mean

flux_plot_data %>% 
  group_by(month) %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ median(.x, na.rm=T))) %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ median(.x, na.rm=T))) %>% 
  mutate(method = "Average") -> avg_median

##### Linear interpolation #####
flux_plot_data %>% 
  group_by(month) %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ mean(.x, na.rm=T))) %>% 
  add_row(month = c(3,8,12,13), 
          diff_mmol_m2_d1 = c(NA,NA,NA,1.13), 
          ebul_mmol_m2_d1 = c(NA,NA,NA,3.67), 
          CO2_mmol_m2_d1 = c(NA,NA,NA,37.5)) %>% 
  arrange(month) %>% 
  mutate(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ na.approx(.x))) %>% 
  filter(!month == 13) -> lm_interpolated
  
lm_interpolated %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ mean(.x, na.rm=T))) %>% 
  mutate(method = "LM",type = "Mean")-> lm_mean

lm_interpolated %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ median(.x, na.rm=T))) %>% 
  mutate(method = "LM",type = "Median") -> lm_median

bind_rows(lm_mean,lm_median) %>% 
  mutate(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~.x*365*5271.937/1000),     # 5271.937 size of pond
         diff_CO2e_t_y1 = diff_mmol_m2_d1*16.04*28*10^-6,
         ebul_CO2e_t_y1 = ebul_mmol_m2_d1*16.04*28*10^-6,
         CO2_t_y1 =  CO2_mmol_m2_d1*44.01*10^-6,
         unit = bquote("ton CO2-e/year")) %>% 
  select(diff_CO2e_t_y1:CO2_t_y1,method,type,unit) -> year_emis_co2_e

year_emis_co2_e %>% 
  mutate(diff_kg_C_y1 = diff_CO2e_t_y1/28*1000*(12/16),
         ebul_kg_C_y1 = ebul_CO2e_t_y1/28*1000*(12/16),
         CO2_kg_C_y1 = CO2_t_y1*1000*(12/44.01),
         unit = bquote("kg C /year")) %>% 
  select(diff_kg_C_y1:CO2_kg_C_y1,method:unit) -> year_emis_kg

##### By water temperature #####
library(mgcv)

daily_temp %>% 
  full_join(flux_plot_data) %>%
  filter(between(CO2_mmol_m2_d1, -600, 200)) -> gam_data 
  
gam_data %>% 
  gam(diff_mmol_m2_d1 ~ s(wtr_temp, bs = "cs"), data = .) -> diff_wtr_model
plot(diff_wtr_model)

gam_data %>% 
  gam(ebul_mmol_m2_d1 ~ s(wtr_temp, bs = "cs"), data = .) -> ebul_wtr_model
plot(ebul_wtr_model)

gam_data %>% 
  gam(CO2_mmol_m2_d1 ~ s(wtr_temp, bs = "cs"), data = .) -> CO2_wtr_model
plot(CO2_wtr_model)

daily_temp %>% 
  mutate(diff_mmol_m2_d1 = predict(diff_wtr_model, newdata = daily_temp),
         ebul_mmol_m2_d1 = predict(ebul_wtr_model, newdata = daily_temp),
         CO2_mmol_m2_d1 = predict(CO2_wtr_model, newdata = daily_temp)) %>% 
  mutate(month = month(date)) %>% 
  group_by(month) -> predicted_gam
  
predicted_gam %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ mean(.x, na.rm = T))) %>%
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ mean(.x, na.rm = T))) %>% 
  mutate(method = "GAM wtr") -> gam_mean

predicted_gam %>% 
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ median(.x, na.rm = T))) %>%
  reframe(across(diff_mmol_m2_d1:CO2_mmol_m2_d1, ~ median(.x, na.rm = T))) %>% 
  mutate(method = "GAM wtr") -> gam_median
  
##### Compare methods #####

bind_rows(avg_mean,
          lm_mean,
          gam_mean) %>% 
  pivot_longer(diff_mmol_m2_d1:CO2_mmol_m2_d1) %>% 
  mutate(method_f = factor(method, levels = c("Average","LM","GAM wtr"))) %>% 
  ggplot(aes(name, value*365/1000, fill = method_f)) +
    geom_col(position = position_dodge2()) + 
    scale_x_discrete(limits = c("ebul_mmol_m2_d1","diff_mmol_m2_d1","CO2_mmol_m2_d1"),
                     labels = c("Ebullitiv CH4","Diffusiv CH4","CO2")) + 
    labs(x = "",
         y = bquote("Yearly emissions (mol m"^-2*" y"^-1*")"),
         fill = "",
         title = "Mean values") + 
    scale_fill_discrete(limits = c("Average","LM","GAM wtr"),
                        labels = c("Average","Linear model","GAM water temp")) -> mean

bind_rows(avg_median,
          lm_median,
          gam_median) %>% 
  pivot_longer(diff_mmol_m2_d1:CO2_mmol_m2_d1) %>% 
  mutate(method_f = factor(method, levels = c("Average","LM","GAM wtr"))) %>% 
  ggplot(aes(name, value*365/1000, fill = method_f)) +
  geom_col(position = position_dodge2()) + 
  scale_x_discrete(limits = c("ebul_mmol_m2_d1","diff_mmol_m2_d1","CO2_mmol_m2_d1"),
                   labels = c("Ebullitiv CH4","Diffusiv CH4","CO2")) + 
  labs(x = "",
       y = bquote("Yearly emissions (mol m"^-2*" y"^-1*")"),
       fill = "",
       title = "Median values") + 
  scale_fill_discrete(limits = c("Average","LM","GAM wtr"),
                      labels = c("Average","Linear model","GAM water temp")) -> median

mean + median + plot_layout(guides = "collect", axes = "collect") & theme(legend.position = 'bottom')
