library(tidyverse);library(lubridate)

read_csv("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/Gribskov/Environmental variables/ilt.csv") -> oxy
source("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/R help script/ggplot_themes.R") ### getting ggplot theme

summary(oxy)

oxy %>% 
  filter(between(datetime, ymd_hms("2023-02-14 06:00:00"),ymd_hms("2024-02-07 08:00:00"))) %>% 
  ggplot(aes(datetime)) +
  geom_line(aes(y = do, col = "do")) + 
  geom_line(aes(y = temp, col = "temp")) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b %Y") +
  scale_color_manual(labels = c("Dissolved oxygen",
                                "Water temperature"),
                     values = c("darkorange","royalblue")) + 
  labs(x = "",
       y = bquote("Dissolved oxygen (mg l"^-1*") and Water temperature (°C)"),
       col = "") + 
  tema -> oxy_temp_plot

tiff("Figures/Figure S2.tiff", height = 500, width = 1000)
oxy_temp_plot 
dev.off()

