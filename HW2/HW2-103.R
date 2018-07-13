library(readr)
library(ggplot2)
library(dplyr)
mobile = read_csv("~/Downloads/mobile_data.csv")

mobile %>% filter(company == "Apple" | (company == "Samsung") | (company == "LG") | (company == "Sony") | (company == "Google")) -> mobile

mobile %>% 
  mutate(NumberOfYes = (LTE == "Yes") + (radio == "Yes") + (radio == "Yes") + (gps == "Yes") +( wlan == "Yes") + (audio_jack == "Yes") + (card_slot == "Yes")) -> m


m %>% 
  mutate(score = battery_mah/ (NumberOfYes  + cam_px + ram)) ->m




 m %>% group_by(company) %>% 
   summarise(avg_score = mean(score, na.rm = TRUE)) ->m
 

 ggplot(m , aes(x = company, y = avg_score)) + geom_col(fill = "purple")
 