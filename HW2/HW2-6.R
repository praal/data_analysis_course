library(readr)
library(ggplot2)
library(dplyr)
mobile = read_csv("~/Downloads/mobile_data.csv")
                  
                  

mobile = arrange(mobile, year)                                  

mobile %>% 
  summarise(l = mean(dim_length, na.rm = TRUE), b = mean(dim_breadth, na.rm = TRUE), thick = mean(dim_thickness, na.rm = TRUE), w = mean(weight, na.rm = TRUE)) ->t

mobile %>% 
  mutate(NumberOfNo = (LTE == "No") + (radio == "No") + (radio == "No") + (gps == "No") +( wlan == "No") + (audio_jack == "No") + (card_slot == "No")) -> m

m %>% 
  filter(dim_length < 200 & dim_breadth < 100 & weight < 500) -> m

m %>% 
  mutate(camStat = (is.na(cam_px) || (!is.na(cam_px) && cam_px < 5)), vidStat = (is.na(video) || (!is.na(video) && video == "No"))) -> m

m %>% 
  mutate(pxcolStat = ifelse(is.na(px_col), 0, px_col), pxrowStat = ifelse(is.na(px_row), 0, px_row) ) -> m


m %>% 
  mutate(score = NumberOfNo - sim_no + (weight - t$w) + (dim_length - t$l) + (dim_breadth - t$b) + 2 * (dim_thickness - t$thick) + 5 * (2017 - year) + 4 * (camStat + vidStat) - pxcolStat - pxrowStat) -> m


m = arrange(m , -score)

ans = head(m , 20)

library(tidyr)
t = unite(ans, name, company, device)
p = ggplot(data = t, aes(x = reorder(name,score) , y = score))
p = p + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
p + geom_col(color = "purple", fill = "purple", alpha = 0.7) + xlab("Mobile") + ylab("Gushkubiat")