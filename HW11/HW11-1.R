library(readr)
library(plotly)

data = read_rds("~/Downloads/week_11/week_11/data/historical_web_data_26112015.rds")


p <- plot_ly(data, x = ~Latitude, y = ~Longitude, z = ~Depth, size = ~Magnitude, colors = c('#BF382A', '#0C4B8E')) %>%
  add_markers() %>%
  layout(scene = list(yaxis = list(title = 'Latitude'),
                      xaxis = list(title = 'Longitude'),
                      zaxis = list(title = 'Depth')))

p