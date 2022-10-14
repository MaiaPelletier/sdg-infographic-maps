library(tidyverse)
library(mapcan)

water_use <- read_csv("water-use.csv")

# get map shape data
map_data <- mapcan(boundaries = province, type = standard)

water_use_map <- 
  left_join(
    map_data,
    water_use,
    by = c("pr_english" = "Geography")
  )

map_plot <- 
  ggplot(water_use_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = -Value), color = "white") + 
  coord_fixed() +
  scale_fill_gradient(  
    high = "#B7E9F5",
    low = "#1686A2",
    na.value = "#eeeeee",
    guide = "none"
  ) +
  theme_void() 
map_plot

ggsave("water-use-map.svg")
