library(tidyverse)
library(cansim)
library(mapcan)

# establish food insecurity data from 2020 for provinces & 2019 for territories
food_insecurity <- 
  tribble(
    ~Geography,                   ~Value,
    "Newfoundland and Labrador",  12.6,
    "Prince Edward Island",       10.7,
    "Nova Scotia",                13.1,
    "New Brunswick",              12.4,
    "Quebec",                     8.6,
    "Ontario",                    11.5,
    "Manitoba",                   12.0,
    "Saskatchewan",               12.9,
    "Alberta",                    15.1,
    "British Columbia",           10.0,
    "Yukon",                      15.3,
    "Northwest Territories",      23.1,
    "Nunavut",                    46.1
)

# get map shape data
map_data <- mapcan(boundaries = province, type = standard)

food_insecur_map <- 
  inner_join(
    map_data,
    food_insecurity,
    by = c("pr_english" = "Geography")
  )

map_plot <- 
  ggplot(food_insecur_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Value), color = "white") + 
  coord_fixed() +
  scale_fill_gradient2( low = "white",
                        mid = "#999999",
                        high = "black",
                        guide = "none") +
  theme_void()

map_plot

ggsave("food-insecurity-map-bw.png")
write_csv(food_insecurity, "food-insecurity.csv")
