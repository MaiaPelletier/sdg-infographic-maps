library(tidyverse)
library(cansim)
library(mapcan)

renewable_data <- read_csv("renewable_data.csv")

renewable_data <- 
  renewable_data %>% 
  select(
    Year = REF_DATE,
    Geography = GEO,
    Type = `Type of electricity generation`,
    Value = VALUE
  ) %>% 
  mutate(Type = ifelse(str_detect(Type, "hydro"), "Renewable", "All")) %>% 
  pivot_wider(
    names_from = Type,
    values_from = Value
  ) %>% 
  transmute(
    Year, Geography,
    Value = Renewable/All
  ) %>% 
  arrange(desc(Value))

# pts <- c(
#   "British Columbia",
#   "Manitoba", 
#   "Prince Edward Island",
#   "Newfoundland and Labrador",
#   "Quebec"
#   )
# 
# renewable_data <- tibble(
#   Geography = pts,
#   target_met = TRUE
# )

# get map shape data
map_data <- mapcan(boundaries = province, type = standard)

renewable_map <- 
  left_join(
    map_data,
    renewable_data,
    by = c("pr_english" = "Geography")
  )

map_plot <- 
  ggplot(renewable_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = -Value), color = "white") + 
  coord_fixed() +
    scale_fill_gradient(  
      high = "#f6e8cb",
      low = "#F0A919",
      na.value = "#eeeeee",
      guide = "none"
    ) +
  theme_void() 
map_plot

ggsave("renewable-energy-map.svg")
