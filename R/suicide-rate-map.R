library(tidyverse)
library(mapcan)
library(showtext)

# Update font path (uncomment if needed)
# sysfonts::font_paths(new = "F:/Documents/Fonts")

# get suicide rates data (from gif github)
suicide_rates <- read_csv("suicide_rates.csv")

# get map shape data
map_data <- mapcan(boundaries = province, type = standard)

# Join shape data to suicide rate data
suicide_map <- 
  inner_join(
      map_data,
      suicide_rates,
      by = c("pr_english" = "Geography")
      )

# Approx center points for shapes and the rates
annotations <- 
  suicide_map %>% 
  group_by(pr_english) %>% 
  summarise(x = mean(long), y = mean(lat), Value = mean(Value))

# Fonts used in map plot
font_add("Oswald", regular = "Oswald-Regular.ttf", "Oswald-SemiBold.ttf")
showtext_auto()

 
ggplot(suicide_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Value), color = "white") +
  # geom_text(aes(label = unique(Value))) +
  annotate("text", x = -1240000, y = 850000,  label = "13,3", size = 4, family = "Oswald") +   # Alberta
  annotate("text", x = -1850000, y = 912727,  label =  "7,0", size = 4, family = "Oswald") +   # BC
  annotate("text", x = -160000,  y = 644233,  label = "12,9", size = 4, family = "Oswald") +   # Manitouba
  annotate("text", x = 2163606,  y = 227947,  label = "11,7", size = 4, family = "Oswald") +   # NB
  annotate("text", x = 2700000,  y = 810000,  label = "11,1", size = 4, family = "Oswald") +   # NFLD
  annotate("text", x = -1200000, y = 1900000, label =  "8,8", size = 4, family = "Oswald") +   # NWT
  annotate("text", x = 2700000,  y = 122727,  label = "11,0", size = 4, family = "Oswald") +   # NS
  annotate("text", x = -100000,  y = 1700000, label = "76,6", size = 4.5, family = "Oswald", color = "white", fontface = "bold") + # Nunavut
  annotate("text", x = 502413,   y = 300000,  label =  "8,9", size = 4, family = "Oswald") +   # ON
  annotate("text", x = 2356672,  y = 451613,  label =  "3,7", size = 4, family = "Oswald") +   # PEI
  annotate("text", x = 1549519,  y = 602762,  label = "10,6", size = 4, family = "Oswald") +   # QC
  annotate("text", x = -705362,  y = 644233,  label = "15,6", size = 4, family = "Oswald") +   # SK
  coord_fixed() +
  # Black and white scale
  scale_fill_gradient2( low = "white",
                        mid = "#999999",
                        high = "black",
                        guide = "none",
                        na.value = "#eeeeee") +
  # Green colour scale
  # scale_fill_gradient(
  #   low = "#BDE4B4",
  #   # mid = "#4C9F38",
  #   high = "#244B1B",
  #   guide = "none"
  # ) +
  theme_void()

ggsave("suicide-rate-map-bw.svg")
