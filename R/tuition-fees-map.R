library(tidyverse)
library(cansim)
library(mapcan)
library(showtext)

# Update font path
# sysfonts::font_paths(new = "F:/Documents/Fonts")

# read tuition data
tuition_fees <- readr::read_csv("tuition_fees.csv") 

# get map shape data
map_data <- mapcan(boundaries = province, type = standard)

tuition_map <- 
  left_join(
    map_data,
    tuition_fees,
    by = c("pr_english" = "Geography")
  )

# Fonts used in map plot
font_add("Oswald", regular = "Oswald-Regular.ttf", "Oswald-SemiBold.ttf")
showtext_auto()

map_plot <- 
  ggplot(tuition_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = Value), color = "white") + 
  coord_fixed() +
  scale_fill_gradient(  
    low = "#F2CCD0",
    high = "#A21526",
    na.value = "#eeeeee",
    guide = "colorbar",
    breaks = c(3500, 8400),
    labels = c("$", "$$$")
    ) +
  guides(fill = guide_colorbar(
    direction = "horizontal",
    ticks = FALSE,
    title = NULL,
    label.position = "top",
    barwidth = 8
    # barheight = 10
    )) +
  theme_void() +
  theme(
    legend.position = c(0.85, 0.75),
    legend.text = element_text(
      family = "sans", 
      face = "bold",
      size = 45,
      color = "#494f56"
      )
  )

map_plot

ggsave("tuition-fees-map.png", width = 8, height = 5, dpi = 320)
