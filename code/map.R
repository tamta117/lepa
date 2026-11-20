# make maps
library(sf)
library(ggplot2)
library(patchwork)
library(ggspatial)

# read in shapefiles
es <- st_read("shapefiles/El_Sauz.shp")
yt <- st_read("shapefiles/Yturria_small.shp")
la <- st_read("shapefiles/Laguna_Atascosa_NWR_small.shp")
tx <- st_read("shapefiles/County.shp")

# reproject el sauz and merge
study_area <- es %>%
  st_transform(st_crs(yt)) %>%
  bind_rows(yt, la)

# customize theme
my_theme <- theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_rect(fill = "white", color = "black"))

# plot main map
(main <- ggplot() +
    geom_sf(data = study_area)+
    annotation_scale(location = "bl", width_hint = 0.4, unit_category = "metric") + 
    my_theme)

# set extent
main_bbox <- st_bbox(study_area)
extent_box <- st_as_sfc(main_bbox)

# plot extent map
(extent <- ggplot() +
    geom_sf(data = tx)+
    geom_sf(data = extent_box, fill = NA, color = "red", linewidth = 0.5)+
    my_theme)

# add extent to main map
(final_map <- main + 
    inset_element(extent, left = 0.02, bottom = 0.02, right = 0.30, top = 0.3))
