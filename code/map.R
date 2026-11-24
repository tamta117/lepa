# make maps
library(sf)
library(patchwork)
library(ggspatial)
library(basemaps)
library(ggnewscale)
library(tidyverse)

# get API key: api.census.gov
# set API key
tidycensus::census_api_key("569ce3ffeba344a720b919d06b5eda4df14f9a1d")

# read in shapefiles
es <- st_read("shapefiles/El_Sauz.shp")%>%
  mutate(property = "Ranch")%>%
  st_transform(., 3857)
yt <- st_read("shapefiles/Yturria_small.shp")%>%
  mutate(property = "Ranch")%>%
  st_transform(., 3857)
la <- st_read("shapefiles/Laguna_Atascosa_NWR_small.shp")%>%
  mutate(property = "Refuge")%>%
  st_transform(., 3857)
tx <- st_read("shapefiles/County_fixed.shp")%>%
  st_transform(., 3857)

# reproject el sauz and merge
study_area <- bind_rows(es, yt, la)

# customize theme
my_theme <- theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  legend.position = "none")

# define extent for base map
study_area_box <- st_buffer(study_area, dist = 5000)

# good maps
# osm_stadia: "alidade_smooth"
# maptiler: "dataviz", "ocean"
# esri: "world_light_gray_base", "world_ocean_base"

# plot main map
(main <- ggplot() +
    # basemap_gglayer(ext = study_area_box, map_service = "esri", map_type = "world_ocean_base")+
    # basemap_gglayer(ext = study_area_box, map_service = "osm_thunderforest", map_type = "neighbourhood", map_token = "28e7392e976c4c4ba606cc34ad592cec")+
    basemap_gglayer(ext = study_area_box, map_service = "maptiler", map_type = "dataviz", map_token = "65nCBw10xNgAshsPLqSF")+
    # basemap_gglayer(ext = study_area_box, map_service = "osm_stadia", map_type = "alidade_smooth", map_token = "98669daa-c865-4d13-854c-b5f5545be2aa")+
    scale_fill_identity() +
    new_scale_fill() +
    # geom_sf(data = study_area, aes(fill = property), alpha = 0.7, linewidth = 0.7)+
    geom_sf(data = es, fill = "#A7915A", color = "#75663f", linewidth = 0.7, alpha = 0.75)+
    geom_sf(data = yt, fill = "#A7915A", color = "#75663f", linewidth = 0.7, alpha = 0.75)+
    geom_sf(data = la, fill = "#767830", color = "#535422", linewidth = 0.7, alpha = 0.75)+
    annotation_scale(location = "bl", width_hint = 0.5, unit_category = "imperial") + # scale bar
    my_theme)
ggsave("figures/ranch_refuge_map.png", dpi = 300)

# set extent and buffer
extent_box <- st_as_sfc(st_bbox(study_area))
tx_box <- st_buffer(tx, dist = 5000)

# plot extent map
(extent <- ggplot() +
    # basemap_gglayer(ext = tx_box, map_service = "maptiler", map_type = "dataviz", map_token = "65nCBw10xNgAshsPLqSF")+
    # scale_fill_identity() +
    # new_scale_fill() +
    geom_sf(data = tx, fill = NA)+
    geom_sf(data = extent_box, fill = "#005b4d", color = "#005b4d", alpha = 0.8)+
    my_theme)
ggsave("figures/tx_extent.png", dpi = 300)

# add extent to main map
(final_map <- main + 
    inset_element(extent, left = 0.02, bottom = 0.02, right = 0.5, top = 0.5))
