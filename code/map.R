# make maps
library(sf)
library(ggplot2)
library(patchwork)
library(ggspatial)
library(basemaps)
library(ggnewscale)

# get API key: api.census.gov
# set API key
tidycensus::census_api_key("569ce3ffeba344a720b919d06b5eda4df14f9a1d")

# read in shapefiles
es <- st_read("shapefiles/El_Sauz.shp")%>%
  mutate(property = "Ranch")
yt <- st_read("shapefiles/Yturria_small.shp")%>%
  mutate(property = "Ranch")
la <- st_read("shapefiles/Laguna_Atascosa_NWR_small.shp")%>%
  mutate(property = "Refuge")
tx <- st_read("shapefiles/County.shp")

# reproject el sauz and merge
study_area <- es %>%
  st_transform(st_crs(yt)) %>%
  bind_rows(yt, la)%>%
  st_transform(., 3857)

# customize theme
my_theme <- theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  panel.background = element_rect(fill = "white", color = "black"))

# define extent for base map
study_area_box <- st_transform(st_as_sfc(st_bbox(c(xmin = -97.69, xmax = -97.2, ymin = 26.1,  ymax = 26.65), crs = 4326)), 3857) %>% 
  st_bbox()

# plot main map
ggplot() +
  geom_sf(data = study_area,
          aes(fill = property),
          color = "black",
          linewidth = 0.3) +
  scale_fill_manual(values = c(
    "Ranch" = "darkseagreen3",
    "Refuge" = "tan"
  )) +
  labs(
    fill = "Study properties",
    title = "Ocelot Study Areas",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
(main <- ggplot() +
    basemap_gglayer(ext = extent_box, map_service = "esri", map_type = "world_topo_map")+
    scale_fill_identity() +
    new_scale_fill() +
    geom_sf(data = study_area, aes(fill = property))+
    scale_fill_manual(values = c("Ranch" = "darkseagreen3", "Refuge" = "tan"))+
    annotation_scale(location = "bl", width_hint = 0.4, unit_category = "metric") + # scale bar
    my_theme)

# set extent
extent_box <- st_as_sfc(st_bbox(study_area))

# plot extent map
(extent <- ggplot() +
    geom_sf(data = tx)+
    geom_sf(data = extent_box, fill = NA, color = "red", linewidth = 0.5)+
    my_theme)

# add extent to main map
(final_map <- main + 
    inset_element(extent, left = 0.02, bottom = 0.02, right = 0.5, top = 0.5))
