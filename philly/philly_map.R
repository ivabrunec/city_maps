## OpenStreetMap: Philadelphia

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

library(sf)
library(ggplot2)
library(osmdata)
library(showtext)
library(raster)
library(dplyr)

font_add_google(name = 'Roboto Condensed', family = 'roboto')
font_add_google(name = 'Abel', family = 'abel')
showtext_auto()

bb = c(-75.19550965879276, 39.93883290313669,
       -75.13388322619197, 39.966203963249264)

roads_all <- opq(bbox = bb) |>
  add_osm_feature(key = 'highway') |>
  osmdata_sf()

buildings <- opq(bb) |> 
  add_osm_feature(key = "building") %>%
  osmdata_sf()

landuse <- opq(bb) |>
  add_osm_feature(key = 'landuse', value = c('grass','forest')) |>
  osmdata_sf()

leisure <- opq(bb) |>
  add_osm_feature(key = 'leisure', value = c('garden','park')) |>
  osmdata_sf()

natural <- opq(bbox = bb) |>
  add_osm_feature(key = 'natural') |>
  osmdata_sf()

# circle
# code from: https://github.com/AbdoulMa/30DayMapChallenge/blob/main/Day8/day8_2022.R
long = -75.16366647413028
lat = 39.952585598153284

center_proj <-
  tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = crs(roads_all$osm_lines))

dist <-  1000
circle <- tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = crs(roads_all$osm_lines)) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = crs(roads_all$osm_lines))


roads_all_lines <- st_intersection(circle, roads_all$osm_lines)
buildings_lines <- st_intersection(circle, buildings$osm_polygons)
landuse_lines <- st_intersection(circle, landuse$osm_polygons) 
leisure_lines <- st_intersection(circle, leisure$osm_polygons)
natural_lines <- st_intersection(circle, natural$osm_polygons)

# color palette from: https://x.com/AlexCristache/status/1842448450731786742/photo/3
ggplot() +
  geom_sf(data = landuse_lines, color = '#024a43', fill = '#024a43') +
  geom_sf(data = leisure_lines, color = '#142d25', fill = '#142d25') +
  geom_sf(data = natural_lines, color = '#142d25', fill = '#142d25')+
  geom_sf(data = buildings_lines, color = '#f2e3d8', fill = '#aa423a',
          linewidth = .2) +
  geom_sf(data = roads_all_lines, color = '#db783e',linewidth = .3) +
  theme_void() +
  theme(plot.caption = element_text(hjust = .5, color = '#024a43', 
                                    family = 'roboto', size = 120),
        plot.subtitle = element_text(hjust = .5, size = 40, family = 'abel',
                                     color = '#142d25'),
        plot.background = element_rect(color = NA, fill = '#f2e3d8')) +
  labs(caption = 'Philadelphia, PA',
       subtitle = " 'the city of brotherly love' ")

ggsave('philly_map.png', height = 10, width = 8, dpi = 300)
