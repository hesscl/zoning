
#dependencies
library(tidyverse)
library(sf)
library(gganimate)
library(ggrepel)
library(haven)

#working directory
setwd("H:/dissertation/metro-povtrends")

#read in tract flat file
tract_panel <- read_dta("./output/panel_for_stata.dta")

#load 2010 tract polygon shapefile
tract_poly <- read_sf(dsn = "./input/2010 Tract Polygon/US_tract_2010.shp",
                      layer = "US_tract_2010",
                      stringsAsFactors = FALSE)


#place shapefile for map labels
place <- read_sf(dsn = "./input/2010 Place Polygon/US_place_2010.shp", 
                 layer = "US_place_2010",
                 stringsAsFactors = F)

#static map theme
theme_map <- function(...) {
  default_font_color = "Black"
  default_background_color = "White"
  
  theme_minimal() +
    theme(
      text = element_text(color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      
      legend.position = "bottom",
      legend.key.width = unit(.75, "inch"),
      legend.key.height = unit(.2, "inch"), 
      legend.title = element_text(hjust = -.75, vjust = .75),
      
      ...
    )
}

#### Transform to Mercator for print --------------------------------------------

tract_poly <- st_transform(tract_poly, st_crs(3857))
place <- st_transform(place, st_crs(3857))


#### Philadelphia -------------------------------------------------------------

#first filter to the tract units of interest 
to_plot <- tract_panel %>% 
  filter(CBSAFP10 %in% c("37980"), year == 2016) %>% 
  select(GEOID10, year, clust, CBSAFP10) %>% 
  mutate(idx = row_number(),
         clust = as_factor(clust)) %>%
  inner_join(tract_poly) %>%
  st_as_sf() %>%
  group_by(clust) %>% 
  summarize()

#now create an object with the municipalities we want to label on map
phl_places <- place %>%
  filter(STATEFP10 %in% c("34", "42"), 
         NAME10 %in% c("Philadelphia", "Camden",  "Mortonville", "Levittown",
                       "Bryn Mawr", "Exton", "Somerdale")) %>%
  filter(!grepl("village", NAMELSAD10)) %>%
  arrange(NAME10)

#take the muni object and determine centroid, coerce to sf and then add NAME col
phl_labels <- phl_places %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), remove = F) %>%
  st_set_crs(value = st_crs(phl_places)) %>%
  st_transform(crs = st_crs(to_plot))
phl_labels$NAME10 <- phl_places$NAME10

bbox <- st_bbox(to_plot)

#make the ggplot
ggplot(to_plot) +
  geom_sf(aes(fill = clust), color = NA, lwd = 0.0) +
  geom_label_repel(data = phl_labels %>% filter(NAME10 %in% c("Levittown", "Somerdale")), 
                   aes(x = X , y = Y, label = NAME10),
                  nudge_y = 40000,
                  nudge_x = 60000,
                  direction    = "both",
                  angle        = 0,
                  segment.size = 0.5, 
                  min.segment.length = 2,
                  segment.color = "grey70",
                  point.padding = 0, fontface = "bold", size = 3) +
  geom_label_repel(data = phl_labels %>% filter(NAME10 %in% c("Camden")), 
                   aes(x = X , y = Y, label = NAME10),
                   nudge_y = 60000,
                   direction    = "both",
                   angle        = 0,
                   segment.size = 0.5, 
                   min.segment.length = 2,
                   segment.color = "grey70",
                   point.padding = 0, fontface = "bold", size = 3) +
  geom_label_repel(data = phl_labels %>% filter(NAME10 %in% c("Philadelphia", "Mortonville", "Bryn Mawr", "Exton")), 
                   aes(x = X , y = Y, label = NAME10),
                   nudge_y = 60000,
                   nudge_x = -60000,
                   direction    = "both",
                   angle        = 0,
                   segment.size = 0.5, 
                   min.segment.length = 2,
                   segment.color = "grey70",
                   point.padding = 0, fontface = "bold", size = 3) +
  scale_fill_viridis_d(direction = -1, option = "A") +
  theme_map() +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  coord_sf(ylim = c(bbox[2], bbox[4]+10000)) +
  labs(fill = "Location Type", x = "", y = "") +
  ggsave("./output/choro/philly_clust.pdf",
         width = 6, height = 6, dpi = 300)










