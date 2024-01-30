##__________________________________
##
## Mapping density and resistance
##__________________________________
##

# This script maps the most up-to-date BGRI data on black-grass density and
# resistance in England.

# Setup ---------------------------------------------------------------------------------------
rm(list = ls())
invisible(gc())
options(stringsAsFactors = F)

# Load libraries ---------------------------------------------------------------
library(tidyverse) 
library(sf) 
library(stars)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires) #find at GitHub repo ropensci/rnaturalearthhires@HEAD
library(ggrepel)
library(viridis)
library(cowplot)

# Set global variables ---------------------------------------------------------
# Set coordinate reference system to wgs84 = epsg 4326 (http://epsg.io/4326)
CRS = st_crs(4326)

# Load data --------------------------------------------------------------------
ab_res <- read.csv("output/ab_res_Atl-Cyc.csv", header = T)

# Map --------------------------------------------------------------------------
# Create sf object with geo_data data frame and CRS
points_sf <- st_as_sf(ab_res, coords = c("long_WGS", "lat_WGS"), crs = CRS)

# Build data frame of the points we have data for
points_data_frame <- points_sf %>%
  data.frame() %>%
  mutate(geometry = as.character(gsub("POINT", "", geometry))) %>%
  separate(col = geometry, into = c("longitude", "latitude"), sep = ",") %>%
  mutate(longitude = gsub("c\\(", "", longitude)) %>%
  mutate(latitude = gsub(")", "", latitude)) %>%
  mutate(longitude = as.numeric(longitude)) %>%
  mutate(latitude = as.numeric(latitude))

# Get map of UK with counties
ukcounties_sf <- rnaturalearth::ne_states("United Kingdom", returnclass = "sf")

# Plot data on map with county labels
ggplot() + 
  geom_sf(data = ukcounties_sf, fill = gray(0.8), color = gray(0.7)) + 
  geom_sf_text(
    data = ukcounties_sf %>% 
      dplyr::distinct(woe_name, .keep_all = TRUE) %>% 
      dplyr::filter( 
        woe_name %in% c(
          "East Riding of Yorkshire",
          "North Yorkshire",
          "South Yorkshire", 
          "Lincolnshire", 
          "Northamptonshire", 
          "Leicestershire", 
          "Nottinghamshire", 
          "Warwickshire", 
          "Buckinghamshire", 
          "Oxfordshire",
          "Norfolk", 
          "Cambridgeshire", 
          "Bedfordshire" 
        )) %>% 
      dplyr::mutate(
        woe_name = recode(woe_name, 
                          "East Riding of Yorkshire" = "E.R.Yorks (N)", 
                          "North Yorkshire" = "N.Yorks (N)", 
                          "South Yorkshire" = "S.Yorks (N)", 
                          "Lincolnshire" = "Lincs (E)", 
                          "Northamptonshire" = "Northants (C)", 
                          "Leicestershire" = "Leics (C)",
                          "Nottinghamshire" = "Notts (C)", 
                          "Warwickshire" = "Warw (C)", 
                          "Buckinghamshire" = "Bucks (C)", 
                          "Oxfordshire" = "Oxon (C)", 
                          "Norfolk" = "Norfolk (E)", 
                          "Cambridgeshire" = "Cambs (E)", 
                          "Bedfordshire" = "Beds (E)"
        )
      ),
    aes(label = woe_name),
    size = 4.5,
    color = "black",
    check_overlap = TRUE) +
  geom_sf(data = points_sf, aes(color = surv_mean, size = den_mean),
          alpha = 0.6, # adds some transparency to all the points (1 is opaque)
          show.legend = "point") + #with geom_sf() we need to use show.legend = "point" (i.e. point data here) to format the legends properly
  scale_size_binned(breaks=c(0,0.5,1.5,2.5,3.5),
                    labels=function(den_mean) as.character(round(den_mean, 1)))+
  coord_sf(xlim = c(-2.4, 1.8), ylim = c(51.6, 54.3)) +
  labs(color = "Resistance \n(% survival)", size = "Density") +
  theme(
    panel.background = element_blank(),
    axis.text.x=element_text(size=14),
    axis.text.y=element_text(size=14),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.title=element_text(size=14), 
    legend.text=element_text(size=12)
  )

ggsave("figures/Figure_1.png", height = 10, width = 10)

# Figure legend:
# Black-grass density and resistance in surveyed fields (n=125). 
# Resistance is the average resistance of a population to two selective 
# herbicides (Atlantis and cycloxidim). Density is average density across all 
# grid cells in a field, categorised as ordinal states: 
# 0-0.5 = absent; >0.5-<1.5 = low; >=1.5 - <2.5 = medium; >=2.5 - <3.5 = high; 
# >=3.5 = very high.