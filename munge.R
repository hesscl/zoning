#### ACS based zoning-segregation analysis ------------------------------------

#dependencies
library(tidyverse)
library(haven)
library(sf)

#working directory = base of repo
setwd("H:/zoning")


#### A. Load NHGIS extracts ---------------------------------------------------

## Flat files

#2000 decennial census
census2000 <- read.csv("input/2000 Tract Tables/nhgis0132_ds151_2000_tract.csv") %>%
  select(-CTY_SUBA, -PLACEA, -(BLCK_GRPA:ZCTAA), -TRBL_CTA, -NAME)

#2008-2012 ACS
acs2012a <- read.csv("./input/2008-2012 ACS Tract Tables/nhgis0132_ds191_20125_2012_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(BLKGRPA:BTBGA), -NAME_E, -NAME_M)
acs2012b <- read.csv("./input/2008-2012 ACS Tract Tables/nhgis0132_ds192_20125_2012_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(CONCITA:BTTRA), -NAME_E, -NAME_M)
acs2012 <- inner_join(acs2012a, acs2012b)

#2013-2017 ACS
acs2018a <- read.csv("./input/2014-2018 ACS Tract Tables/nhgis0132_ds239_20185_2018_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(BLKGRPA:BTBGA), -NAME_E, -NAME_M)
acs2018b <- read.csv("./input/2014-2018 ACS Tract Tables/nhgis0132_ds240_20185_2018_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(CONCITA:BTTRA), -NAME_E, -NAME_M)
acs2018 <- inner_join(acs2018a, acs2018b)

## Spatial data

#CBSA shapefile
cbsa_shp <- read_sf("./input/CBSA/US_cbsa_2017.shp", 
                    stringsAsFactors = F)

#Metropolitan division shapefile
metdiv_shp <- read_sf("./input/CBSA/US_metdiv_2017.shp",
                      stringsAsFactors = F) %>%
  st_transform(crs = st_crs(cbsa_shp))

#2000 tract shapefile
census2000_shp <- read_sf("./input/2000 Tract Polygon/US_tract_2000.shp", 
                          stringsAsFactors = F) %>%
  st_transform(crs = st_crs(cbsa_shp))

#2008-2012 ACS shapefile
acs2012_shp <- read_sf("./input/2008-2012 ACS Tract Polygon/US_tract_2012.shp", 
                       stringsAsFactors = F) %>%
  st_transform(crs = st_crs(cbsa_shp))

#2013-2017 ACS shapefile
acs2018_shp <- read_sf("./input/2013-2017 ACS Tract Polygon/US_tract_2017.shp", 
                       stringsAsFactors = F) %>%
  st_transform(crs = st_crs(cbsa_shp))

#clustering for neighborhoods
clust <- read_sf("./input/hclust/US-hclust-zscore.shp", 
                 stringsAsFactors = F) %>%
  group_by(CBSAA, clust) %>%
  summarize() %>%
  st_transform(crs = st_crs(cbsa_shp))


#### Overview of goals --------------------------------------------------------

#IV:

#changes in % of new housing among metro area is SF dev in newer suburbs 
#   - metropolitan areas with exclusionary zoning will have higher
#     level of new housing built in such location + structure type

#DV:

#changes in concentration of poverty in metro area
#   - metropolitan areas with increasingly exclusive housing dev will see
#     increasing level of poverty concentration over time

#To-do:

#identify the metro (met div or CBSA) that a tract falls in at each period
#identify the location type the tract falls in at each period
#summarize poverty concentration for the whole metro at each period
#summarize where new housing is being constructed at each period
#describe association between changes over time (delta povconc and delta new housing)
#describe rank of metros on each measure
#map of metro showing new housing dev relative to places of increasing poverty
#weighted statistics by metro population

#### I. Identify CBSA for each tract ------------------------------------------

#Strategy: i. spatial intersection of tract shapefile with metropolitan boundary
#          shapefiles, 
#          ii. select metropolitan divisions if applicable, otherwise
#          use the CBSA code, 
#          iii. create a flat crosswalk table for appending the appropriate metro codes
#          to the tract table with

## Census 2000

census2000_shp <- st_join(census2000_shp, cbsa_shp %>% select(CBSAFP, geometry), 
                          left = FALSE)

census2000_shp <- st_join(census2000_shp, metdiv_shp %>% select(METDIVFP, geometry), 
                          left = FALSE)

census2000_metro_cw <- census2000_shp %>%
  filter(!is.na(CBSAFP)) %>%
  select(GISJOIN, CBSAFP, METDIVFP) %>%
  st_drop_geometry() %>%
  mutate(METRO = if_else(is.na(METDIVFP), CBSAFP, METDIVFP))

census2000 <- inner_join(census2000, census2000_metro_cw)

## ACS 2008-2012

acs2012_shp <- st_join(acs2012_shp, cbsa_shp %>% select(CBSAFP, geometry), 
                       left = FALSE)

acs2012_shp <- st_join(acs2012_shp, metdiv_shp %>% select(METDIVFP, geometry), 
                       left = FALSE)

acs2012_metro_cw <- acs2012_shp %>%
  filter(!is.na(CBSAFP)) %>%
  select(GISJOIN, CBSAFP, METDIVFP) %>%
  st_drop_geometry() %>%
  mutate(METRO = if_else(is.na(METDIVFP), CBSAFP, METDIVFP))

acs2012 <- inner_join(acs2012, acs2012_metro_cw)

## ACS 2014-2018

acs2018_shp <- st_join(acs2018_shp, cbsa_shp %>% select(CBSAFP, geometry), 
                       left = FALSE)

acs2018_shp <- st_join(acs2018_shp, metdiv_shp %>% select(METDIVFP, geometry), 
                       left = FALSE)

acs2018_metro_cw <- acs2018_shp %>%
  filter(!is.na(CBSAFP)) %>%
  select(GISJOIN, CBSAFP, METDIVFP) %>%
  st_drop_geometry() %>%
  mutate(METRO = if_else(is.na(METDIVFP), CBSAFP, METDIVFP))

acs2018 <- inner_join(acs2018, acs2018_metro_cw)


#### II. Identify location type for each tract --------------------------------

#Strategy: i. spatial intersection of tract centroids with polygons for location
#              types in the metropolitan area
#          ii. drop geometry and append the cluster values back to the tract data

census2000_clust_cw <- st_join(st_centroid(census2000_shp) %>% select(GISJOIN), 
                            clust %>% select(clust), 
                            left = FALSE)
acs2012_clust_cw <- st_join(st_centroid(acs2012_shp) %>% select(GISJOIN), 
                         clust %>% select(clust), 
                         left = FALSE)
acs2018_clust_cw <- st_join(st_centroid(acs2018_shp) %>% select(GISJOIN), 
                         clust %>% select(clust), 
                         left = FALSE)

census2000_clust_cw <- st_drop_geometry(census2000_clust_cw)
acs2012_clust_cw <- st_drop_geometry(acs2012_clust_cw)
acs2018_clust_cw <- st_drop_geometry(acs2018_clust_cw)


#### III. Summarize concentration of poverty and housing development ----------

#Strategy: i. create flag for high-poverty neighborhoods
#          ii. group tract tables by metro area
#          iii. compute share of poor individuals living in high poverty tract for each metro
#          iv. compute share of new housing units that were own-occ SF in newer suburbs










