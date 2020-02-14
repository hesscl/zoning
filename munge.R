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
census2000 <- read.csv("input/2000 Tract Tables/nhgis0128_ds151_2000_tract.csv")

#2008-2012 ACS
acs2012a <- read.csv("./input/2008-2012 ACS Tract Tables/nhgis0128_ds191_20125_2012_tract.csv")
acs2012b <- read.csv("./input/2008-2012 ACS Tract Tables/nhgis0128_ds192_20125_2012_tract.csv")

#2013-2017 ACS
acs2017a <- read.csv("./input/2013-2017 ACS Tract Tables/nhgis0128_ds233_20175_2017_tract.csv")
acs2017b <- read.csv("./input/2013-2017 ACS Tract Tables/nhgis0128_ds234_20175_2017_tract.csv")

## Spatial data

#CBSA shapefile
cbsa_shp <- read_sf("./input/CBSA/US_cbsa_2017.shp")

#Metropolitan division shapefile
metdiv_shp <- read_sf("./input/CBSA/US_metdiv_2017.shp")

#2000 tract shapefile
census2000_shp <- read_sf("./input/2000 Tract Polygon/US_tract_2000.shp")

#2008-2012 ACS shapefile
acs2012_shp <- read_sf("./input/2008-2012 ACS Tract Polygon/US_tract_2012.shp")

#2013-2017 ACS shapefile
acs2017_shp <- read_sf("./input/2013-2017 ACS Tract Polygon/US_tract_2017.shp")


#### A. Identify CBSA for each tract ------------------------------------------

census2000_shp <- st_join(census2000_shp, cbsa_shp %>% select(CBSAFP, geometry), 
                          left = FALSE)

census2000_shp <- st_join(census2000_shp, metdiv_shp %>% select(METDIVFP, geometry), 
                          left = FALSE)

census2000_cw <- census2000_shp %>%
  filter(!is.na(CBSAFP)) %>%
  select(GISJOIN, CBSAFP, METDIVFP) %>%
  st_drop_geometry() %>%
  mutate(METRO = if_else(is.na(METDIVFP), CBSAFP, METDIVFP))

census2000 <- inner_join(census2000, census2000_cw)


#### B. Prepare extracts for MCIB in Stata ------------------------------------

inc2000 <- census2000 %>%
  select(GISJOIN, METRO, starts_with("GMX")) %>%
  gather(-GISJOIN, -METRO, value = "COUNT", key = "INCBIN") %>%
  mutate(INCBIN = type.convert(str_extract(INCBIN, "[0-9][0-9][0-9]")),
         MIN = NA,
         MAX = NA,
         MIN = ifelse(INCBIN == "1", 0, MIN),
         MAX = ifelse(INCBIN == "1", 10000, MAX))


#### C. Save extracts for MCIB in Stata ---------------------------------------

write_dta(census2000, "./input/stata/census2000.dta")





