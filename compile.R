#### ACS based zoning-segregation analysis ------------------------------------

#dependencies
library(tidyverse)    #dplyr, ggplot, stringr
library(sf)           #spatial
library(sandwich)     #robust SE estimation
library(lmtest)       #coefficient testing
library(skimr)        #summary statistic table
library(haven)        #read and write stata files
library(stargazer)    #output to latex and ASCII

#working directory = base of repo
setwd("H:/zoning")

#avoid treating string columns as factors
options(stringsAsFactors = FALSE)


#### A. Load NHGIS extracts ---------------------------------------------------

## Tract flat files

#2008-2012 ACS (calling 2010 based on midpoint)
acs2010a <- read.csv("./input/2008-2012 ACS Tract Tables/nhgis0157_ds191_20125_2012_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(BLKGRPA:BTBGA), -NAME_E, -NAME_M)
acs2010b <- read.csv("./input/2008-2012 ACS Tract Tables/nhgis0157_ds192_20125_2012_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(CONCITA:BTTRA), -NAME_E, -NAME_M)
acs2010 <- inner_join(acs2010a, acs2010b)

#2014-2018 ACS (calling 2016 based on midpoint)
acs2016a <- read.csv("./input/2014-2018 ACS Tract Tables/nhgis0157_ds239_20185_2018_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(BLKGRPA:BTBGA), -NAME_E, -NAME_M)
acs2016b <- read.csv("./input/2014-2018 ACS Tract Tables/nhgis0157_ds240_20185_2018_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(CONCITA:BTTRA), -NAME_E, -NAME_M)
acs2016 <- inner_join(acs2016a, acs2016b)

#census regions table
regions <- read.csv("./input/census_regions.csv") %>%
  select(STATE=State, REGION=Region, DIVISION=Division)

## CBSA flat files

# 2008-2012 ACS
#acs2010a_cbsa <- read.csv("./input/2008-2012 ACS CBSA Tables/nhgis0134_ds191_20125_2012_cbsa.csv") %>%
#  select(-(GISJOIN:ANRCA), -(CSAA:NAME_E), -NAME_M)
#acs2010b_cbsa <- read.csv("./input/2008-2012 ACS CBSA Tables/nhgis0134_ds192_20125_2012_cbsa.csv") %>%
#  select(-(GISJOIN:ANRCA), -(CSAA:NAME_E), -NAME_M)
#acs2010_cbsa <- inner_join(acs2010a_cbsa, acs2010b_cbsa)

# 2014-2018 ACS
#acs2016a_cbsa <- read.csv("./input/2014-2018 ACS CBSA Tables/nhgis0133_ds239_20185_2018_cbsa.csv") %>%
#  select(-(GISJOIN:ANRCA), -(CSAA:NAME_E), -NAME_M)
#acs2016b_cbsa <- read.csv("./input/2014-2018 ACS CBSA Tables/nhgis0133_ds240_20185_2018_cbsa.csv") %>%
#  select(-(GISJOIN:ANRCA), -(CSAA:NAME_E), -NAME_M)
#acs2016_cbsa <- inner_join(acs2016a_cbsa, acs2016b_cbsa)

## Paul's file

conpov_2018 <- read_dta("./input/conpov_2018.dta")

## Metropolitan Division flat files

#ISSUE: the counties covered by metropolitan areas appear to have changed between ACS
#       time periods here. E.g. Boston, MA in 2014-2018 vs Boston-Quincy, MA, 
#       Tacoma, WA to Tacoma-Lakewood, WA. These complicate using metdiv data since
#       we are wanting to use median estimates in the model (i.e. cannot just aggregate)
#       Options: 1.) use CBSA, 2.) try to use IPUMS to estimate MetDiv values using
#       harmonized definition of metdiv 3.) use aggregation even if improper

# 2008-2012 ACS
#acs2010a_mdiv <- read.csv("./input/2008-2012 ACS MetDiv Tables/nhgis0135_ds191_20125_2012_metdiv.csv") %>%
#  select(-(GISJOIN:CSAA), -(NECTAA:NAME_E), -NAME_M)
#acs2010b_mdiv <- read.csv("./input/2008-2012 ACS MetDiv Tables/nhgis0135_ds192_20125_2012_metdiv.csv") %>%
#  select(-(GISJOIN:CSAA), -(NECTAA:NAME_E), -NAME_M)
#acs2010_mdiv <- inner_join(acs2010a_mdiv, acs2010b_mdiv)

# 2014-2018 ACS
#acs2016a_mdiv <- read.csv("./input/2014-2018 ACS MetDiv Tables/nhgis0135_ds239_20185_2018_metdiv.csv") %>%
#  select(-(GISJOIN:CSAA), -(NECTAA:NAME_E), -NAME_M)
#acs2016b_mdiv <- read.csv("./input/2014-2018 ACS MetDiv Tables/nhgis0135_ds240_20185_2018_metdiv.csv") %>%
#  select(-(GISJOIN:CSAA), -(NECTAA:NAME_E), -NAME_M)
#acs2016_mdiv <- inner_join(acs2016a_mdiv, acs2016b_mdiv)

## Spatial data

#CBSA shapefile
cbsa_shp <- read_sf("./input/2010 CBSA Polygon/US_cbsa_2010.shp") %>%
  filter(MEMI10 == "1")

#Metropolitan division shapefile
metdiv_shp <- read_sf("./input/2010 MetDiv Polygon/US_metdiv_2010.shp") %>%
  st_transform(crs = st_crs(cbsa_shp))

#2008-2012 ACS shapefile
acs2010_shp <- read_sf("./input/2008-2012 ACS Tract Polygon/US_tract_2012.shp") %>%
  st_transform(crs = st_crs(cbsa_shp))

#2013-2017 ACS shapefile (there is no updated geography available for 2014-2018)
acs2016_shp <- read_sf("./input/2013-2017 ACS Tract Polygon/US_tract_2017.shp") %>%
  st_transform(crs = st_crs(cbsa_shp))

#clustering for neighborhoods
clust <- read_sf("./input/hclust/US-hclust-no-filter.shp") %>%
  mutate(
    clust = case_when(
      hclust == 4 ~ "Older Suburb",
      hclust == 3 ~ "City",
      hclust == 2 ~ "Rural/Exurb",
      hclust == 1 ~ "Newer Suburb"
  )) %>%
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

## Rename CBSA/MetDiv name columns to avoid collisions on join
cbsa_shp <- cbsa_shp %>%
  rename(CBSANAME = NAME10,
         CBSANAMELSAD = NAMELSAD10,
         CBSAFP = CBSAFP10)

metdiv_shp <- metdiv_shp %>%
  rename(METDIVNAME = NAME10,
         METDIVNAMELSAD = NAMELSAD10,
         METDIVFP = METDIVFP10)

acs2010_cbsa <- acs2010_cbsa %>%
  rename(CBSANAMELSAD = CBSA,
         CBSAFP = CBSAA) %>%
  mutate(CBSAFP = as.character(CBSAFP)) %>%
  mutate(CBSAFP = ifelse(grepl("Honolulu", CBSANAMELSAD), 46520, CBSAFP),
         CBSAFP = ifelse(grepl("Los Angeles", CBSANAMELSAD), 31080, CBSAFP),
         CBSAFP = ifelse(grepl("Santa Maria", CBSANAMELSAD), 42200, CBSAFP))

acs2016_cbsa <- acs2016_cbsa %>%
  rename(CBSANAMELSAD = CBSA,
         CBSAFP = CBSAA) %>%
  mutate(CBSAFP = as.character(CBSAFP))

## function to join metro codes onto flatfiles based on spatial joins
metro_joiner <- function(tbl, shp){
  
  #spatial intersections
  tbl_metro_cw <- st_join(st_centroid(shp), 
                          cbsa_shp %>% select(CBSAFP, CBSANAME, CBSANAMELSAD, geometry), 
                          left = TRUE)
  
  tbl_metro_cw <- st_join(tbl_metro_cw, 
                          metdiv_shp %>% select(METDIVFP, METDIVNAME, METDIVNAMELSAD, geometry), 
                          left = TRUE)

  #select appropriate code and return flat table of codes + tract IDs
  tbl_metro_cw <- tbl_metro_cw %>%
    filter(!is.na(CBSAFP)) %>%
    select(GISJOIN, starts_with("CBSA"), starts_with("METDIV")) %>%
    st_drop_geometry() %>%
    mutate(METRO = if_else(is.na(METDIVFP), CBSAFP, METDIVFP),
           METRONAME = if_else(is.na(METDIVFP), CBSANAME, METDIVNAME),
           METRONAMELSAD = if_else(is.na(METDIVFP), CBSANAMELSAD, METDIVNAMELSAD))
  
  #1:1 join of tract data to crosswalk
  tbl <- inner_join(tbl, tbl_metro_cw)
  
  #return this version of the table
  tbl
}

## ACS 2008-2012
acs2010 <- metro_joiner(acs2010, acs2010_shp)

## ACS 2014-2018
acs2016 <- metro_joiner(acs2016, acs2016_shp)


#### II. Identify location type for each tract --------------------------------

#Strategy: i. spatial intersection of tract centroids with polygons for location
#              types in the metropolitan area
#          ii. drop geometry and append the cluster values back to the tract data
#          iii. append location indicator to the flat files

#point in polygon intersection for tract centroids in cluster regions
acs2010_clust_cw <- st_join(st_centroid(acs2010_shp) %>% select(GISJOIN), 
                            clust %>% select(clust), 
                            left = FALSE)
acs2016_clust_cw <- st_join(st_centroid(acs2016_shp) %>% select(GISJOIN), 
                            clust %>% select(clust), 
                            left = FALSE)

#drop the geometry column to make the crosswalks dataframes
acs2010_clust_cw <- st_drop_geometry(acs2010_clust_cw)
acs2016_clust_cw <- st_drop_geometry(acs2016_clust_cw)

#1:1 join of the spatially-intersected cluster value to the tract data
acs2010 <- inner_join(acs2010, acs2010_clust_cw)
acs2016 <- inner_join(acs2016, acs2016_clust_cw)


#### III. Identify the region for each tract ----------------------------------

#m:1 join of the tracts to the region table
acs2010 <- left_join(acs2010, regions)
acs2016 <- left_join(acs2016, regions)


#### IV. Compute median metro values using MCIB -------------------------------

#source mcib helper script that will process values through stata module
source("./mcib.R")

#load and clean files from mcib 
result_inc_2010 <- read_dta("./output/mcib/results_mcib_inc_2010.dta")
result_inc_2016 <- read_dta("./output/mcib/results_mcib_inc_2016.dta")
result_rent_2010 <- read_dta("./output/mcib/results_mcib_rent_2010.dta")
result_rent_2016 <- read_dta("./output/mcib/results_mcib_rent_2016.dta")

#now trim down into what we'll need for later, also divide so values represent thousands
result_inc_2010 <- result_inc_2010 %>%
  mutate(med_hh_inc = p50/1000, YEAR = 2010) %>%
  select(METRO, YEAR, med_hh_inc)

result_inc_2016 <- result_inc_2016 %>%
  mutate(med_hh_inc = p50/1000, YEAR = 2016) %>%
  select(METRO, YEAR, med_hh_inc)

result_rent_2010 <- result_rent_2010 %>%
  mutate(med_gross_rent = p50/1000, YEAR = 2010) %>%
  select(METRO, YEAR, med_gross_rent)

result_rent_2016 <- result_rent_2016 %>%
  mutate(med_gross_rent = p50/1000, YEAR = 2016) %>%
  select(METRO, YEAR, med_gross_rent)

#now assemble into panel 
result_inc <- bind_rows(result_inc_2010, result_inc_2016)
result_rent <- bind_rows(result_rent_2010, result_rent_2016)


#### V. Analyze concentration of poverty and housing development --------------

#Strategy: i. create flag for high-poverty neighborhoods
#          ii. group tract tables by metro area
#          iii. compute share of poor individuals living in high poverty tract for each metro
#          iv. compute share of new housing units that were own-occ SF in newer suburbs

#look at analysis using 20%, 30%, 40% definitions of poor neighborhood
#conditional on within-metro changes: 
# +non-spatial factors in metropolitan area 
# +changes in segregation

#assign a scalar to determine threshold for poor/affluent neighborhoods
thresh_set <- c(.20, .30, .40)

#load the function for running a complete analysis at set threshold level
source("./analysis.R")

#run analyses at each threshold levels
map(thresh_set, run_zoning_analysis) 

