#### ACS based zoning-segregation analysis ------------------------------------

#dependencies
library(tidyverse)    #dplyr, ggplot, stringr
library(sf)           #spatial
library(sandwich)     #robust SE estimation
library(lmtest)       #coefficient testing

#working directory = base of repo
setwd("H:/zoning")

#avoid treating string columns as factors
options(stringsAsFactors = FALSE)


#### A. Load NHGIS extracts ---------------------------------------------------

## Tract flat files

#2000 decennial census
census2000a <- read.csv("input/2000 Tract Tables/nhgis0132_ds146_2000_tract.csv") %>%
  select(-CTY_SUBA, -PLACEA, -(BLCK_GRPA:ZCTAA), -TRBL_CTA, -NAME)
census2000b <- read.csv("input/2000 Tract Tables/nhgis0132_ds151_2000_tract.csv") %>%
  select(-CTY_SUBA, -PLACEA, -(BLCK_GRPA:ZCTAA), -TRBL_CTA, -NAME)
census2000 <- inner_join(census2000a, census2000b)

#2008-2012 ACS (calling 2010 based on midpoint)
acs2010a <- read.csv("./input/2008-2012 ACS Tract Tables/nhgis0132_ds191_20125_2012_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(BLKGRPA:BTBGA), -NAME_E, -NAME_M)
acs2010b <- read.csv("./input/2008-2012 ACS Tract Tables/nhgis0132_ds192_20125_2012_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(CONCITA:BTTRA), -NAME_E, -NAME_M)
acs2010 <- inner_join(acs2010a, acs2010b)

#2014-2018 ACS (calling 2016 based on midpoint)
acs2016a <- read.csv("./input/2014-2018 ACS Tract Tables/nhgis0132_ds239_20185_2018_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(BLKGRPA:BTBGA), -NAME_E, -NAME_M)
acs2016b <- read.csv("./input/2014-2018 ACS Tract Tables/nhgis0132_ds240_20185_2018_tract.csv") %>%
  select(-REGIONA, -DIVISIONA, -COUSUBA, -PLACEA, -(CONCITA:BTTRA), -NAME_E, -NAME_M)
acs2016 <- inner_join(acs2016a, acs2016b)

#census regions table
regions <- read.csv("./input/census_regions.csv") %>%
  select(STATE=State, REGION=Region, DIVISION=Division)

## CBSA flat files

# Census 2000
# -not available, would need to be computed from county or smaller units
# -issue is for median tables, can only approximate value

# 2008-2012 ACS
acs2010a_cbsa <- read.csv("./input/2008-2012 ACS CBSA Tables/nhgis0134_ds191_20125_2012_cbsa.csv") %>%
  select(-(GISJOIN:ANRCA), -(CSAA:NAME_E), -NAME_M)
acs2010b_cbsa <- read.csv("./input/2008-2012 ACS CBSA Tables/nhgis0134_ds192_20125_2012_cbsa.csv") %>%
  select(-(GISJOIN:ANRCA), -(CSAA:NAME_E), -NAME_M)
acs2010_cbsa <- inner_join(acs2010a_cbsa, acs2010b_cbsa)

# 2014-2018 ACS
acs2016a_cbsa <- read.csv("./input/2014-2018 ACS CBSA Tables/nhgis0133_ds239_20185_2018_cbsa.csv") %>%
  select(-(GISJOIN:ANRCA), -(CSAA:NAME_E), -NAME_M)
acs2016b_cbsa <- read.csv("./input/2014-2018 ACS CBSA Tables/nhgis0133_ds240_20185_2018_cbsa.csv") %>%
  select(-(GISJOIN:ANRCA), -(CSAA:NAME_E), -NAME_M)
acs2016_cbsa <- inner_join(acs2016a_cbsa, acs2016b_cbsa)

## Spatial data

#CBSA shapefile
cbsa_shp <- read_sf("./input/2013-2017 ACS CBSA Polygon/US_cbsa_2017.shp") %>%
  filter(MEMI == "1")

#Metropolitan division shapefile
metdiv_shp <- read_sf("./input/2013-2017 ACS CBSA Polygon/US_metdiv_2017.shp") %>%
  st_transform(crs = st_crs(cbsa_shp))

#2000 tract shapefile
census2000_shp <- read_sf("./input/2000 Tract Polygon/US_tract_2000.shp") %>%
  st_transform(crs = st_crs(cbsa_shp))

#2008-2012 ACS shapefile
acs2010_shp <- read_sf("./input/2008-2012 ACS Tract Polygon/US_tract_2012.shp") %>%
  st_transform(crs = st_crs(cbsa_shp))

#2013-2017 ACS shapefile
acs2016_shp <- read_sf("./input/2013-2017 ACS Tract Polygon/US_tract_2017.shp") %>%
  st_transform(crs = st_crs(cbsa_shp))

#clustering for neighborhoods
clust <- read_sf("./input/hclust/US-hclust-zscore.shp") %>%
  group_by(CBSAA, clust) %>%
  summarize() %>%
  ungroup() %>%
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
  rename(CBSANAME = NAME,
         CBSANAMELSAD = NAMELSAD)

metdiv_shp <- metdiv_shp %>%
  rename(METDIVNAME = NAME,
         METDIVNAMELSAD = NAMELSAD)

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

## Census 2000
census2000 <- metro_joiner(census2000, census2000_shp)

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
census2000_clust_cw <- st_join(st_centroid(census2000_shp) %>% select(GISJOIN), 
                               clust %>% select(clust), 
                               left = FALSE)
acs2010_clust_cw <- st_join(st_centroid(acs2010_shp) %>% select(GISJOIN), 
                            clust %>% select(clust), 
                            left = FALSE)
acs2016_clust_cw <- st_join(st_centroid(acs2016_shp) %>% select(GISJOIN), 
                            clust %>% select(clust), 
                            left = FALSE)

#drop the geometry column to make the crosswalks dataframes
census2000_clust_cw <- st_drop_geometry(census2000_clust_cw)
acs2010_clust_cw <- st_drop_geometry(acs2010_clust_cw)
acs2016_clust_cw <- st_drop_geometry(acs2016_clust_cw)

#1:1 join of the spatially-intersected cluster value to the tract data
census2000 <- inner_join(census2000, census2000_clust_cw)
acs2010 <- inner_join(acs2010, acs2010_clust_cw)
acs2016 <- inner_join(acs2016, acs2016_clust_cw)

#### III. Identify the region for each tract ----------------------------------

#m:1 join of the tracts to the region table
census2000 <- left_join(census2000, regions)
acs2010 <- left_join(acs2010, regions)
acs2016 <- left_join(acs2016, regions)

#test maps
#census2000 %>% filter(CBSAFP == "42660") %>% 
#  left_join(census2000_shp %>% select(GISJOIN, geometry)) %>% 
#  st_as_sf() %>% 
#  select(clust) %>% 
#  plot()

#acs2010 %>% filter(CBSAFP == "19100") %>% 
#  left_join(acs2010_shp %>% select(GISJOIN, geometry)) %>% 
#  st_as_sf() %>% 
#  select(clust) %>% 
#  plot()

#acs2016 %>% filter(CBSAFP == "35620") %>% 
#  left_join(acs2016_shp %>% select(GISJOIN, geometry)) %>% 
#  st_as_sf() %>% 
#  select(clust) %>% 
#  plot()


#### IV. Summarize concentration of poverty and housing development -----------

#Strategy: i. create flag for high-poverty neighborhoods
#          ii. group tract tables by metro area
#          iii. compute share of poor individuals living in high poverty tract for each metro
#          iv. compute share of new housing units that were own-occ SF in newer suburbs

#look at analysis using 20%, 30%, 40% definitions of poor neighborhood
#conditional on within-metro changes: 
# +non-spatial factors in metropolitan area 
# +changes in segregation

#assign a scalar to determine threshold for poor neighborhoods
poor_tract_thresh <- .20

#assign a function that will take an imput table of tract data and produce a metro summary
metro_summary <- function(tbl, year){
  tbl <- tbl %>%
    
   #group the tracts by metro area
    group_by(CBSAFP, CBSANAMELSAD) %>%
    
    #compute summary columns using various aggregation funs
    summarize(REGION = max(REGION),
              conc_pov = sum(poor_in_pov_tract)/sum(tot_poor),
              new_hu_excl_zon = sum(tot_own_occ_sfh_newer_suburbs)/sum(hu_blt_post_2000),
              dis_nhb_nhw = (.5) * sum(abs(tot_nhb/sum(tot_nhb) - tot_nhw/sum(tot_nhw))),
              dis_hsp_nhw = (.5) * sum(abs(tot_hsp/sum(tot_hsp) - tot_nhw/sum(tot_nhw))),
              dis_api_nhw = (.5) * sum(abs(tot_nhapi/sum(tot_nhapi) - tot_nhw/sum(tot_nhw))),
              tot_pop = sum(tot_pop),
              tot_nhw = sum(tot_nhw),
              tot_nhb = sum(tot_nhb),
              tot_nhapi = sum(tot_nhapi),
              tot_nhoth = sum(tot_nhoth),
              tot_hsp = sum(tot_hsp),
              pov_rat = sum(tot_poor)/sum(tot_pop)) %>%
    
    #mutate some compositions based on counts
    mutate(YEAR = year,
           pct_nhw = tot_nhw/tot_pop,
           pct_nhb = tot_nhb/tot_pop,
           pct_nhapi = tot_nhapi/tot_pop,
           pct_hsp = tot_hsp/tot_pop) %>%
    
    #arrange largest to smallest by pop
    arrange(desc(tot_pop)) %>%
    
    #ungroup table
    ungroup()
  
  #return the metro table as fn output
  tbl
}

## compute 2000 metropolitan summaries
census2000 <- census2000 %>%
  mutate(high_pov_tract = if_else(GN6001+GN6002 > 0, GN6001/(GN6001+GN6002) >= poor_tract_thresh, FALSE),
         tot_poor = GN6001,
         poor_in_pov_tract = if_else(high_pov_tract, tot_poor, 0L),
         hu_blt_post_2000 = GD6001+GD6002+GD6003+GD6004+GD6005+GD6006+GD6007+
           GD6064+GD6065+GD6066+GD6067+GD6068+GD6069+GD6070,
         own_occ_sfh_post_2000 = GD6001,
         newer_suburb = clust == "Newer Suburb",
         tot_own_occ_sfh_newer_suburbs = if_else(newer_suburb, own_occ_sfh_post_2000, 0L),
         tot_pop = FL5001,
         tot_nhw = FMS001,
         tot_nhb = FMS002,
         tot_nhapi = FMS004+FMS005,
         tot_nhoth = FMS003+FMS006+FMS007,
         tot_hsp = FMS008+FMS009+FMS010+FMS011+FMS012+FMS013+FMS014) 

metro_sum_2000 <- census2000 %>%
  metro_summary(year = 2000) 

## compute ACS 2008-2012 metropolitan summaries
acs2010 <- acs2010 %>%
  mutate(high_pov_tract = if_else(QUVE001 > 0, (QUVE002+QUVE003)/(QUVE001) >= poor_tract_thresh, FALSE),
         tot_poor = QUVE002+QUVE003,
         poor_in_pov_tract = if_else(high_pov_tract, tot_poor, 0L),
         tot_occ_hu = RGZE001,
         hu_blt_post_2000 = RGZE003+RGZE039,
         own_occ_sfh_post_2000 = RGZE004,
         newer_suburb = clust == "Newer Suburb",
         tot_own_occ_sfh_newer_suburbs = if_else(newer_suburb, own_occ_sfh_post_2000, 0L),
         tot_pop = QSPE001,
         tot_nhw = QSYE003,
         tot_nhb = QSYE004,
         tot_nhapi = QSYE006+QSYE007,
         tot_nhoth = QSYE005+QSYE008+QSYE009+QSYE010+QSYE011,
         tot_hsp = QSYE012)

acs2010_cbsa <- acs2010_cbsa %>%
  mutate(med_hh_inc = QU1E001 * 1.09, #chain to 2018 CPI
         tot_unemp = QXSE005,
         tot_in_labf = QXSE002,
         tot_16plus = QXSE001,
         pct_unemp = tot_unemp/tot_in_labf,
         tot_hu = QX7E001,
         tot_vac_hu = QX7E003,
         med_gross_rent = QZTE001 * 1.09,
         med_val = QZ6E001,
         med_yr_blt = QY2E001) %>% 
  select(CBSAFP, starts_with("med"), starts_with("pct"))

metro_sum_2010 <- acs2010 %>%
  metro_summary(year = 2010) %>%
  inner_join(acs2010_cbsa)

## compute ACS 2014-2018 metropolitan summaries
acs2016 <- acs2016 %>%
  mutate(high_pov_tract = if_else(AJY4E001 > 0, (AJY4E002+AJY4E003)/(AJY4E001) >= poor_tract_thresh, FALSE),
         tot_poor = AJY4E002+AJY4E003,
         poor_in_pov_tract = if_else(high_pov_tract, tot_poor, 0L),
         tot_occ_hu = AKL6E001,
         hu_blt_post_2000 = AKL6E003+AKL6E010+AKL6E046+AKL6E053,
         own_occ_sfh_post_2000 = AKL6E004+AKL6E011,
         newer_suburb = clust == "Newer Suburb",
         tot_own_occ_sfh_newer_suburbs = if_else(newer_suburb, own_occ_sfh_post_2000, 0L),
         tot_pop = AJWME001,
         tot_nhw = AJWVE003,
         tot_nhb = AJWVE004,
         tot_nhapi = AJWVE006+AJWVE007,
         tot_nhoth = AJWVE005+AJWVE008+AJWVE009+AJWVE010+AJWVE011,
         tot_hsp = AJWVE012) 

acs2016_cbsa <- acs2016_cbsa %>%
  mutate(med_hh_inc = AJZAE001,
         tot_unemp = AJ1CE005,
         tot_in_labf = AJ1CE002,
         tot_16plus = AJ1CE001,
         pct_unemp = tot_unemp/tot_in_labf,
         tot_hu = AJ1TE001,
         tot_vac_hu = AJ1TE003,
         med_gross_rent = AJ3EE001,
         med_val = AJ3QE001,
         med_yr_blt = AJ2NE001) %>%
  select(CBSAFP, starts_with("med"), starts_with("pct"))

metro_sum_2016 <- acs2016 %>%
  metro_summary(year = 2016) %>%
  inner_join(acs2016_cbsa)

#compile each time period's metro summaries into a single tbl
metro_sum <- bind_rows(metro_sum_2010, metro_sum_2016)

#assign a vector of top100 metro's names for filtering by total pop
top100 <- metro_sum_2016 %>%
  top_n(100, tot_pop) %>%
  pull(CBSAFP)

#look for incomplete panel obs
metro_sum %>% 
  group_by(CBSAFP) %>% 
  tally %>% 
  filter(n != 2) 

#filter for incomplete panel obs (boulder CO omitted from clustering)
metro_sum <- metro_sum %>%
  group_by(CBSAFP) %>%
  filter(!is.na(conc_pov)) %>%
  ungroup()

#now collapse panel into change scores for key variables
change <- metro_sum %>%
  filter(YEAR > 2000) %>%
  arrange(CBSAFP, YEAR) %>%
  group_by(CBSAFP, CBSANAMELSAD) %>%
  mutate(chg_new_hu_excl_zon = new_hu_excl_zon - lag(new_hu_excl_zon),
         chg_conc_pov = conc_pov - lag(conc_pov),
         chg_pov_rat = pov_rat - lag(pov_rat),
         chg_pct_nhw = pct_nhw - lag(pct_nhw),
         chg_pct_nhb = pct_nhb - lag(pct_nhb),
         chg_pct_hsp = pct_hsp - lag(pct_hsp),
         chg_pct_nhapi = pct_nhapi - lag(pct_nhapi),
         chg_pct_unemp = pct_unemp - lag(pct_unemp),
         chg_dis_nhb_nhw = dis_nhb_nhw - lag(dis_nhb_nhw),
         chg_dis_hsp_nhw = dis_hsp_nhw - lag(dis_hsp_nhw),
         chg_med_hh_inc = med_hh_inc - lag(med_hh_inc),
         chg_med_gross_rent = med_gross_rent - lag(med_gross_rent),
         chg_med_val = med_val - lag(med_val),
         chg_med_yr_blt = med_yr_blt - lag(med_yr_blt),
         lag_pov_rat = lag(pov_rat)) %>%
  filter(YEAR == 2016)


#### V. Descriptive statistics for panel --------------------------------------

#first reorder the levels to NE, MW, S, W since that is normal
change$REGION <- factor(change$REGION)
change$REGION <- factor(change$REGION, levels = levels(change$REGION)[c(2, 1, 3, 4)])


ggplot(change, aes(x = chg_pov_rat, y = chg_conc_pov)) + 
  geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "\nChange in Poverty Rate of Metro", 
       y = "Change in Poverty Concentration\n") +
  theme_minimal() +
  theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
  ggsave(filename = "./output/chg_conc_pov_by_chg_pov_rat.png",
         width = 6, height = 4, dpi = 300)

ggplot(change, aes(x = chg_new_hu_excl_zon, y = chg_conc_pov)) +
  geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "\nChange in Exclusionary Share of New HU", 
       y = "Change in Poverty Concentration\n") +
  theme_minimal() +
  theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
  ggsave(filename = "./output/chg_conc_pov_by_chg_excl_hu.png",
         width = 8, height = 6, dpi = 300)

ggplot(change, aes(x = chg_pov_rat, y = chg_conc_pov)) + 
  facet_grid(~ REGION) +
  geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "\nChange in Poverty Rate of Metro", 
       y = "Change in Poverty Concentration\n") +
  theme_minimal() +
  ggsave(filename = "./output/chg_pov_by_chg_conc_pov_and_region.png",
         width = 8, height = 6, dpi = 300)

ggplot(change, aes(x = chg_new_hu_excl_zon, y = chg_conc_pov)) + 
  facet_grid(~ REGION) +
  geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "\nChange in Exclusionary Share of New HU", 
       y = "Change in Poverty Concentration\n") +
  theme_minimal() +
  theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
  ggsave(filename = "./output/chg_conc_pov_by_chg_excl_hu_and_region.png",
         width = 8, height = 6, dpi = 300)


#### VI. Model estimation -----------------------------------------------------

#NB: imposing top 100 reduces differences between ols and wls,

## Base model without adjustment for changes in metropolitan context
base_formula <- chg_conc_pov ~ chg_new_hu_excl_zon + chg_pov_rat

#OLS
base_ols <- lm(base_formula, change)
summary(base_ols)

#OLS with HC SEs
coeftest(base_ols, vcov = vcovHC(base_ols, type = "HC0"))

#WLS
base_wls <- lm(base_formula, change, weights = tot_pop)
summary(base_wls)

#WLS with HC SEs
coeftest(base_wls, vcov = vcovHC(base_wls, type = "HC0"))


## Full model with adjustment for changes in metropolitan context
full_formula <- chg_conc_pov ~ chg_new_hu_excl_zon +
  chg_pct_nhw + chg_pct_nhb + chg_pct_hsp + chg_pct_nhapi + 
  chg_dis_nhb_nhw + chg_dis_hsp_nhw + chg_pov_rat + chg_pct_unemp +
  chg_med_hh_inc + chg_med_gross_rent +
  REGION

#OLS
full_ols <- lm(full_formula, change)
summary(full_ols)

#OLS with HC SEs
coeftest(full_ols, vcov = vcovHC(full_ols, type = "HC0"))

#WLS
full_wls <- lm(full_formula, change, weights = tot_pop)
summary(full_wls)

#WLS with HC SEs
coeftest(full_wls, vcov = vcovHC(full_wls, type = "HC0"))


#### VII. Model tables --------------------------------------------------------

stargazer::stargazer(full_ols, coeftest(full_ols, vcov = vcovHC(full_ols, type = "HC0")),
                     full_wls, coeftest(full_wls, vcov = vcovHC(full_wls, type = "HC0")),
                     column.labels = c("OLS", "OLS Robust SE", "WLS", "WLS Robust SE"))



#### VIII. Model data visualizations ------------------------------------------

## Predicted values with other variables held at means
pred_grid <- expand_grid(
  chg_conc_pov = round(seq(min(change$chg_conc_pov), max(change$chg_conc_pov), .01), 2),
  chg_new_hu_excl_zon = round(seq(min(change$chg_new_hu_excl_zon), max(change$chg_new_hu_excl_zon), .01), 2),
  chg_pct_nhw = mean(change$chg_pct_nhw),
  chg_pct_nhb = mean(change$chg_pct_nhb),
  chg_pct_hsp = mean(change$chg_pct_hsp),
  chg_pct_nhapi = mean(change$chg_pct_nhapi),
  chg_pct_unemp = mean(change$chg_pct_unemp),
  chg_dis_nhb_nhw = mean(change$chg_dis_nhb_nhw),
  chg_dis_hsp_nhw = mean(change$chg_dis_hsp_nhw),
  chg_pov_rat = mean(change$chg_dis_nhb_nhw),
  chg_med_hh_inc = mean(change$chg_med_hh_inc),
  chg_med_gross_rent = mean(change$chg_med_gross_rent),
  REGION = c("Northeast")
)

pred_grid$xb <- predict(full_wls, newdata = pred_grid, se.fit = T)$fit
pred_grid$se <- predict(full_wls, newdata = pred_grid, se.fit = T)$se.fit

ggplot(pred_grid, aes(x = chg_new_hu_excl_zon, y = xb, 
                      ymin = xb - 1.96 * se, ymax = xb + 1.96 * se)) +
  geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
  geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
  geom_ribbon(alpha = .25) +
  geom_line() +
  theme_minimal() +
  theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
  labs(x = "\nChange in Exclusionary Share of New HU",
       y = "Predicted Change in Poverty Concentration\n") + 
  ggsave(filename = "./output/wls_full_pred.png",
         width = 6, height = 4, dpi = 300)
  
## Maps

#a little bit of data preparation
detroit2010 <- acs2010 %>% filter(CBSAFP == "19820") %>% mutate(year = 2010)
detroit2016 <- acs2016 %>% filter(CBSAFP == "19820") %>% mutate(year = 2016)
detroit <- bind_rows(detroit2010, detroit2016) %>%
  mutate(pct_new_hu_sfh = own_occ_sfh_post_2000/tot_occ_hu) %>% #percent of occupied HU that was recent and SFH
  arrange(GISJOIN, year) %>%
  group_by(GISJOIN) %>%
  mutate(chg_new_hu_sfh = pct_new_hu_sfh - lag(pct_new_hu_sfh)) %>%
  filter(year == 2016) %>%
  left_join(acs2016_shp) %>%
  st_as_sf() 

ggplot(detroit, aes(fill = tot_poor/tot_pop >= .20)) + 
  geom_sf(lwd = 0.01) + 
  theme_minimal()

ggplot(detroit, aes(fill = chg_new_hu_sfh)) + 
  geom_sf(lwd = 0.01) + 
  scale_fill_gradient2(midpoint = 0) +
  theme_minimal()





