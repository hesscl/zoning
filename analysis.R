#Strategy: -write a set of functions that can be used to produce the desired estimates
#          at a parameter value for high pov/aff neighborhood, 
#          -run descriptive plots and tables, save to output
#          -estimate models for concentration of pov/aff,
#          -save model tables to storage, create plot for focal variable's coefficient

clean_coef_tbl <- function(file, tex = TRUE){
  
  tbl <- read_lines(file)
  
  labels <- c("Chg Conc. Poverty", "Chg Conc. Black Poverty", "Chg Conc. Latino Poverty",
              "Chg Conc. White Poverty", "Chg Conc. White Affluence", "Chg in % Excl. HU", 
              "Chg % Black", "Chg % Latino", "Chg % Asian", "Chg % Other Race",
              "Chg % Unemployed", "Chg Median HH Income", "Chg Median Gross Rent", "Chg Poverty Rate",
              "Chg % Vacant HU")
  
  if(!tex) {
    variables <- c("chg_conc_pov", "chg_conc_blk_pov", "chg_conc_lat_pov",
                   "chg_conc_wht_pov", "chg_conc_wht_aff", "chg_new_hu_excl_zon", 
                   "chg_pct_nhb", "chg_pct_hsp", "chg_pct_nhapi", "chg_pct_nhoth",
                   "chg_pct_unemp", "chg_med_hh_inc", "chg_med_gross_rent", "chg_pov_rat",
                   "chg_pct_vac_hu")
    variables <- str_pad(variables, width = 30, side = "right")
    labels <- str_pad(labels, width = 30, side = "right")
  } else {
    variables <- c("chg\\\\_conc\\\\_pov", "chg\\\\_conc\\\\_blk\\\\_pov", "chg\\\\_conc\\\\_lat\\\\_pov",
                   "chg\\\\_conc\\\\_wht\\\\_pov", "chg\\\\_conc\\\\_wht\\\\_aff", "chg\\\\_new\\\\_hu\\\\_excl\\\\_zon", 
                   "chg\\\\_pct\\\\_nhb", "chg\\\\_pct\\\\_hsp", "chg\\\\_pct\\\\_nhapi", "chg\\\\_pct\\\\_nhoth",
                   "chg\\\\_pct\\\\_unemp", "chg\\\\_med\\\\_hh\\\\_inc", "chg\\\\_med\\\\_gross\\\\_rent", "chg\\\\_pov\\\\_rat",
                   "chg\\\\_pct\\\\_vac\\\\_hu")
  }
  
  for (i in seq_along(variables)){
    tbl <- gsub(variables[i], labels[i], tbl)
  }
  
  write_lines(tbl, file)
}

#assign a function that will take an input table of tract data and produce the CBSA-level IV 
cbsa_summary <- function(tbl){
  tbl <- tbl %>%
    group_by(CBSAFP, CBSANAME, CBSANAMELSAD) %>%
    mutate(new_hu_excl_zon = sum(tot_own_occ_sfh_excl_suburbs)/sum(occ_hu_blt_post_2000)) %>%
    ungroup()
}

#assign a function that will take an input table of tract data and produce a metro summary
metro_summary <- function(tbl, year){
  tbl <- tbl %>%
    
    #group the tracts by metro area
    group_by(METRO, METRONAME, METRONAMELSAD) %>%
    
    #compute summary columns using various aggregation funs
    summarize(CBSAFP = unique(CBSAFP),
              REGION = max(REGION),
              new_hu_excl_zon = unique(new_hu_excl_zon),
              conc_pov = ifelse(sum(tot_poor, na.rm = T) > 0,
                                 sum(poor_in_pov_tract, na.rm = T)/sum(tot_poor, na.rm = T), NA),
              conc_blk_pov = ifelse(sum(tot_blk_poor, na.rm = T) > 0,
                                     sum(blk_poor_in_pov_tract, na.rm = T)/sum(tot_blk_poor, na.rm = T), NA),
              conc_lat_pov = ifelse(sum(tot_lat_poor, na.rm = T) > 0,
                                     sum(lat_poor_in_pov_tract, na.rm = T)/sum(tot_lat_poor, na.rm = T), NA),
              conc_wht_pov = ifelse(sum(tot_wht_poor, na.rm = T) > 0,
                                     sum(wht_poor_in_pov_tract, na.rm = T)/sum(tot_wht_poor, na.rm = T), NA),
              conc_blk_lat_pov = ifelse(sum(tot_blk_poor+tot_lat_poor, na.rm = T) > 0,
                                         sum(blk_poor_in_pov_tract+lat_poor_in_pov_tract, na.rm = T)/sum(tot_blk_poor+tot_lat_poor, na.rm = T), NA),
              conc_wht_aff = ifelse(sum(tot_wht_aff, na.rm = T) > 0, 
                                 sum(wht_aff_in_aff_tract, na.rm = T)/sum(tot_wht_aff, na.rm = T), NA),
              dis_nhb_nhw = (.5) * sum(abs(tot_nhb/sum(tot_nhb) - tot_nhw/sum(tot_nhw))),
              dis_hsp_nhw = (.5) * sum(abs(tot_hsp/sum(tot_hsp) - tot_nhw/sum(tot_nhw))),
              dis_api_nhw = (.5) * sum(abs(tot_nhapi/sum(tot_nhapi) - tot_nhw/sum(tot_nhw))),
              tot_pop = sum(tot_pop),
              tot_nhw = sum(tot_nhw),
              tot_nhb = sum(tot_nhb),
              tot_blk = sum(tot_blk),
              tot_nhapi = sum(tot_nhapi),
              tot_nhoth = sum(tot_nhoth),
              tot_hsp = sum(tot_hsp),
              pov_rat = sum(tot_poor)/sum(tot_pop_pov_det),
              tot_vac_hu = sum(tot_vac_hu),
              tot_hu = sum(tot_hu),
              tot_in_labf = sum(tot_in_labf),
              tot_unemp = sum(tot_unemp),
              tot_new_suburb = sum(clust == "Newer Suburb")) %>%
    
    #mutate some compositions based on counts
    mutate(YEAR = year,
           pct_nhw = tot_nhw/tot_pop,
           pct_nhb = tot_nhb/tot_pop,
           pct_nhapi = tot_nhapi/tot_pop,
           pct_hsp = tot_hsp/tot_pop,
           pct_nhoth = tot_nhoth/tot_pop,
           pct_vac_hu = tot_vac_hu/tot_hu,
           pct_unemp = tot_unemp/tot_in_labf) %>%
    
    #arrange largest to smallest by pop
    arrange(desc(tot_pop)) %>%
    
    #ungroup table
    ungroup()
  
  #return the metro table as fn output
  tbl
}

#assign a function to process the results we want (possibly just source script?)
run_zoning_analysis <- function(tract_thresh){
  
  ## compute ACS 2008-2012 metropolitan summaries
  metro_sum_2010 <- acs2010 %>%
    mutate(high_pov_tract = ifelse(QUVE001 > 0, (QUVE002+QUVE003)/(QUVE001) >= tract_thresh, NA),
           tot_poor = QUVE002+QUVE003,
           tot_pop_pov_det = QUVE001,
           poor_in_pov_tract = ifelse(high_pov_tract, tot_poor, 0),
           tot_blk_poor = Q9SE002,
           tot_blk_pop_pov_det = Q9SE001,
           blk_poor_in_pov_tract = ifelse(high_pov_tract, tot_blk_poor, 0),
           tot_lat_poor = Q9ZE002,
           tot_lat_pop_pov_det = Q9ZE001,
           lat_poor_in_pov_tract = ifelse(high_pov_tract, tot_lat_poor, 0),
           tot_wht_poor = Q9YE002,
           tot_wht_pop_poor_det = Q9YE001,
           wht_poor_in_pov_tract = ifelse(high_pov_tract, tot_wht_poor, 0),
           tot_aff = QU0E015+QU0E016+QU0E017,
           tot_hh = QU0E001,
           high_aff_tract = ifelse(tot_hh > 0, tot_aff/tot_hh >= tract_thresh, NA),
           tot_wht_aff = RBVE015+RBVE016+RBVE017,
           wht_aff_in_aff_tract = ifelse(high_aff_tract, tot_wht_aff, 0),
           tot_occ_hu = RGZE001,
           occ_hu_blt_post_2000 = RGZE003+RGZE039,
           own_occ_sfh_post_2000 = RGZE004,
           excl_suburb = clust %in% c("Newer Suburb", "Rural/Exurb"),
           tot_own_occ_sfh_excl_suburbs = ifelse(excl_suburb, own_occ_sfh_post_2000, 0),
           tot_pop = QSPE001,
           tot_nhw = QSYE003,
           tot_nhb = QSYE004,
           tot_blk = QSYE004+QSYE014,
           tot_nhapi = QSYE006+QSYE007,
           tot_nhoth = QSYE005+QSYE008+QSYE009+QSYE010+QSYE011,
           tot_hsp = QSYE012,
           tot_unemp = QXSE005,
           tot_in_labf = QXSE003,
           tot_16plus = QXSE001,
           tot_hu = QX7E001,
           tot_vac_hu = QX7E003,
           tot_in_labf = QXSE003,
           tot_unemp = QXSE005) %>%
    cbsa_summary() %>%
    metro_summary(year = 2010)
  
  ## compute ACS 2014-2018 metropolitan summaries
  metro_sum_2016 <- acs2016 %>%
    mutate(high_pov_tract = ifelse(AJY4E001 > 0, (AJY4E002+AJY4E003)/(AJY4E001) >= tract_thresh, NA),
           tot_poor = AJY4E002+AJY4E003,
           tot_pop_pov_det = AJY4E001,
           poor_in_pov_tract = ifelse(high_pov_tract, tot_poor, 0),
           tot_blk_poor = AKD3E002,
           tot_blk_pop_pov_det = AKD3E001,
           blk_poor_in_pov_tract = ifelse(high_pov_tract, tot_blk_poor, 0),
           tot_lat_poor = AKEAE002,
           tot_lat_pop_pov_det = AKEAE002,
           lat_poor_in_pov_tract = ifelse(high_pov_tract, tot_lat_poor, 0),
           tot_wht_poor = AKD9E002,
           tot_wht_pop_pov_det = AKD9E001,
           wht_poor_in_pov_tract = ifelse(high_pov_tract, tot_wht_poor, 0),
           tot_aff = AJY9E015+AJY9E016+AJY9E017,
           tot_hh = AJY9E001,
           high_aff_tract = ifelse(tot_hh > 0, tot_aff/tot_hh >= tract_thresh, NA),
           tot_wht_aff = AKF7M015+AKF7M016+AKF7M017,
           wht_aff_in_aff_tract = ifelse(high_aff_tract, tot_wht_aff, 0),
           tot_occ_hu = AKL6E001,
           occ_hu_blt_post_2000 = AKL6E003+AKL6E010+AKL6E046+AKL6E053,
           own_occ_sfh_post_2000 = AKL6E004+AKL6E011,
           excl_suburb = clust %in% c("Newer Suburb", "Rural/Exurb"),
           tot_own_occ_sfh_excl_suburbs = ifelse(excl_suburb, own_occ_sfh_post_2000, 0),
           tot_pop = AJWME001,
           tot_nhw = AJWVE003,
           tot_nhb = AJWVE004,
           tot_blk = AJWVE004+AJWVE014,
           tot_nhapi = AJWVE006+AJWVE007,
           tot_nhoth = AJWVE005+AJWVE008+AJWVE009+AJWVE010+AJWVE011,
           tot_hsp = AJWVE012,
           tot_unemp = AJ1CE005,
           tot_in_labf = AJ1CE003,
           tot_16plus = AJ1CE001,
           tot_hu = AJ1TE001,
           tot_vac_hu = AJ1TE003,
           tot_in_labf = AJ1CE003,
           tot_unemp = AJ1CE005) %>%
    cbsa_summary() %>%
    metro_summary(year = 2016)
  
  #for testing values against paul's data
  #metro_sum_test <- inner_join(metro_sum_2016, conpov_2018,
  #                             by = c("METRO" = "newcode")) %>%
  #  select(METRO, conc_blk_pov, bconpov) %>%
  #  mutate(conc_pov = conc_blk_pov * 100,
  #         diff = conc_pov - bconpov)
  #MAD
  #mean(abs(metro_sum_test$conc_pov-metro_sum_test$bconpov))
  
  #compile each time period's metro summaries into a single tbl
  metro_sum <- bind_rows(metro_sum_2010, metro_sum_2016)
  
  #assign a vector of top100 metro's names for filtering by total pop
  #top100 <- metro_sum_2016 %>%
  #  top_n(100, tot_pop) %>%
  #  pull(METRO)
  
  #look for incomplete panel obs
  #metro_sum %>% 
  #  group_by(METRO) %>% 
  #  tally %>% 
  #  filter(n != 2)
  
  #now collapse panel into change scores for key variables
  change <- metro_sum %>%
    filter(YEAR > 2000) %>%
    inner_join(result_inc) %>%
    inner_join(result_rent) %>%
    arrange(METRO, YEAR) %>%
    group_by(METRO, METRONAMELSAD) %>%
    mutate(chg_new_hu_excl_zon = new_hu_excl_zon - lag(new_hu_excl_zon),
           chg_conc_pov = conc_pov - lag(conc_pov),
           chg_conc_blk_pov = conc_blk_pov - lag(conc_blk_pov),
           chg_conc_lat_pov = conc_lat_pov - lag(conc_lat_pov),
           chg_conc_wht_pov = conc_wht_pov - lag(conc_wht_pov),
           chg_conc_wht_aff = conc_wht_aff - lag(conc_wht_aff),
           chg_pov_rat = pov_rat - lag(pov_rat),
           chg_pct_nhw = pct_nhw - lag(pct_nhw),
           chg_pct_nhb = pct_nhb - lag(pct_nhb),
           chg_pct_hsp = pct_hsp - lag(pct_hsp),
           chg_pct_nhapi = pct_nhapi - lag(pct_nhapi),
           chg_pct_nhoth = pct_nhoth - lag(pct_nhoth),
           chg_pct_unemp = pct_unemp - lag(pct_unemp),
           chg_dis_nhb_nhw = dis_nhb_nhw - lag(dis_nhb_nhw),
           chg_dis_hsp_nhw = dis_hsp_nhw - lag(dis_hsp_nhw),
           chg_med_hh_inc = med_hh_inc - lag(med_hh_inc),
           chg_med_gross_rent = med_gross_rent - lag(med_gross_rent),
           chg_pct_vac_hu = pct_vac_hu - lag(pct_vac_hu),
           lag_pov_rat = lag(pov_rat)) %>%
    filter(YEAR == 2016)
  
  #change omitted category to non-Hispanic white, use change in other as term
  #look at omitting dissimilarity
  #coeffiicent tables for all metros at .20 and .40 thresholds
  #plot for predictions
  
  change <- change %>%
    filter(!is.na(conc_pov), !is.na(REGION), tot_pop >= 50000)
  
  
  #### Descriptive statistics for panel ---------------------------------------
  
  #first reorder the levels to NE, MW, S, W since that is normal
  change$REGION <- factor(change$REGION)
  change$REGION <- factor(change$REGION, levels = levels(change$REGION)[c(2, 1, 3, 4)])
  
  #plot the change score data
  ggplot(change, aes(x = chg_pov_rat, y = chg_conc_pov)) + 
    geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
    geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = "\nChange in Poverty Rate of Metro", 
         y = "Change in Poverty Concentration\n") +
    theme_minimal() +
    theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
    ggsave(filename = paste0("./output/desc/chg_conc_pov_", tract_thresh,"_by_chg_pov_rat.png"),
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
    ggsave(filename = paste0("./output/desc/chg_conc_pov_", tract_thresh, "_by_chg_excl_hu.png"),
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
    ggsave(filename = paste0("./output/desc/chg_pov_by_chg_conc_pov_", tract_thresh, "_and_region.png"),
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
    ggsave(filename = paste0("./output/desc/chg_conc_pov_", tract_thresh, "_by_chg_excl_hu_and_region.png"),
           width = 8, height = 6, dpi = 300)
  
  #Latex table of descriptives
  sum_tbl <- change %>% 
    ungroup() %>%
    select(chg_conc_pov, chg_conc_blk_pov, chg_conc_lat_pov, chg_conc_wht_pov,
           chg_conc_wht_aff, chg_new_hu_excl_zon, chg_pct_nhb, chg_pct_hsp, 
           chg_pct_nhapi, chg_pct_nhoth, chg_pov_rat, chg_pct_unemp, chg_med_hh_inc,
           chg_med_gross_rent, chg_pct_vac_hu) %>%
    skim() %>%
    as_tibble() %>% select(variable=skim_variable, 
                           mean=numeric.mean,
                           sd=numeric.sd, 
                           min=numeric.p0,
                           max=numeric.p100)
  
  sum_tbl
  print(xtable::xtable(sum_tbl, digits = 3), include.rownames = FALSE,
        file = paste0("./output/desc/desc_table_", tract_thresh, "_thresh_data.tex"))
  print(xtable::xtable(sum_tbl, digits = 3), include.rownames = FALSE,
        file = paste0("./output/desc/desc_table_", tract_thresh, "_thresh_data.txt"))
  
  #### Model estimation -------------------------------------------------------
  
  ### Overall concentration of poverty
  
  ## Base model without adjustment for changes in metropolitan context
  base_form_pov <- chg_conc_pov ~ chg_new_hu_excl_zon + chg_pov_rat 
  
  #OLS
  base_ols_pov <- lm(base_form_pov, change)
  summary(base_ols_pov)
  
  #OLS with HC SEs
  coeftest(base_ols_pov, vcov = vcovHC(base_ols_pov, type = "HC1"))
  
  #WLS
  base_wls_pov <- lm(base_form_pov, change, weights = tot_pop)
  summary(base_wls_pov)
  
  #WLS with HC SEs
  coeftest(base_wls_pov, vcov = vcovHC(base_wls_pov, type = "HC1"))
  
  ## Full model with adjustment for changes in metropolitan context
  full_form_pov <- chg_conc_pov ~ chg_new_hu_excl_zon + 
    chg_pct_nhb + chg_pct_hsp + chg_pct_nhapi + chg_pct_nhoth + 
    chg_pct_unemp + chg_med_hh_inc + chg_med_gross_rent + chg_pov_rat +
    chg_pct_vac_hu
  
  #OLS
  full_ols_pov <- lm(full_form_pov, change)
  summary(full_ols_pov)
  
  #OLS with HC SEs
  coeftest(full_ols_pov, vcov = vcovHC(full_ols_pov, type = "HC1"))
  
  #WLS
  full_wls_pov <- lm(full_form_pov, change, weights = tot_pop)
  summary(full_wls_pov)
  
  #WLS with HC SEs
  coeftest(full_wls_pov, vcov = vcovHC(full_wls_pov, type = "HC1"))
  
  
  ## Concentration of black poverty
  
  ## Base model without adjustment for changes in metropolitan context
  base_form_blk_pov <- chg_conc_blk_pov ~ chg_new_hu_excl_zon + chg_pov_rat 
  
  #OLS
  base_ols_blk_pov <- lm(base_form_blk_pov, change)
  summary(base_ols_blk_pov)
  
  #OLS with HC SEs
  coeftest(base_ols_blk_pov, vcov = vcovHC(base_ols_blk_pov, type = "HC1"))
  
  #WLS
  base_wls_blk_pov <- lm(base_form_blk_pov, change, weights = tot_nhb)
  summary(base_wls_blk_pov)
  
  #WLS with HC SEs
  coeftest(base_wls_blk_pov, vcov = vcovHC(base_wls_blk_pov, type = "HC1"))
  
  ## Full model with adjustment for changes in metropolitan context
  full_form_blk_pov <- chg_conc_blk_pov ~ chg_new_hu_excl_zon + 
    chg_pct_nhb + chg_pct_hsp + chg_pct_nhapi + chg_pct_nhoth + 
    chg_pct_unemp + chg_med_hh_inc + chg_med_gross_rent + chg_pov_rat +
    chg_pct_vac_hu
  
  #OLS
  full_ols_blk_pov <- lm(full_form_blk_pov, change)
  summary(full_ols_blk_pov)
  
  #OLS with HC SEs
  coeftest(full_ols_blk_pov, vcov = vcovHC(full_ols_blk_pov, type = "HC1"))
  
  #WLS
  full_wls_blk_pov <- lm(full_form_blk_pov, change, weights = tot_nhb)
  summary(full_wls_blk_pov)
  
  #WLS with HC SEs
  coeftest(full_wls_blk_pov, vcov = vcovHC(full_wls_blk_pov, type = "HC1"))
  
  ## Concentration of latino poverty
  
  ## Base model without adjustment for changes in metropolitan context
  base_form_lat_pov <- chg_conc_lat_pov ~ chg_new_hu_excl_zon + chg_pov_rat 
  
  #OLS
  base_ols_lat_pov <- lm(base_form_lat_pov, change)
  summary(base_ols_lat_pov)
  
  #OLS with HC SEs
  coeftest(base_ols_lat_pov, vcov = vcovHC(base_ols_lat_pov, type = "HC1"))
  
  #WLS
  base_wls_lat_pov <- lm(base_form_lat_pov, change, weights = tot_hsp)
  summary(base_wls_lat_pov)
  
  #WLS with HC SEs
  coeftest(base_wls_lat_pov, vcov = vcovHC(base_wls_lat_pov, type = "HC1"))
  
  ## Full model with adjustment for changes in metropolitan context
  full_form_lat_pov <- chg_conc_lat_pov ~ chg_new_hu_excl_zon + 
    chg_pct_nhb + chg_pct_hsp + chg_pct_nhapi + chg_pct_nhoth + 
    chg_pct_unemp + chg_med_hh_inc + chg_med_gross_rent + chg_pov_rat +
    chg_pct_vac_hu
  
  #OLS
  full_ols_lat_pov <- lm(full_form_lat_pov, change)
  summary(full_ols_lat_pov)
  
  #OLS with HC SEs
  coeftest(full_ols_lat_pov, vcov = vcovHC(full_ols_lat_pov, type = "HC1"))
  
  #WLS
  full_wls_lat_pov <- lm(full_form_lat_pov, change, weights = tot_hsp)
  summary(full_wls_lat_pov)
  
  #WLS with HC SEs
  coeftest(full_wls_lat_pov, vcov = vcovHC(full_wls_lat_pov, type = "HC1"))
  
  ## Concentration of white poverty
  
  ## Base model without adjustment for changes in metropolitan context
  base_form_wht_pov <- chg_conc_wht_pov ~ chg_new_hu_excl_zon + chg_pov_rat 
  
  #OLS
  base_ols_wht_pov <- lm(base_form_wht_pov, change)
  summary(base_ols_wht_pov)
  
  #OLS with HC SEs
  coeftest(base_ols_wht_pov, vcov = vcovHC(base_ols_wht_pov, type = "HC1"))
  
  #WLS
  base_wls_wht_pov <- lm(base_form_wht_pov, change, weights = tot_nhw)
  summary(base_wls_wht_pov)
  
  #WLS with HC SEs
  coeftest(base_wls_wht_pov, vcov = vcovHC(base_wls_wht_pov, type = "HC1"))
  
  ## Full model with adjustment for changes in metropolitan context
  full_form_wht_pov <- chg_conc_wht_pov ~ chg_new_hu_excl_zon + 
    chg_pct_nhb + chg_pct_hsp + chg_pct_nhapi + chg_pct_nhoth + 
    chg_pct_unemp + chg_med_hh_inc + chg_med_gross_rent + chg_pov_rat +
    chg_pct_vac_hu
  
  #OLS
  full_ols_wht_pov <- lm(full_form_wht_pov, change)
  summary(full_ols_wht_pov)
  
  #OLS with HC SEs
  coeftest(full_ols_wht_pov, vcov = vcovHC(full_ols_wht_pov, type = "HC1"))
  
  #WLS
  full_wls_wht_pov <- lm(full_form_wht_pov, change, weights = tot_nhw)
  summary(full_wls_wht_pov)
  
  #WLS with HC SEs
  coeftest(full_wls_wht_pov, vcov = vcovHC(full_wls_wht_pov, type = "HC1"))
  
  
  ### Concentration of white affluence
  
  ## Base model without adjustment for changes in metropolitan context
  base_form_aff <- chg_conc_wht_aff ~ chg_new_hu_excl_zon + chg_pov_rat
  
  #OLS
  base_ols_aff <- lm(base_form_aff, change)
  summary(base_ols_aff)
  
  #OLS with HC SEs
  coeftest(base_ols_aff, vcov = vcovHC(base_ols_aff, type = "HC1"))
  
  #WLS
  base_wls_aff <- lm(base_form_aff, change, weights = tot_nhw)
  summary(base_wls_aff)
  
  #WLS with HC SEs
  coeftest(base_wls_aff, vcov = vcovHC(base_wls_aff, type = "HC1"))
  
  
  ## Full model with adjustment for changes in metropolitan context
  full_form_aff <- chg_conc_wht_aff ~ chg_new_hu_excl_zon +
    chg_pct_nhb + chg_pct_hsp + chg_pct_nhapi + chg_pct_nhoth + 
    chg_pov_rat + chg_pct_unemp + chg_med_hh_inc + chg_med_gross_rent +
    chg_pct_vac_hu
  
  #OLS
  full_ols_aff <- lm(full_form_aff, change)
  summary(full_ols_aff)
  
  #OLS with HC SEs
  coeftest(full_ols_aff, vcov = vcovHC(full_ols_aff, type = "HC1"))
  
  #WLS
  full_wls_aff <- lm(full_form_aff, change, weights = tot_nhw)
  summary(full_wls_aff)
  
  #WLS with HC SEs
  coeftest(full_wls_aff, vcov = vcovHC(full_wls_aff, type = "HC1"))
  
  
  #### VIII. Model tables --------------------------------------------------------
  
  #concentration of poverty
  stargazer::stargazer(full_ols_pov, coeftest(full_ols_pov, vcov = vcovHC(full_ols_pov, type = "HC1")),
                       full_wls_pov, coeftest(full_wls_pov, vcov = vcovHC(full_wls_pov, type = "HC1")),
                       column.labels = c("OLS", "OLS Robust SE", "WLS", "WLS Robust SE"),
                       model.names = FALSE,
                       dep.var.labels.include = FALSE,
                       out = c(paste0("./output/model_tbl/pov_models_", tract_thresh, "_thresh.tex"),
                               paste0("./output/model_tbl/pov_models_", tract_thresh, "_thresh.txt"))) 
  
  #concentration of black poverty
  stargazer::stargazer(full_ols_blk_pov, coeftest(full_ols_blk_pov, vcov = vcovHC(full_ols_blk_pov, type = "HC1")),
                       full_wls_blk_pov, coeftest(full_wls_blk_pov, vcov = vcovHC(full_wls_blk_pov, type = "HC1")),
                       column.labels = c("OLS", "OLS Robust SE", "WLS", "WLS Robust SE"),
                       model.names = FALSE,
                       dep.var.labels.include = FALSE,
                       out = c(paste0("./output/model_tbl/blk_pov_models_", tract_thresh, "_thresh.tex"),
                               paste0("./output/model_tbl/blk_pov_models_", tract_thresh, "_thresh.txt"))) 
  
  #concentration of latino poverty
  stargazer::stargazer(full_ols_lat_pov, coeftest(full_ols_lat_pov, vcov = vcovHC(full_ols_lat_pov, type = "HC1")),
                       full_wls_lat_pov, coeftest(full_wls_lat_pov, vcov = vcovHC(full_wls_lat_pov, type = "HC1")),
                       model.names = FALSE,
                       dep.var.labels.include = FALSE,
                       column.labels = c("OLS", "OLS Robust SE", "WLS", "WLS Robust SE"),
                       out = c(paste0("./output/model_tbl/lat_pov_models_", tract_thresh, "_thresh.tex"),
                               paste0("./output/model_tbl/lat_pov_models_", tract_thresh, "_thresh.txt"))) 
  
  #concentration of white poverty
  stargazer::stargazer(full_ols_wht_pov, coeftest(full_ols_wht_pov, vcov = vcovHC(full_ols_wht_pov, type = "HC1")),
                       full_wls_wht_pov, coeftest(full_wls_wht_pov, vcov = vcovHC(full_wls_wht_pov, type = "HC1")),
                       model.names = FALSE,
                       dep.var.labels.include = FALSE,
                       column.labels = c("OLS", "OLS Robust SE", "WLS", "WLS Robust SE"),
                       out = c(paste0("./output/model_tbl/wht_pov_models_", tract_thresh, "_thresh.tex"),
                               paste0("./output/model_tbl/wht_pov_models_", tract_thresh, "_thresh.txt"))) 
  
  #concentration of white affluence
  stargazer::stargazer(full_ols_aff, coeftest(full_ols_aff, vcov = vcovHC(full_ols_aff, type = "HC1")),
                       full_wls_aff, coeftest(full_wls_aff, vcov = vcovHC(full_wls_aff, type = "HC1")),
                       model.names = FALSE,
                       dep.var.labels.include = FALSE,
                       column.labels = c("OLS", "OLS Robust SE", "WLS", "WLS Robust SE"),
                       out = c(paste0("./output/model_tbl/aff_models_", tract_thresh, "_thresh.tex"),
                               paste0("./output/model_tbl/aff_models_", tract_thresh, "_thresh.txt")))
  
  stargazer::stargazer(coeftest(full_wls_pov, vcov = vcovHC(full_wls_pov, type = "HC1")),
                       coeftest(full_wls_blk_pov, vcov = vcovHC(full_wls_blk_pov, type = "HC1")),
                       coeftest(full_wls_lat_pov, vcov = vcovHC(full_wls_lat_pov, type = "HC1")),
                       coeftest(full_wls_wht_pov, vcov = vcovHC(full_wls_wht_pov, type = "HC1")),
                       coeftest(full_wls_aff, vcov = vcovHC(full_wls_aff, type = "HC1")),
                       model.names = FALSE,
                       dep.var.labels.include = FALSE,
                       column.labels = c("Overall", "Black", "Latino", "White", "White"),
                       out = c(paste0("./output/model_tbl/weighted_models_", tract_thresh, "_thresh.tex"),
                               paste0("./output/model_tbl/weighted_models_", tract_thresh, "_thresh.txt")))
  
  tex_files <- paste0("./output/model_tbl/", 
                      list.files("./output/model_tbl/", pattern = "tex"))  
  txt_files <- paste0("./output/model_tbl/", 
                      list.files("./output/model_tbl/", pattern = "txt")) 
  
  map(tex_files, clean_coef_tbl)
  map(txt_files, clean_coef_tbl, tex = FALSE)
  
  #### IX. Model data visualizations ------------------------------------------
  
  ## Predicted values with other variables held at means
  pred_grid_pov <- expand_grid(
    chg_conc_pov = round(seq(min(change$chg_conc_pov), max(change$chg_conc_pov), .01), 2),
    chg_new_hu_excl_zon = round(seq(min(change$chg_new_hu_excl_zon), max(change$chg_new_hu_excl_zon), .01), 2),
    chg_pct_nhb = mean(change$chg_pct_nhb),
    chg_pct_hsp = mean(change$chg_pct_hsp),
    chg_pct_nhapi = mean(change$chg_pct_nhapi),
    chg_pct_nhoth = mean(change$chg_pct_nhoth),
    chg_pct_unemp = mean(change$chg_pct_unemp),
    chg_pov_rat = mean(change$chg_pov_rat),
    chg_med_hh_inc = mean(change$chg_med_hh_inc),
    chg_med_gross_rent = mean(change$chg_med_gross_rent),
    chg_pct_vac_hu = mean(change$chg_pct_vac_hu)
  )
  
  pred_grid_blk_pov <- expand_grid(
    chg_conc_blk_pov = round(seq(min(change$chg_conc_blk_pov), max(change$chg_conc_blk_pov), .01), 2),
    chg_new_hu_excl_zon = round(seq(min(change$chg_new_hu_excl_zon), max(change$chg_new_hu_excl_zon), .01), 2),
    chg_pct_nhb = mean(change$chg_pct_nhb),
    chg_pct_hsp = mean(change$chg_pct_hsp),
    chg_pct_nhapi = mean(change$chg_pct_nhapi),
    chg_pct_nhoth = mean(change$chg_pct_nhoth),
    chg_pct_unemp = mean(change$chg_pct_unemp),
    chg_pov_rat = mean(change$chg_pov_rat),
    chg_med_hh_inc = mean(change$chg_med_hh_inc),
    chg_med_gross_rent = mean(change$chg_med_gross_rent),
    chg_pct_vac_hu = mean(change$chg_pct_vac_hu)
  )
  
  pred_grid_lat_pov <- expand_grid(
    chg_conc_lat_pov = round(seq(min(change$chg_conc_lat_pov), max(change$chg_conc_lat_pov), .01), 2),
    chg_new_hu_excl_zon = round(seq(min(change$chg_new_hu_excl_zon), max(change$chg_new_hu_excl_zon), .01), 2),
    chg_pct_nhb = mean(change$chg_pct_nhb),
    chg_pct_hsp = mean(change$chg_pct_hsp),
    chg_pct_nhapi = mean(change$chg_pct_nhapi),
    chg_pct_nhoth = mean(change$chg_pct_nhoth),
    chg_pct_unemp = mean(change$chg_pct_unemp),
    chg_pov_rat = mean(change$chg_pov_rat),
    chg_med_hh_inc = mean(change$chg_med_hh_inc),
    chg_med_gross_rent = mean(change$chg_med_gross_rent),
    chg_pct_vac_hu = mean(change$chg_pct_vac_hu)
  )
  
  pred_grid_wht_pov <- expand_grid(
    chg_conc_wht_pov = round(seq(min(change$chg_conc_wht_pov), max(change$chg_conc_wht_pov), .01), 2),
    chg_new_hu_excl_zon = round(seq(min(change$chg_new_hu_excl_zon), max(change$chg_new_hu_excl_zon), .01), 2),
    chg_pct_nhb = mean(change$chg_pct_nhb),
    chg_pct_hsp = mean(change$chg_pct_hsp),
    chg_pct_nhapi = mean(change$chg_pct_nhapi),
    chg_pct_nhoth = mean(change$chg_pct_nhoth),
    chg_pct_unemp = mean(change$chg_pct_unemp),
    chg_pov_rat = mean(change$chg_pov_rat),
    chg_med_hh_inc = mean(change$chg_med_hh_inc),
    chg_med_gross_rent = mean(change$chg_med_gross_rent),
    chg_pct_vac_hu = mean(change$chg_pct_vac_hu)
  )
  
  pred_grid_aff <- expand_grid(
    chg_conc_wht_aff = round(seq(min(change$chg_conc_wht_aff), max(change$chg_conc_wht_aff), .01), 2),
    chg_new_hu_excl_zon = round(seq(min(change$chg_new_hu_excl_zon), max(change$chg_new_hu_excl_zon), .01), 2),
    chg_pct_nhb = mean(change$chg_pct_nhb),
    chg_pct_hsp = mean(change$chg_pct_hsp),
    chg_pct_nhapi = mean(change$chg_pct_nhapi),
    chg_pct_nhoth = mean(change$chg_pct_nhoth),
    chg_pct_unemp = mean(change$chg_pct_unemp),
    chg_pov_rat = mean(change$chg_pov_rat),
    chg_med_hh_inc = mean(change$chg_med_hh_inc),
    chg_med_gross_rent = mean(change$chg_med_gross_rent),
    chg_pct_vac_hu = mean(change$chg_pct_vac_hu)
  )
  
  pred_grid_pov$xb <- predict(full_wls_pov, newdata = pred_grid_pov, se.fit = T)$fit
  pred_grid_pov$se <- predict(full_wls_pov, newdata = pred_grid_pov, se.fit = T)$se.fit
  
  pred_grid_blk_pov$xb <- predict(full_wls_blk_pov, newdata = pred_grid_blk_pov, se.fit = T)$fit
  pred_grid_blk_pov$se <- predict(full_wls_blk_pov, newdata = pred_grid_blk_pov, se.fit = T)$se.fit
  
  pred_grid_lat_pov$xb <- predict(full_wls_lat_pov, newdata = pred_grid_lat_pov, se.fit = T)$fit
  pred_grid_lat_pov$se <- predict(full_wls_lat_pov, newdata = pred_grid_lat_pov, se.fit = T)$se.fit
  
  pred_grid_wht_pov$xb <- predict(full_wls_wht_pov, newdata = pred_grid_wht_pov, se.fit = T)$fit
  pred_grid_wht_pov$se <- predict(full_wls_wht_pov, newdata = pred_grid_wht_pov, se.fit = T)$se.fit
  
  pred_grid_aff$xb <- predict(full_wls_aff, newdata = pred_grid_aff, se.fit = T)$fit
  pred_grid_aff$se <- predict(full_wls_aff, newdata = pred_grid_aff, se.fit = T)$se.fit
  
  ggplot(pred_grid_pov, aes(x = chg_new_hu_excl_zon, y = xb, 
                            ymin = xb - 1.96 * se, ymax = xb + 1.96 * se)) +
    geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
    geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
    geom_ribbon(alpha = .25) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
    labs(x = "\nChange in Exclusionary Share of New HU",
         y = "Predicted Change in Poverty Concentration\n") + 
    ggsave(filename = paste0("./output/pred/full_wls_pov_pred_", tract_thresh, ".png"),
           width = 6, height = 4, dpi = 300)
  
  ggplot(pred_grid_blk_pov, aes(x = chg_new_hu_excl_zon, y = xb, 
                            ymin = xb - 1.96 * se, ymax = xb + 1.96 * se)) +
    geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
    geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
    geom_ribbon(alpha = .25) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
    labs(x = "\nChange in Exclusionary Share of New HU",
         y = "Predicted Change in Black Poverty Concentration\n") + 
    ggsave(filename = paste0("./output/pred/full_wls_blk_pov_pred_", tract_thresh, ".png"),
           width = 6, height = 4, dpi = 300)
  
  ggplot(pred_grid_lat_pov, aes(x = chg_new_hu_excl_zon, y = xb, 
                                ymin = xb - 1.96 * se, ymax = xb + 1.96 * se)) +
    geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
    geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
    geom_ribbon(alpha = .25) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
    labs(x = "\nChange in Exclusionary Share of New HU",
         y = "Predicted Change in Latino Poverty Concentration\n") + 
    ggsave(filename = paste0("./output/pred/full_wls_lat_pov_pred_", tract_thresh, ".png"),
           width = 6, height = 4, dpi = 300)
  
  ggplot(pred_grid_wht_pov, aes(x = chg_new_hu_excl_zon, y = xb, 
                                ymin = xb - 1.96 * se, ymax = xb + 1.96 * se)) +
    geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
    geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
    geom_ribbon(alpha = .25) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
    labs(x = "\nChange in Exclusionary Share of New HU",
         y = "Predicted Change in White Poverty Concentration\n") + 
    ggsave(filename = paste0("./output/pred/full_wls_wht_pov_pred_", tract_thresh, ".png"),
           width = 6, height = 4, dpi = 300)
  
  ggplot(pred_grid_aff, aes(x = chg_new_hu_excl_zon, y = xb, 
                            ymin = xb - 1.96 * se, ymax = xb + 1.96 * se)) +
    geom_vline(xintercept = 0, color = "grey60", linetype = 3) + 
    geom_hline(yintercept = 0, color = "grey60", linetype = 3) +
    geom_ribbon(alpha = .25) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(plot.margin = unit(c(.25, .25, .25, .25), "in")) +
    labs(x = "\nChange in Exclusionary Share of New HU",
         y = "Predicted Change in Affluence Concentration\n") + 
    ggsave(filename = paste0("./output/pred/full_wls_aff_pred_", tract_thresh, ".png"),
           width = 6, height = 4, dpi = 300)
  
  write_dta(change, paste0("./input/data_for_stata_", tract_thresh, ".dta"))
  
}