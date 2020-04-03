
#### Median Income for Each METRO ---------------------------------------------

## 2008-2012 ACS

#create tables by CBSA summarizing income and rent bin totals
mean_inc_2010 <- acs2010 %>%
  group_by(METRO) %>%
  summarize(meanhhy = sum(QVBE001, na.rm = T)/sum(QU0E001, na.rm = T))

metro_inc_2010 <- acs2010 %>%
  filter(!is.na(METRO)) %>%
  select(GISJOIN, METRO, starts_with("QU0E")) %>%
  gather(key = "bin", value = "count", -GISJOIN, -METRO) %>%
  group_by(METRO, bin) %>%
  summarize(hhs = sum(count)) %>%
  mutate(tothhs = hhs[bin == "QU0E001"],
         min = case_when(
           bin == "QU0E002" ~ 0,
           bin == "QU0E003" ~ 10000,
           bin == "QU0E004" ~ 15000,
           bin == "QU0E005" ~ 20000,
           bin == "QU0E006" ~ 25000,
           bin == "QU0E007" ~ 30000,
           bin == "QU0E008" ~ 35000,
           bin == "QU0E009" ~ 40000,
           bin == "QU0E010" ~ 45000,
           bin == "QU0E011" ~ 50000,
           bin == "QU0E012" ~ 60000,
           bin == "QU0E013" ~ 75000,
           bin == "QU0E014" ~ 100000,
           bin == "QU0E015" ~ 125000,
           bin == "QU0E016" ~ 150000,
           bin == "QU0E017" ~ 200000
         ),
         max = case_when(
           bin == "QU0E002" ~ 10000,
           bin == "QU0E003" ~ 15000,
           bin == "QU0E004" ~ 20000,
           bin == "QU0E005" ~ 25000,
           bin == "QU0E006" ~ 30000,
           bin == "QU0E007" ~ 35000,
           bin == "QU0E008" ~ 40000,
           bin == "QU0E009" ~ 45000,
           bin == "QU0E010" ~ 50000,
           bin == "QU0E011" ~ 60000,
           bin == "QU0E012" ~ 75000,
           bin == "QU0E013" ~ 100000,
           bin == "QU0E014" ~ 125000,
           bin == "QU0E015" ~ 150000,
           bin == "QU0E016" ~ 200000
         )) %>% 
  filter(bin != "QU0E001") %>%
  ungroup() %>% 
  left_join(mean_inc_2010) %>%
  mutate(bin = parse_number(bin)-1) %>%
  select(METRO, bin, meanhhy, tothhs, hhs, min, max)

write_dta(metro_inc_2010, "./output/mcib/mcib_inc_2010.dta")

## 2014-2018 ACS

mean_inc_2016 <- acs2016 %>%
  group_by(METRO) %>%
  summarize(meanhhy = sum(AJZBE001, na.rm = T)/sum(AJY9E001, na.rm = T))

metro_inc_2016 <- acs2016 %>%
  filter(!is.na(METRO)) %>%
  select(GISJOIN, METRO, starts_with("AJY9E")) %>%
  gather(key = "bin", value = "count", -GISJOIN, -METRO) %>%
  group_by(METRO, bin) %>%
  summarize(hhs = sum(count)) %>%
  mutate(tothhs = hhs[bin == "AJY9E001"],
         min = case_when(
           bin == "AJY9E002" ~ 0,
           bin == "AJY9E003" ~ 10000,
           bin == "AJY9E004" ~ 15000,
           bin == "AJY9E005" ~ 20000,
           bin == "AJY9E006" ~ 25000,
           bin == "AJY9E007" ~ 30000,
           bin == "AJY9E008" ~ 35000,
           bin == "AJY9E009" ~ 40000,
           bin == "AJY9E010" ~ 45000,
           bin == "AJY9E011" ~ 50000,
           bin == "AJY9E012" ~ 60000,
           bin == "AJY9E013" ~ 75000,
           bin == "AJY9E014" ~ 100000,
           bin == "AJY9E015" ~ 125000,
           bin == "AJY9E016" ~ 150000,
           bin == "AJY9E017" ~ 200000
         ),
         max = case_when(
           bin == "AJY9E002" ~ 10000,
           bin == "AJY9E003" ~ 15000,
           bin == "AJY9E004" ~ 20000,
           bin == "AJY9E005" ~ 25000,
           bin == "AJY9E006" ~ 30000,
           bin == "AJY9E007" ~ 35000,
           bin == "AJY9E008" ~ 40000,
           bin == "AJY9E009" ~ 45000,
           bin == "AJY9E010" ~ 50000,
           bin == "AJY9E011" ~ 60000,
           bin == "AJY9E012" ~ 75000,
           bin == "AJY9E013" ~ 100000,
           bin == "AJY9E014" ~ 125000,
           bin == "AJY9E015" ~ 150000,
           bin == "AJY9E016" ~ 200000
         )) %>% 
  filter(bin != "AJY9E001") %>%
  ungroup() %>% 
  left_join(mean_inc_2010) %>%
  mutate(bin = parse_number(bin)-1) %>%
  select(METRO, bin, meanhhy, tothhs, hhs, min, max)

write_dta(metro_inc_2016, "./output/mcib/mcib_inc_2016.dta")


#### Median Gross Rent for each METRO -----------------------------------------

## 2008-2012 ACS

#create tables by CBSA summarizing income and rent bin totals
mean_rent_2010 <- acs2010 %>%
  group_by(METRO) %>%
  summarize(meanhhy = sum(QZUE001, na.rm = T)/sum(QZSE002, na.rm = T))

metro_rent_2010 <- acs2010 %>%
  filter(!is.na(METRO)) %>%
  select(GISJOIN, METRO, starts_with("QZSE")) %>%
  gather(key = "bin", value = "count", -GISJOIN, -METRO) %>%
  group_by(METRO, bin) %>%
  summarize(hhs = sum(count)) %>%
  mutate(tothhs = hhs[bin == "QZSE002"],
         min = case_when(
           bin == "QZSE003" ~ 0,
           bin == "QZSE004" ~ 100,
           bin == "QZSE005" ~ 150,
           bin == "QZSE006" ~ 200,
           bin == "QZSE007" ~ 250,
           bin == "QZSE008" ~ 300,
           bin == "QZSE009" ~ 350,
           bin == "QZSE010" ~ 400,
           bin == "QZSE011" ~ 450,
           bin == "QZSE012" ~ 500,
           bin == "QZSE013" ~ 550,
           bin == "QZSE014" ~ 600,
           bin == "QZSE015" ~ 650,
           bin == "QZSE016" ~ 700,
           bin == "QZSE017" ~ 750,
           bin == "QZSE018" ~ 800,
           bin == "QZSE019" ~ 900,
           bin == "QZSE020" ~ 1000,
           bin == "QZSE021" ~ 1250,
           bin == "QZSE022" ~ 1500,
           bin == "QZSE023" ~ 2000
         ),
         max = case_when(
           bin == "QZSE003" ~ 100,
           bin == "QZSE004" ~ 150,
           bin == "QZSE005" ~ 200,
           bin == "QZSE006" ~ 250,
           bin == "QZSE007" ~ 300,
           bin == "QZSE008" ~ 350,
           bin == "QZSE009" ~ 400,
           bin == "QZSE010" ~ 450,
           bin == "QZSE011" ~ 500,
           bin == "QZSE012" ~ 550,
           bin == "QZSE013" ~ 600,
           bin == "QZSE014" ~ 650,
           bin == "QZSE015" ~ 700,
           bin == "QZSE016" ~ 750,
           bin == "QZSE017" ~ 800,
           bin == "QZSE018" ~ 900,
           bin == "QZSE019" ~ 1000,
           bin == "QZSE020" ~ 1250,
           bin == "QZSE021" ~ 1500,
           bin == "QZSE022" ~ 2000
         )) %>% 
  filter(!bin %in% c("QZSE001", "QZSE002", "QZSE024")) %>%
  ungroup() %>% 
  left_join(mean_rent_2010) %>%
  mutate(bin = parse_number(bin)-2) %>%
  select(METRO, bin, meanhhy, tothhs, hhs, min, max)

haven::write_dta(metro_rent_2010, "./output/mcib/mcib_rent_2010.dta")

## 2014-2018 ACS

#create tables by CBSA summarizing income and rent bin totals
mean_rent_2016 <- acs2016 %>%
  group_by(METRO) %>%
  summarize(meanhhy = sum(AJ3FE001, na.rm = T)/sum(AJ3DE002, na.rm = T))

metro_rent_2016 <- acs2016 %>%
  filter(!is.na(METRO)) %>%
  select(GISJOIN, METRO, starts_with("AJ3DE")) %>%
  gather(key = "bin", value = "count", -GISJOIN, -METRO) %>%
  group_by(METRO, bin) %>%
  summarize(hhs = sum(count)) %>%
  mutate(tothhs = hhs[bin == "AJ3DE002"],
         min = case_when(
           bin == "AJ3DE003" ~ 0,
           bin == "AJ3DE004" ~ 100,
           bin == "AJ3DE005" ~ 150,
           bin == "AJ3DE006" ~ 200,
           bin == "AJ3DE007" ~ 250,
           bin == "AJ3DE008" ~ 300,
           bin == "AJ3DE009" ~ 350,
           bin == "AJ3DE010" ~ 400,
           bin == "AJ3DE011" ~ 450,
           bin == "AJ3DE012" ~ 500,
           bin == "AJ3DE013" ~ 550,
           bin == "AJ3DE014" ~ 600,
           bin == "AJ3DE015" ~ 650,
           bin == "AJ3DE016" ~ 700,
           bin == "AJ3DE017" ~ 750,
           bin == "AJ3DE018" ~ 800,
           bin == "AJ3DE019" ~ 900,
           bin == "AJ3DE020" ~ 1000,
           bin == "AJ3DE021" ~ 1250,
           bin == "AJ3DE022" ~ 1500,
           bin == "AJ3DE023" ~ 2000,
           bin == "AJ3DE024" ~ 2500,
           bin == "AJ3DE025" ~ 3000,
           bin == "AJ3DE026" ~ 3500,
         ),
         max = case_when(
           bin == "AJ3DE003" ~ 100,
           bin == "AJ3DE004" ~ 150,
           bin == "AJ3DE005" ~ 200,
           bin == "AJ3DE006" ~ 250,
           bin == "AJ3DE007" ~ 300,
           bin == "AJ3DE008" ~ 350,
           bin == "AJ3DE009" ~ 400,
           bin == "AJ3DE010" ~ 450,
           bin == "AJ3DE011" ~ 500,
           bin == "AJ3DE012" ~ 550,
           bin == "AJ3DE013" ~ 600,
           bin == "AJ3DE014" ~ 650,
           bin == "AJ3DE015" ~ 700,
           bin == "AJ3DE016" ~ 750,
           bin == "AJ3DE017" ~ 800,
           bin == "AJ3DE018" ~ 900,
           bin == "AJ3DE019" ~ 1000,
           bin == "AJ3DE020" ~ 1250,
           bin == "AJ3DE021" ~ 1500,
           bin == "AJ3DE022" ~ 2000,
           bin == "AJ3DE023" ~ 2500,
           bin == "AJ3DE024" ~ 3000,
           bin == "AJ3DE025" ~ 3500,
         )) %>% 
  filter(!bin %in% c("AJ3DE001", "AJ3DE002", "AJ3DE027")) %>%
  ungroup() %>% 
  left_join(mean_rent_2016) %>%
  mutate(bin = parse_number(bin)-2) %>%
  select(METRO, bin, meanhhy, tothhs, hhs, min, max)

write_dta(metro_rent_2016, "./output/mcib/mcib_rent_2016.dta")

#### Run Stata script ---------------------------------------------------------

#run stata script, exit stata on completion
system('"C:\\Program Files\\Stata16\\StataMP-64.exe" /e do H:\\zoning\\mcib.do"')

