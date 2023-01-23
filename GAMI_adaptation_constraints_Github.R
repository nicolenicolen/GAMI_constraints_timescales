# Script for Theokritoff et al., forthcoming

# Load libraries

library(ggplot2)
library(tidyverse)
library(plm)
library(broom)
library(WDI)
library(countrycode)
library(data.table)
library(readstata13)
library(viridis)
library(ggthemes)
library(sandwich)
library(lmtest)
library(stargazer)
library(dplyr)
library(readxl)
library(gridExtra)
library(betareg)
library(viridis)
library(ggthemes)
library(maps)
library(rworldmap)
library(mapproj)
library(writexl)
library(scales)
library(rlang)
library(car)
library(caret)
library(glm.predict)
library(sjPlot)
library(tidyr)
library(plotly)
library(factoextra)
library(devtools)
library(ggbiplot)
library(corrplot)
library(Rcpp)
library(knitr)

# Load GAMI data and categorize constraint group 
# 1 = low constraints, 2 = medium constraints, 3 = high constraints 

GAMI <- read_excel('/Users/nicole/Desktop/Climate_Analytics/GAMI_ET/Data/GAMI/Results_Emily.xlsx') %>%
  select(countrycode, limits) %>%
  #mutate(limits = 100-limits) %>%
  mutate(class = ifelse(limits < 60, "1",
                        ifelse(limits >= 60 & limits <= 80, "2", "3"))) 

# Load socioeconomic variables for baseline period 2003-2013 (governance, education, gdppc, gender inequality, hdi)

# Governance (Andrijevic et al., 2019)

governance <- read.csv('.../Governance_obs_proj_Andijevic.csv') %>%
  select(-X) %>%
  filter(scenario == "Observed") %>%
  filter(year %in% 2003:2013) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(governance = mean(governance)) %>% 
  ungroup() %>% 
  filter(year == 2008) %>%
  select(-scenario, -year) 

# Education - mean years of schooling (Wittgenstein Center)

education <- read.csv(".../mean-years-of-schooling.csv") %>%
  rename_all(tolower) %>%
  mutate(countrycode = countrycode(entity, 'country.name', 'iso3c')) %>%
  select(-code) %>%
  dplyr::rename(education = "average.total.years.of.schooling.for.adult.population..lee.lee..2016...barro.lee..2018..and.undp..2018..") %>%
  #dplyr::rename(countrycode = "entity") %>%
  filter(year %in% 2003:2013) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(education = mean(education)) %>% 
  ungroup() %>% 
  filter(year == 2008) %>%
  select(-year, -entity) 

# GDP per capita (World Bank, 2022)

gdppc <- read.csv('.../gdp_per_capita_worldbank_1960-2020.csv', skip = 3) %>% 
  rename_all(tolower) %>% 
  select(country.code, x2003:x2013) %>% 
  gather(year, gdppc, -country.code) %>% 
  mutate(year = year %>% str_replace("x", "") %>% as.integer()) %>% 
  dplyr::group_by(country.code) %>% 
  dplyr::mutate(gdppc = mean(gdppc)) %>% 
  ungroup() %>% 
  filter(year == 2008) %>% 
  select(country.code, gdppc) %>% 
  dplyr::rename(countrycode = country.code)

# Gender inequality (Andrijevic et al., 2020)

gii <- read.csv(".../Gender_obs_proj_Andrijevic.csv") %>%
  select(-X) %>%
  filter(scenario == "Observed") %>%
  filter(year %in% 2003:2013) %>%
  select(-scenario) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(gii = mean(gii)) %>% 
  ungroup() %>% 
  filter(year == 2008) %>%
  select(-year)

# Load regions (World Bank Regions)

regions <- read.csv(".../Regions_WB.csv")

# Combine all into master 

master <- GAMI %>%
  dplyr::left_join(education, countrycode, by = c('countrycode')) %>% 
  dplyr::left_join(gdppc, countrycode, by = c('countrycode')) %>% 
  dplyr::left_join(gii, countrycode, by = c('countrycode')) %>%
  dplyr::left_join(governance, countrycode, by=c('countrycode')) %>%
  dplyr::left_join(regions, countrycode, by=c('countrycode')) %>%
  mutate(scenario = "Observed") %>%
  mutate(year = 2008)

# Establish median for each socioeconomic variable per class type 

baseline <- master %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(gov_median = median(governance, na.rm = T)) %>%
  dplyr::mutate(edu_median = median(education, na.rm = T)) %>%
  dplyr::mutate(gdppc_median = median(gdppc, na.rm = T)) %>%
  dplyr::mutate(gii_median = median(gii, na.rm = T)) %>%
  ungroup()

# Load projections data for all socioeconomic variables (governance, education, gdppc, gender inequality, hdi)

mys <- read_xlsx('.../mys_projections.xlsx') %>%
  mutate(countrycode = countrycode(area, 'country.name', 'iso3c')) %>%
  select(-area) %>%
  filter(year %in% 2015:2100)
  
Proj_socio <- read.csv('.../Pop_Edu_Gov_Gii_Urb_obs_proj.csv') %>%
  select(countrycode, year, scenario, governance, gdppc, gii, postsec) %>%
  filter(year %in% 2015:2100) %>%
  dplyr::left_join(regions, countrycode, by=c('countrycode')) %>%
  full_join(master %>% select(class, countrycode), by='countrycode') %>%
  full_join(mys %>% select(education, countrycode, year, scenario), by = c('countrycode', 'year', 'scenario')) %>%
  filter(scenario != "Observed")

#Proj_socio <- Proj_socio %>%
  #full_join(Proj_hdi %>% select(hdi, countrycode, year, scenario), by = c('countrycode', 'year', 'scenario')) %>%
  #full_join(master %>% select(class, countrycode), by='countrycode')

# Country-level ranking 

Proj_country <- Proj_socio %>%
  filter(year %in% c(2030, 2050, 2070, 2090)) %>%               # filtering 2030, 2050, 2070, 2090
  #drop_na("class") %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>%           # filtering SSP1, SSP2 & SSP3
  #filter(class == "3") %>%                                      # change according to class of interest
  mutate(gov_rank = case_when((governance >= 0.697) ~ "1",
                              (governance < 0.697 & governance > 0.478) ~ "2", 
                              (governance < 0.478 & governance > 0.432) ~ "3",
                              (governance <= 0.432) ~ "3"), 
         edu_rank = case_when((education >= 9.763) ~ "1",
                              (education < 9.763 & education >= 7.213) ~ "2", 
                              (education < 7.213 & education >= 5.054) ~ "3",
                              (education <= 5.054) ~ "3"),
         gdppc_rank = case_when((gdppc >= 27078) ~ "1",
                                (gdppc < 27078 & gdppc >= 3203) ~ "2", 
                                (gdppc < 3203 & gdppc >= 1112) ~ "3",
                                (gdppc <= 1112) ~ "3"),
         gii_rank = case_when((gii <= 0.08) ~ "1",
                              (gii > 0.08 & gii <= 0.45) ~ "2", 
                              (gii > 0.45 & gii <= 0.56) ~ "3",
                              (gii >= 0.56) ~ "3"))

# Establish median per class type for projections 

Proj_master <- Proj_socio %>% 
  filter(year %in% c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)) %>%
  dplyr::group_by(class, scenario, year) %>%
  dplyr::mutate(gov_median = median(governance, na.rm = T)) %>%
  dplyr::mutate(edu_median = median(education, na.rm = T)) %>%
  dplyr::mutate(gdppc_median = median(gdppc, na.rm = T)) %>%
  dplyr::mutate(gii_median = median(gii, na.rm = T)) %>%
  drop_na("class") %>%
  ungroup()

# Cut-off values for the different indicators 

# 1 = low ; 2 = medium ; 3 = high
# Governance            -> 1 = 0.697 ;   2 = 0.478 ;  3 = 0.432
# Education (mys)       -> 1 = 9.763 ;  2 = 7.213 ;  3 = 5.054
# GDP per capita        -> 1 = 27078 ;  2 = 3203 ;  3 = 1112
# Gender inequality     -> 1 = 0.08 ;   2 = 0.45 ;  3 = 0.56

# Rank projections for countries 

Ranked <- Proj_master %>%
  filter(class == "2") %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>% 
  mutate(gov_rank = case_when((gov_median >= 0.697) ~ "1",
                              (gov_median < 0.697 & gov_median > 0.478) ~ "2", 
                              (gov_median < 0.478 & gov_median > 0.432) ~ "3",
                              (gov_median <= 0.432) ~ "3"), 
         edu_rank = case_when((edu_median >= 9.763) ~ "1",
                              (edu_median < 9.763 & edu_median >= 7.213) ~ "2", 
                              (edu_median < 7.213 & edu_median >= 5.054) ~ "3",
                              (edu_median <= 5.054) ~ "3"),
         gdppc_rank = case_when((gdppc_median >= 27078) ~ "1",
                                (gdppc_median < 27078 & gdppc_median >= 3203) ~ "2", 
                                (gdppc_median < 3203 & gdppc_median >= 1112) ~ "3",
                                (gdppc_median <= 1112) ~ "3"),
         gii_rank = case_when((gii_median <= 0.08) ~ "1",
                              (gii_median > 0.08 & gii_median <= 0.45) ~ "2", 
                              (gii_median > 0.45 & gii_median <= 0.56) ~ "3",
                              (gii_median >= 0.56) ~ "3"))

Ranked <- Ranked %>%
  filter(year == "2015") %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) 

Baseline_ranking_2008 <- read_xlsx('/Users/nicole/Desktop/Climate_Analytics/GAMI_ET/Data/NEW_2023/Figure_2/Fig2_GAMI_baseline_median_socioeconomic.xlsx') %>%
  mutate(gov_rank = case_when((governance >= 0.697) ~ "1",
                              (governance < 0.697 & governance > 0.478) ~ "2", 
                              (governance < 0.478 & governance > 0.432) ~ "3",
                              (governance <= 0.432) ~ "3"), 
         edu_rank = case_when((education >= 9.763) ~ "1",
                              (education < 9.763 & education >= 7.213) ~ "2", 
                              (education < 7.213 & education >= 5.054) ~ "3",
                              (education <= 5.054) ~ "3"),
         gdppc_rank = case_when((gdppc >= 27078) ~ "1",
                                (gdppc < 27078 & gdppc >= 2174) ~ "2", 
                                (gdppc < 2174 & gdppc >= 1545) ~ "3",
                                (gdppc <= 1545) ~ "3"),
         gii_rank = case_when((gii <= 0.08) ~ "1",
                              (gii > 0.08 & gii <= 0.45) ~ "2", 
                              (gii > 0.45 & gii <= 0.56) ~ "3",
                              (gii >= 0.56) ~ "3"))#,
write_xlsx(Baseline_ranking_2008,"/Users/nicole/Desktop/2008_ranked_all.xlsx")

# 1 = low ; 2 = medium ; 3 = high
# Governance            -> 1 = 0.697 ;   2 = 0.478 ;  3 = 0.432
# Education (mys)       -> 1 = 9.763 ;  2 = 7.213 ;  3 = 5.054
# GDP per capita        -> 1 = 27078 ;  2 = 3203 ;  3 = 1112
# Gender inequality     -> 1 = 0.08 ;   2 = 0.45 ;  3 = 0.56

#### SENSITIVITY Test ####

# Load GAMI data and categorize constraint group 
# 1 = low constraints, 2 = medium constraints, 3 = high constraints 

GAMI <- read_excel('.../Results.xlsx') %>%
  select(countrycode, limits) %>%
  #mutate(limits = 100-limits) %>%
  mutate(class = ifelse(limits < 50, "1",
                        ifelse(limits >= 50 & limits <= 70, "2", "3"))) 

# Combine all into master 

master <- GAMI %>%
  dplyr::left_join(education, countrycode, by = c('countrycode')) %>% 
  dplyr::left_join(gdppc, countrycode, by = c('countrycode')) %>% 
  dplyr::left_join(gii, countrycode, by = c('countrycode')) %>%
  dplyr::left_join(governance, countrycode, by=c('countrycode')) %>%
  dplyr::left_join(regions, countrycode, by=c('countrycode')) %>%
  mutate(scenario = "Observed") %>%
  mutate(year = 2008)

# Establish median for each socioeconomic variable per class type 

baseline <- master %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(gov_median = median(governance, na.rm = T)) %>%
  dplyr::mutate(edu_median = median(education, na.rm = T)) %>%
  dplyr::mutate(gdppc_median = median(gdppc, na.rm = T)) %>%
  dplyr::mutate(gii_median = median(gii, na.rm = T)) %>%
  ungroup()

Proj_socio <- read.csv('.../Pop_Edu_Gov_Gii_Urb_obs_proj.csv') %>%
  select(countrycode, year, scenario, governance, gdppc, gii, postsec) %>%
  filter(year %in% 2015:2100) %>%
  dplyr::left_join(regions, countrycode, by=c('countrycode')) %>%
  full_join(master %>% select(class, countrycode), by='countrycode') %>%
  full_join(mys %>% select(education, countrycode, year, scenario), by = c('countrycode', 'year', 'scenario')) %>%
  filter(scenario != "Observed")

# Cut-off values for the different indicators 

# 1 = low ; 2 = medium ; 3 = high
# Governance            -> 1 = 0.834 ;   2 = 0.516 ;  3 = 0.454
# Education (mys)       -> 1 = 11.509 ;  2 = 7.736 ;  3 = 5.990
# GDP per capita        -> 1 = 31699 ;  2 = 6295 ;  3 = 1604
# Gender inequality     -> 1 = 0.02 ;   2 = 0.40 ;  3 = 0.56

Proj_country <- Proj_socio %>%
  filter(year %in% c(2030, 2050, 2070, 2090)) %>%               # filtering 2030, 2050, 2070, 2090
  drop_na("class") %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>%           # filtering SSP1, SSP2 & SSP3
  filter(class == "2") %>%                                      # change according to class of interest
  mutate(gov_rank = case_when((governance >= 0.834) ~ "1",
                              (governance < 0.834 & governance > 0.516) ~ "2", 
                              (governance < 0.516 & governance > 0.454) ~ "3",
                              (governance <= 0.454) ~ "3"), 
         edu_rank = case_when((education >= 11.509) ~ "1",
                              (education < 11.509 & education >= 7.736) ~ "2", 
                              (education < 7.736 & education >= 5.990) ~ "3",
                              (education <= 5.990) ~ "3"),
         gdppc_rank = case_when((gdppc >= 31699) ~ "1",
                                (gdppc < 31699 & gdppc >= 6295) ~ "2", 
                                (gdppc < 6295 & gdppc >= 1604) ~ "3",
                                (gdppc <= 1604) ~ "3"),
         gii_rank = case_when((gii <= 0.02) ~ "1",
                              (gii > 0.02 & gii <= 0.40) ~ "2", 
                              (gii > 0.40 & gii <= 0.56) ~ "3",
                              (gii >= 0.56) ~ "3"))

# Establish median per class type for projections 

Proj_master <- Proj_socio %>% 
  filter(year %in% c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)) %>%
  dplyr::group_by(class, scenario, year) %>%
  dplyr::mutate(gov_median = median(governance, na.rm = T)) %>%
  dplyr::mutate(edu_median = median(education, na.rm = T)) %>%
  dplyr::mutate(gdppc_median = median(gdppc, na.rm = T)) %>%
  dplyr::mutate(gii_median = median(gii, na.rm = T)) %>%
  drop_na("class") %>%
  ungroup()

Ranked <- Proj_master %>%
  filter(class == "2") %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>% 
  mutate(gov_rank = case_when((gov_median >= 0.834) ~ "1",
                              (gov_median < 0.834 & gov_median > 0.516) ~ "2", 
                              (gov_median < 0.516 & gov_median > 0.454) ~ "3",
                              (gov_median <= 0.454) ~ "3"), 
         edu_rank = case_when((edu_median >= 11.509) ~ "1",
                              (edu_median < 11.509 & edu_median >= 7.736) ~ "2", 
                              (edu_median < 7.736 & edu_median >= 5.990) ~ "3",
                              (edu_median <= 5.990) ~ "3"),
         gdppc_rank = case_when((gdppc_median >= 31699) ~ "1",
                                (gdppc_median < 31699 & gdppc_median >= 6295) ~ "2", 
                                (gdppc_median < 6295 & gdppc_median >= 1604) ~ "3",
                                (gdppc_median <= 1604) ~ "3"),
         gii_rank = case_when((gii_median <= 0.02) ~ "1",
                              (gii_median > 0.02 & gii_median <= 0.40) ~ "2", 
                              (gii_median > 0.40 & gii_median <= 0.56) ~ "3",
                              (gii_median >= 0.56) ~ "3"))

# 1 = low ; 2 = medium ; 3 = high
# Governance            -> 1 = 0.834 ;   2 = 0.516 ;  3 = 0.454
# Education (mys)       -> 1 = 11.509 ;  2 = 7.736 ;  3 = 5.990
# GDP per capita        -> 1 = 31699 ;  2 = 6295 ;  3 = 1604
# Gender inequality     -> 1 = 0.02 ;   2 = 0.40 ;  3 = 0.56

Baseline_ranking_2008 <- read_xlsx('.../Fig2_GAMI_baseline_median_socioeconomic_sensitivity.xlsx') %>%
  mutate(gov_rank = case_when((governance >= 0.834) ~ "1",
                              (governance < 0.834 & governance > 0.516) ~ "2", 
                              (governance < 0.516 & governance > 0.454) ~ "3",
                              (governance <= 0.454) ~ "3"), 
         edu_rank = case_when((education >= 11.509) ~ "1",
                              (education < 11.509 & education >= 7.736) ~ "2", 
                              (education < 7.736 & education >= 5.990) ~ "3",
                              (education <= 5.990) ~ "3"),
         gdppc_rank = case_when((gdppc >= 31699) ~ "1",
                                (gdppc < 31699 & gdppc >= 6295) ~ "2", 
                                (gdppc < 6295 & gdppc >= 1604) ~ "3",
                                (gdppc <= 1604) ~ "3"),
         gii_rank = case_when((gii <= 0.02) ~ "1",
                              (gii > 0.02 & gii <= 0.40) ~ "2", 
                              (gii > 0.40 & gii <= 0.56) ~ "3",
                              (gii >= 0.56) ~ "3"))
