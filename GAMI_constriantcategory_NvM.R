# Script for GAMI paper (Theokritoff et al., forthcoming)

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

GAMI <- read_excel('.../GAMI_Results.xlsx') %>%
  select(countrycode, limits) %>%
  #mutate(limits = 100-limits) %>%
  mutate(class = ifelse(limits <= 60, "1",
                        ifelse(limits >60 & limits <80, "2", "3"))) 

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

# Education - primary education (Wittgenstein Center)

postsec <- read.csv('.../education_historical_WC.csv') %>%
  select(countrycode, year, post.secondary) %>%
  dplyr::rename(postsec = post.secondary) %>%
  filter(year %in% 2003:2013) %>%
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(postsec = mean(postsec)) %>% 
  ungroup() %>% 
  filter(year == 2008) #%>%

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

# Human Development Index HDI (UNDP)

hdi <- read.csv('.../HDI_UNDP_1990-2019.csv', skip = 5) %>% 
  rename_all(tolower) %>% 
  select(country, x2003:x2013) %>% 
  gather(year, hdi, -country) %>% 
  mutate(countrycode = countrycode(country, 'country.name', 'iso3c')) %>% 
  select(-country) %>% 
  mutate(year = year %>% str_replace("x", "") %>% as.integer()) %>% 
  filter(!year == 0) %>% 
  mutate(hdi = as.numeric(hdi)) %>% 
  dplyr::group_by(countrycode) %>% 
  dplyr::mutate(hdi = mean(hdi)) %>% 
  ungroup() %>% 
  filter(year == 2008) %>%
  select(-year)

# Load regions (World Bank Regions)

regions <- read.csv(".../Regions_WB.csv")
#regions_g <- read_xlsx("/Users/nicole/Desktop/Mechanization/Data/Regions/Regions_WB_global.xlsx")

# Combine all into master 

master <- GAMI %>%
  dplyr::left_join(education, countrycode, by = c('countrycode')) %>% 
  dplyr::left_join(postsec, countrycode, by = c('countrycode')) %>% 
  dplyr::left_join(gdppc, countrycode, by = c('countrycode')) %>% 
  dplyr::left_join(gii, countrycode, by = c('countrycode')) %>%
  dplyr::left_join(governance, countrycode, by=c('countrycode')) %>%
  dplyr::left_join(hdi, countrycode, by=c('countrycode')) %>%
  dplyr::left_join(regions, countrycode, by=c('countrycode')) %>%
  mutate(scenario = "Observed") %>%
  mutate(year = 2008)
#write_xlsx(master,"/Users/nicole/Desktop/GAMI_master_baseline_2008.xlsx")

# Establish median for each socioeconomic variable per class type 

baseline <- master %>%
  dplyr::group_by(class) %>%
  dplyr::mutate(gov_median = median(governance, na.rm = T)) %>%
  dplyr::mutate(edu_median = median(education, na.rm = T)) %>%
  dplyr::mutate(postsec_median = median(postsec, na.rm = T)) %>%
  dplyr::mutate(gdppc_median = median(gdppc, na.rm = T)) %>%
  dplyr::mutate(gii_median = median(gii, na.rm = T)) %>%
  dplyr::mutate(hdi_median = median(hdi, na.rm = T)) %>%
  ungroup()
#write_xlsx(baseline,".../GAMI_baseline_median_socioeconomic.xlsx")

# Load projections data for all socioeconomic variables (governance, education, gdppc, gender inequality, hdi)

Proj_socio <- read.csv('.../Pop_Edu_Gov_Gii_Urb_obs_proj.csv') %>%
  select(countrycode, year, scenario, governance, mys.15, gdppc, gii, postsec) %>%
  dplyr::rename(education = mys.15) %>%
  filter(year %in% 2020:2100) %>%
  dplyr::left_join(regions, countrycode, by=c('countrycode')) 

# Add HDI to Proj_socio

Proj_hdi <- read_excel(".../HDI-SSPs.xlsx") %>%
  mutate(countrycode = countrycode(country, 'country.name', 'iso3c')) %>%
  select(year, countrycode, SSP1, SSP2, SSP3, SSP4, SSP5) %>%
  pivot_longer(
    cols = starts_with("SSP"),
    names_to = "scenario",
    #names_prefix = "SSP",
    values_to = "hdi"
  )

Proj_socio <- Proj_socio %>%
  full_join(Proj_hdi %>% select(hdi, countrycode, year, scenario), by = c('countrycode', 'year', 'scenario')) %>%
  full_join(master %>% select(class, countrycode), by='countrycode')

# Country-level ranking 

Proj_country <- Proj_socio %>%
  filter(year %in% c(2030, 2050, 2070, 2090)) %>%               # filtering 2030, 2050, 2070, 2090
  drop_na("class") %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>%           # filtering SSP1, SSP2 & SSP3
  filter(class == "2") %>%                                      # change according to class of interest
  mutate(gov_rank = case_when((governance >= 0.718) ~ "1",
                              (governance < 0.718 & governance > 0.48) ~ "2", 
                              (governance < 0.48 & governance > 0.42) ~ "3",
                              (governance <= 0.42) ~ "3"), 
         edu_rank = case_when((education >= 10.01) ~ "1",
                              (education < 10.01 & education >= 7.13) ~ "2", 
                              (education < 7.13 & education >= 5.34) ~ "3",
                              (education <= 5.34) ~ "3"),
         postsec_rank = case_when((postsec >= 0.15) ~ "1",
                                  (postsec < 0.15 & postsec >= 0.06) ~ "2", 
                                  (postsec < 0.06 & postsec >= 0.03) ~ "3",
                                  (postsec <= 0.03) ~ "3"),
         gdppc_rank = case_when((gdppc >= 24965) ~ "1",
                                (gdppc < 24965 & gdppc >= 2174) ~ "2", 
                                (gdppc < 2174 & gdppc >= 1545) ~ "3",
                                (gdppc <= 1545) ~ "3"),
         gii_rank = case_when((gii <= 0.08) ~ "1",
                              (gii > 0.08 & gii <= 0.45) ~ "2", 
                              (gii > 0.45 & gii <= 0.56) ~ "3",
                              (gii >= 0.56) ~ "3"),
         hdi_rank = case_when((hdi >= 0.86) ~ "1",
                              (hdi < 0.86 & hdi >= 0.66) ~ "2", 
                              (hdi < 0.66 & hdi >= 0.50) ~ "3",
                              (hdi <= 0.50) ~ "3"))
#write_xlsx(Proj_country,".../Proj_country_MEDIUM_constraints.xlsx")

# Establish median per class type for projections 

Proj_master <- Proj_socio %>% 
  filter(year %in% c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)) %>%
  dplyr::group_by(class, scenario, year) %>%
  dplyr::mutate(gov_median = median(governance, na.rm = T)) %>%
  dplyr::mutate(edu_median = median(education, na.rm = T)) %>%
  dplyr::mutate(postsec_median = median(postsec, na.rm = T)) %>%
  dplyr::mutate(gdppc_median = median(gdppc, na.rm = T)) %>%
  dplyr::mutate(gii_median = median(gii, na.rm = T)) %>%
  dplyr::mutate(hdi_median = median(hdi, na.rm = T)) %>%
  drop_na("class") %>%
  ungroup()

# Cut-off values for the different indicators 

# 1 = low ; 2 = medium ; 3 = high
# Governance            -> 1 = 0.72 ;   2 = 0.48 ;  3 = 0.42
# Education (mys)       -> 1 = 10.01 ;  2 = 7.13 ;  3 = 5.34
# Education (postsec)   -> 1 = 0.15 ;   2 = 0.06 ;  3 = 0.03
# GDP per capita        -> 1 = 24965 ;  2 = 2174 ;  3 = 1545
# Gender inequality     -> 1 = 0.08 ;   2 = 0.45 ;  3 = 0.56
# HDI                   -> 1 = 0.86 ;   2 = 0.66 ;  3 = 0.50

# Rank projections for countries 

Ranked <- Proj_master %>%
  mutate(gov_rank = case_when((gov_median >= 0.718) ~ "1",
                              (gov_median < 0.718 & gov_median > 0.48) ~ "2", 
                              (gov_median < 0.48 & gov_median > 0.42) ~ "3",
                              (gov_median <= 0.42) ~ "3"), 
         edu_rank = case_when((edu_median >= 10.01) ~ "1",
                              (edu_median < 10.01 & edu_median >= 7.13) ~ "2", 
                              (edu_median < 7.13 & edu_median >= 5.34) ~ "3",
                              (edu_median <= 5.34) ~ "3"),
         postsec_rank = case_when((postsec_median >= 0.15) ~ "1",
                              (postsec_median < 0.15 & postsec_median >= 0.06) ~ "2", 
                              (postsec_median < 0.06 & postsec_median >= 0.03) ~ "3",
                              (postsec_median <= 0.03) ~ "3"),
         gdppc_rank = case_when((gdppc_median >= 24965) ~ "1",
                                (gdppc_median < 24965 & gdppc_median >= 2174) ~ "2", 
                                (gdppc_median < 2174 & gdppc_median >= 1545) ~ "3",
                                (gdppc_median <= 1545) ~ "3"),
         gii_rank = case_when((gii_median <= 0.08) ~ "1",
                              (gii_median > 0.08 & gii_median <= 0.45) ~ "2", 
                              (gii_median > 0.45 & gii_median <= 0.56) ~ "3",
                              (gii_median >= 0.56) ~ "3"),
         hdi_rank = case_when((hdi_median >= 0.86) ~ "1",
                                (hdi_median < 0.86 & hdi_median >= 0.66) ~ "2", 
                                (hdi_median < 0.66 & hdi_median >= 0.50) ~ "3",
                                (hdi_median <= 0.50) ~ "3"))
#write_xlsx(Ranked,".../Ranked_all.xlsx")

