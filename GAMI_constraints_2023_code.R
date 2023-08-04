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
library(corrplot)
library(Rcpp)
library(knitr)

# Load GAMI data and categorize constraint group (5 countries removed - outliers)
# 1 = medium constraints, 2 = high constraints, 3 = very high constraints 

GAMI <- read_excel('.../GAMI_rankings-5countries.xlsx') %>%
  select(countrycode, limits) %>%
  #mutate(limits = 100-limits) %>%
  mutate(class = ifelse(limits < 0.60, "1",
                        ifelse(limits >= 0.60 & limits <= 0.80, "2", "3")))

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

# Combine into master 
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

# Group by class
grouped_baseline <- baseline %>%
  dplyr::group_by(class)

# Calculate the median for each variable within each class
summary_df <- grouped_baseline %>%
  dplyr::summarize(
    gov_median = median(gov_median, na.rm = TRUE),
    edu_median = median(edu_median, na.rm = TRUE),
    gdppc_median = median(gdppc_median, na.rm = TRUE),
    gii_median = median(gii_median, na.rm = TRUE)
  )

# 1 = low ; 2 = medium ; 3 = high
# Governance            -> 1 = 0.743 ;   2 = 0.476 ;  3 = 0.427
# Education (mys)       -> 1 = 10.7 ;  2 = 7.17 ;  3 = 4.77
# GDP per capita        -> 1 = 31700 ;  2 = 3172 ;  3 = 1033
# Gender inequality     -> 1 = 0.0477 ;   2 = 0.450 ;  3 = 0.568

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

Proj_country <- Proj_socio %>%
  filter(year %in% c(2030, 2050, 2070, 2090)) %>% 
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>% 
  mutate(gov_rank = case_when((governance >= 0.743) ~ "1",
                              (governance < 0.743 & governance >= 0.476) ~ "2", 
                              (governance < 0.476 & governance >= 0.427) ~ "3",
                              (governance < 0.427) ~ "3"), 
         edu_rank = case_when((education >= 10.7) ~ "1",
                              (education < 10.7 & education >= 7.17) ~ "2", 
                              (education < 7.17 & education >= 4.77) ~ "3",
                              (education < 4.77) ~ "3"),
         gdppc_rank = case_when((gdppc >= 31700) ~ "1",
                                (gdppc < 31700 & gdppc >= 3172) ~ "2", 
                                (gdppc < 3172 & gdppc >= 1033) ~ "3",
                                (gdppc < 1033) ~ "3"),
         gii_rank = case_when((gii <= 0.0477) ~ "1",
                              (gii > 0.0477 & gii >= 0.450) ~ "2", 
                              (gii < 0.450 & gii >= 0.568) ~ "3",
                              (gii < 0.568) ~ "3")) 

# Group by class and count the number of distinct country codes for each class
num_countrycodes_by_class <- Proj_country %>%
  group_by(class) %>%
  summarize(num_countrycodes = n_distinct(countrycode))
print(num_countrycodes_by_class)

Proj_country <- Proj_country %>%
  semi_join(GAMI, by = "countrycode") %>%
  filter(!is.na(class))

# Very high (filter '3')

Proj_veryhigh <- Proj_country %>%
  filter(class == '3') 

Proj_veryhigh <- Proj_veryhigh %>%
  mutate(
    countif_1 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "1", na.rm = TRUE),
    countif_2 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "2", na.rm = TRUE),
    countif_3 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "3", na.rm = TRUE)
  ) %>%
  mutate(
    countif_1 = replace_na(countif_1, 0),
    countif_2 = replace_na(countif_2, 0),
    countif_3 = replace_na(countif_3, 0)
  )

Proj_veryhigh <- Proj_veryhigh %>%
  mutate(
    medium = ifelse(countif_1 >= 3, "medium", NA_character_),
    high = ifelse(
      (countif_1 == 0 & countif_2 == 2 & countif_3 == 1) |
        (countif_1 == 1 & countif_2 == 1 & countif_3 == 1) |
        (countif_2 == 2 & countif_1 == 2) |
        (countif_2 == 1 & countif_1 == 2) |
        (countif_2 == 2 & countif_1 == 1) |
        (countif_2 == 3 & countif_1 <= 1) |
        (countif_2 == 3 & countif_1 == 1) |
        (countif_2 == 4),
      "high", NA_character_
    ),
    veryhigh = ifelse(
      countif_3 >= 3 |
        (countif_2 == 2 & countif_3 == 2) |
        (countif_3 == 2),
      "veryhigh", NA_character_
    )
  )

result_veryhigh <- Proj_veryhigh %>%
  filter(year %in% c(2030, 2050, 2070, 2090)) %>%
  group_by(scenario, year) %>%
  summarise(
    veryhigh_countries = sum(veryhigh == "veryhigh", na.rm = TRUE),
    high_countries = sum(high == "high", na.rm = TRUE),
    medium_countries = sum(medium == "medium", na.rm = TRUE),
    .groups = "drop"
  )

# High (filter '2')

Proj_high <- Proj_country %>%
  filter(class == '2') %>%
  mutate(
    countif_1 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "1", na.rm = TRUE),
    countif_2 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "2", na.rm = TRUE),
    countif_3 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "3", na.rm = TRUE)
  ) %>%
  mutate_at(vars(countif_1, countif_2, countif_3), replace_na, 0) %>%
  mutate(
    medium = ifelse(countif_1 >= 3, "medium", NA_character_),
    high = ifelse(
      (countif_2 == 2 & countif_1 == 2) |
        (countif_2 == 1 & countif_1 == 2) |
        (countif_2 == 2 & countif_1 == 1) |
        (countif_2 == 3 & countif_1 <= 1) |
        (countif_2 == 3 & countif_1 == 1) |
        (countif_2 == 4),
      "high", NA_character_
    ),
    veryhigh = ifelse(
      countif_3 >= 3 |
        (countif_2 == 2 & countif_3 == 2) |
        (countif_3 == 2),
      "veryhigh", NA_character_
    )
  )

result_high <- Proj_high %>%
  filter(year %in% c(2030, 2050, 2070, 2090)) %>%
  group_by(scenario, year) %>%
  summarise(
    veryhigh_countries = sum(veryhigh == "veryhigh", na.rm = TRUE),
    high_countries = sum(high == "high", na.rm = TRUE),
    medium_countries = sum(medium == "medium", na.rm = TRUE),
    .groups = "drop"
  )

# Medium (filter '1')

Proj_medium <- Proj_country %>%
  filter(class == '1')

Proj_medium <- Proj_medium %>%
  mutate(
    countif_1 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "1"),
    countif_2 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "2"),
    countif_3 = rowSums(.[, c("gov_rank", "edu_rank", "gdppc_rank", "gii_rank")] == "3")
  )%>%
  mutate(
    countif_1 = replace_na(countif_1, 0),
    countif_2 = replace_na(countif_2, 0),
    countif_3 = replace_na(countif_3, 0)
  )

Proj_medium <- Proj_medium %>%
  mutate(
    medium = ifelse(countif_1 >= 3, "medium", NA_character_),
    high = ifelse(
      (countif_2 == 2 & countif_1 == 2) |
        (countif_2 == 1 & countif_1 == 2) |
        (countif_2 == 2 & countif_1 == 1) |
        (countif_2 == 3 & countif_1 <= 1) |
        (countif_2 == 3 & countif_1 == 1) |
        (countif_2 == 4),
      "high", NA_character_
    ),
    veryhigh = ifelse(
      countif_3 >= 3 |
        (countif_2 == 2 & countif_3 == 2) |
        (countif_3 == 2),
      "veryhigh", NA_character_
    )
  )

result_medium <- Proj_medium %>%
  filter(year %in% c(2030, 2050, 2070, 2090)) %>%
  group_by(scenario, year) %>%
  summarise(
    veryhigh_countries = sum(veryhigh == "veryhigh", na.rm = TRUE),
    high_countries = sum(high == "high", na.rm = TRUE),
    medium_countries = sum(medium == "medium", na.rm = TRUE),
    .groups = "drop"
  )

# Add together result_high and result_veryhigh and sum per year and scenario
combined_result_5 <- rbind(result_high, result_veryhigh, result_medium) %>%
  group_by(scenario, year) %>%
  summarise(
    total_veryhigh_countries = sum(veryhigh_countries, na.rm = TRUE),
    total_high_countries = sum(high_countries, na.rm = TRUE),
    total_medium_countries = sum(medium_countries, na.rm = TRUE),
    .groups = "drop"
  )

#### Timescales for overcoming constraints #### 

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

# Group by class
grouped_baseline <- baseline %>%
  dplyr::group_by(class)

# Calculate the median for each variable within each class
summary_df <- grouped_baseline %>%
  dplyr::summarize(
    gov_median = median(gov_median, na.rm = TRUE),
    edu_median = median(edu_median, na.rm = TRUE),
    gdppc_median = median(gdppc_median, na.rm = TRUE),
    gii_median = median(gii_median, na.rm = TRUE)
  )

Proj_country <- Proj_socio %>%
  filter(year %in% c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)) %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>% 
  mutate(gov_rank = case_when((governance >= 0.743) ~ "1",
                              (governance < 0.743 & governance >= 0.476) ~ "2", 
                              (governance < 0.476 & governance >= 0.427) ~ "3",
                              (governance < 0.427) ~ "3"), 
         edu_rank = case_when((education >= 10.7) ~ "1",
                              (education < 10.7 & education >= 7.17) ~ "2", 
                              (education < 7.17 & education >= 4.77) ~ "3",
                              (education < 4.77) ~ "3"),
         gdppc_rank = case_when((gdppc >= 31700) ~ "1",
                                (gdppc < 31700 & gdppc >= 3172) ~ "2", 
                                (gdppc < 3172 & gdppc >= 1033) ~ "3",
                                (gdppc < 1033) ~ "3"),
         gii_rank = case_when((gii <= 0.0477) ~ "1",
                              (gii > 0.0477 & gii >= 0.450) ~ "2", 
                              (gii < 0.450 & gii >= 0.568) ~ "3",
                              (gii < 0.568) ~ "3")) 

Proj_country_median <- Proj_country %>%
  group_by(class, scenario, year) %>%
  summarise(
    median_governance = mean(governance, na.rm = TRUE),
    median_gdppc = mean(gdppc, na.rm = TRUE),
    median_gii = mean(gii, na.rm = TRUE),
    median_education = mean(education, na.rm = TRUE),
    .groups = "drop"
  )

# Filter data for class 3 only (very high)
class_3_data <- Proj_country_median %>%
  filter(class == '3')

print(class_3_data)

# Add the four rank columns
class_3_data$rank_gov <- 0.743
class_3_data$rank_edu <- 10.7
class_3_data$rank_gdppc <- 31700
class_3_data$rank_gii <- 0.0477

# Add the four indicator columns
class_3_data$gov <- ifelse(class_3_data$median_governance > class_3_data$rank_gov, "yes", "no")
class_3_data$edu <- ifelse(class_3_data$median_education > class_3_data$rank_edu, "yes", "no")
class_3_data$gdppc <- ifelse(class_3_data$median_gdppc > class_3_data$rank_gdppc, "yes", "no")
class_3_data$gii <- ifelse(class_3_data$median_gii < class_3_data$rank_gii, "yes", "no")

# class 2 (high)

# Filter data for class 3 only
class_2_data <- Proj_country_median %>%
  filter(class == '2')

print(class_2_data)

# Add the four rank columns
class_2_data$rank_gov <- 0.743
class_2_data$rank_edu <- 10.7
class_2_data$rank_gdppc <- 31700
class_2_data$rank_gii <- 0.0477

# Add the four indicator columns
class_2_data$gov <- ifelse(class_2_data$median_governance > class_2_data$rank_gov, "yes", "no")
class_2_data$edu <- ifelse(class_2_data$median_education > class_2_data$rank_edu, "yes", "no")
class_2_data$gdppc <- ifelse(class_2_data$median_gdppc > class_2_data$rank_gdppc, "yes", "no")
class_2_data$gii <- ifelse(class_2_data$median_gii < class_2_data$rank_gii, "yes", "no")

print(class_2_data)

### Sensitivity Analysis ####

GAMI <- read_excel('/Users/nicole/Desktop/Climate_Analytics/GAMI_ET/Data/GAMI/GAMI_rankings-5countries.xlsx') %>%
  select(countrycode, limits) %>%
  #mutate(limits = 100-limits) %>%
  mutate(class = ifelse(limits < 0.50, "1",
                        ifelse(limits >= 0.50 & limits <= 0.70, "2", "3")))
print(GAMI)

# Combine into master 
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

# Group by class
grouped_baseline <- baseline %>%
  dplyr::group_by(class)

# Calculate the median for each variable within each class
summary_df <- grouped_baseline %>%
  dplyr::summarize(
    gov_median = median(gov_median, na.rm = TRUE),
    edu_median = median(edu_median, na.rm = TRUE),
    gdppc_median = median(gdppc_median, na.rm = TRUE),
    gii_median = median(gii_median, na.rm = TRUE)
  )

Proj_country <- Proj_socio %>%
  filter(year %in% c(2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)) %>%
  filter(scenario %in% c("SSP1", "SSP2", "SSP3")) %>% 
  mutate(gov_rank = case_when((governance >= 0.845) ~ "1",
                              (governance < 0.845 & governance >= 0.526) ~ "2", 
                              (governance < 0.526 & governance >= 0.450) ~ "3",
                              (governance < 0.450) ~ "3"), 
         edu_rank = case_when((education >= 11.8) ~ "1",
                              (education < 11.8 & education >= 7.87) ~ "2", 
                              (education < 7.87 & education >= 5.83) ~ "3",
                              (education < 5.83) ~ "3"),
         gdppc_rank = case_when((gdppc >= 40239) ~ "1",
                                (gdppc < 40239 & gdppc >= 6749) ~ "2", 
                                (gdppc < 6749 & gdppc >= 1347) ~ "3",
                                (gdppc < 1347) ~ "3"),
         gii_rank = case_when((gii <= 0.0199) ~ "1",
                              (gii > 0.0199 & gii >= 0.381) ~ "2", 
                              (gii < 0.381 & gii >= 0.566) ~ "3",
                              (gii < 0.566) ~ "3"))  

Proj_country_median <- Proj_country %>%
  group_by(class, scenario, year) %>%
  summarise(
    median_governance = mean(governance, na.rm = TRUE),
    median_gdppc = mean(gdppc, na.rm = TRUE),
    median_gii = mean(gii, na.rm = TRUE),
    median_education = mean(education, na.rm = TRUE),
    .groups = "drop"
  )

# class 3 (very high)

# Filter data for class 3 only
class_3_data <- Proj_country_median %>%
  filter(class == '3')

# Update the values for class_3_data based on new gov_median, edu_median, gdppc_median, and gii_median
class_3_data$rank_gov <- 0.845
class_3_data$rank_edu <- 11.8
class_3_data$rank_gdppc <- 40239
class_3_data$rank_gii <- 0.0199

# Add the four indicator columns
class_3_data$gov <- ifelse(class_3_data$median_governance > class_3_data$rank_gov, "yes", "no")
class_3_data$edu <- ifelse(class_3_data$median_education > class_3_data$rank_edu, "yes", "no")
class_3_data$gdppc <- ifelse(class_3_data$median_gdppc > class_3_data$rank_gdppc, "yes", "no")
class_3_data$gii <- ifelse(class_3_data$median_gii < class_3_data$rank_gii, "yes", "no")

# class 2 (high)

# Filter data for class 3 only
class_2_data <- Proj_country_median %>%
  filter(class == '2')

# Add the four rank columns
class_2_data$rank_gov <- 0.845
class_2_data$rank_edu <- 11.8
class_2_data$rank_gdppc <- 40239
class_2_data$rank_gii <- 0.0199

# Add the four indicator columns
class_2_data$gov <- ifelse(class_2_data$median_governance > class_2_data$rank_gov, "yes", "no")
class_2_data$edu <- ifelse(class_2_data$median_education > class_2_data$rank_edu, "yes", "no")
class_2_data$gdppc <- ifelse(class_2_data$median_gdppc > class_2_data$rank_gdppc, "yes", "no")
class_2_data$gii <- ifelse(class_2_data$median_gii < class_2_data$rank_gii, "yes", "no")









