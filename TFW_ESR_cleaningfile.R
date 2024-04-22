
library(broom)
library(eurostat)
library(tidyverse)
library(haven)
library(janitor)
library(wesanderson)
library(surveytoolbox)
library(lubridate)
library(countrycode)


setwd("~/Desktop/Sociology/Dissertation/Analysis/paper_3")


#### Scripts for gathering the country- and regional- level data from the Eurostat ### -------------------------------------------------------

## UK data for older NUTS2 divisions come from the UK statistics website (locally downloaded)
uk_gdp <- read_csv("data_immigrants/gdp_uk.csv") %>%
  clean_names() %>%
  filter(nuts_level == "NUTS2" | nuts_level == "UK") %>%
  dplyr::select(-nuts_level, -region_name) %>%
  pivot_longer(-nuts_code, names_to = "year_gdp", values_to = "gdp") %>%
  mutate(year_gdp = as.numeric(str_remove(year_gdp, "x")),
         log_gdp = log(gdp),
         year_gdp = ifelse(year_gdp == 20183, 2018, year_gdp)) %>%
  rename(region = nuts_code) %>%
  dplyr::select(-gdp) 

## CH data for older dates come from the CHR statistics website (locally downloaded)

ch_gdp <- read_csv("data/gdp_ch.csv") %>% 
  dplyr::select(-Canton, -`2020p`) %>%
  rename(region = nuts2) %>%
  pivot_longer(-region, names_to = "year_gdp", values_to = "gdp") %>% 
  group_by(region, year_gdp) %>% 
  summarize(gdp = sum(gdp)) %>%
  ungroup() %>%
  mutate(log_gdp = log(gdp),
         year_gdp = as.numeric(year_gdp)) %>%
  dplyr::select(-gdp)


## IE data for older NUTS2 divisions come from the IE statistics website (locally downloaded)

ie_gdp <- read_csv("data/gdp_ie.csv") %>%
  mutate(log_gdp = log(gdp)) %>%
  dplyr::select(-gdp)

## Download GDP data for all the other nuts2 regions and countries from Eurostat (last accessed Jan. 24th, 2023)

gdp <- get_eurostat(id = "nama_10r_2gdp") %>%
  filter(unit == "MIO_EUR") %>%
  mutate(geo = case_when(geo == "HR02" | geo == "HR05" | geo == "HR06" ~ "HR04", 
                         TRUE ~ geo),
         year_gdp = year(time) - 1) %>%
  group_by(geo, year_gdp) %>% 
  summarise(values = sum(values)) %>%
  filter(values != 0 & !str_detect(geo, "CH")) %>%
  mutate(log_gdp = log(values)) %>%
  rename(region = geo) %>% 
  mutate(region = case_when(year_gdp < 2010 & region == "SI03" ~ "SI01", ## recode SI regions to older classification
                            year_gdp < 2010 & region == "SI04" ~ "SI02",
                            TRUE ~ region)) %>%
  dplyr::select(-values) %>% 
  ungroup() %>%
  bind_rows(uk_gdp) %>%
  bind_rows(ch_gdp) %>%
  bind_rows(ie_gdp)

#write_csv(gdp_data, "data/gdp_data.csv")

## Download unemployment data for all the nuts2 regions and countries from Eurostat (last accessed Jan. 24th, 2023)
unemployment_lfs <- get_eurostat(id = "lfst_r_lfur2gac") %>%
  filter(sex == "T" & c_birth == "TOTAL" & age == "Y15-64") %>%
  mutate(year_unemployment = year(time) - 1) %>%
  rename(unemployment_rate = values) %>%
  dplyr::select(geo, contains("unemployment")) 


#write_csv(unemployment, "data/unemployment.csv")

## Download social expenditure data (as share of gdp) for all the countries from Eurostat (last accessed Jan. 24th, 2023)

exp <- get_eurostat(id = "spr_exp_fto") %>%
  filter(spdep == "SPBENEFNOREROUTE" & unit == "PC_GDP") %>%
  filter(str_length(geo) == 2) %>%
  mutate(year = year(time),
         expenditure = values/100) %>%
  dplyr::select(geo, year, expenditure)

#write_csv(exp, "data/exp.csv")


## nuts correspondence tables for older NUTS codes to match with the Census 2011 data

uk_recodes <- read_csv("~/Desktop/Sociology/Dissertation/Analysis/paper_3/evs2017nutsrecodes.csv") ## data extracted from Eurostat where the UK is listed with the new region boundaries
el_recodes <- read_csv("~/Desktop/Sociology/Dissertation/Analysis/paper_3/greece_correspondence.csv") ## From Eurostat pdf sheets about how to proceed about recodings

## Census data (last accessed Jan. 24th, 2023)
census <- get_eurostat("cens_11cobe_r2") %>% ## download census data
  filter(isced97 == "TOTAL" & sex == "T" & age == "TOTAL") %>% ## filter to all education levels, all sex and all age groups
  dplyr::select(geo, c_birth, values) %>%
  bind_rows(recodes17) %>% ## specific values for the new UK regions
  left_join(el_recodes, by = c("geo" = "code11")) %>% ## used to get 2013 codes instead of 2011
  mutate(geo = case_when(str_detect(geo, "EL") & str_length(geo) == 4 ~ code13,
                         TRUE ~ geo)) %>%
  dplyr::select(-nuts_name_greek, -code13) %>%
  filter(!is.na(geo)) %>%
  pivot_wider(names_from = c_birth, values_from = values) %>%
  rename(native_born = NAT,
         total_pop = TOTAL,
         fborn = FOR,
         non_eu_fborn = NEU,
         eu_fborn = EU_OTH) %>%
  mutate(pct_fborn = fborn / total_pop,
         pct_non_eu_fborn = non_eu_fborn / total_pop,
         pct_eu_fborn = eu_fborn / total_pop,
         pct_fborn_sq = pct_fborn^2,
         pct_non_eu_fborn_sq = pct_non_eu_fborn^2,
         pct_eu_fborn_sq = pct_eu_fborn^2,
         country_code = str_extract(geo, "[A-Z]{2}"),
         non_eu_prop = pct_non_eu_fborn/pct_fborn) 

## Census data disaggregated by education levels (last accessed Jan. 24th, 2023)
census_edu <- get_eurostat("cens_11cobe_r2") %>% 
  mutate(edu = case_when(isced97 == "ED1" ~ "primary",
                         isced97 == "ED2" | isced97 == "ED3" | isced97 == "ED4" ~ "secundary",
                         isced97 == "ED5" | isced97 == "ED6" ~ "tertiary",
                         TRUE ~ "other")) %>%
  filter(sex == "T" & age == "TOTAL") %>%
  dplyr::select(geo, c_birth, edu, values) %>%
  left_join(el_recodes, by = c("geo" = "code11")) %>%
  mutate(geo = case_when(str_detect(geo, "EL") & str_length(geo) == 4 ~ code13,
                         TRUE ~ geo)) %>%
  dplyr::select(-nuts_name_greek, -code13) %>%
  filter(!is.na(geo)) %>%
  group_by(geo, c_birth, edu) %>%
  summarize(values = sum(values)) %>%
  pivot_wider(names_from = c(c_birth, edu), values_from = values) %>%
  ungroup() %>%
  mutate(total_pop =  reduce(dplyr::select(., starts_with("TOTAL")), `+`),
         pct_native_tertiary = NAT_tertiary / total_pop,
         pct_fborn_primary = FOR_primary / total_pop,
         pct_fborn_secundary = FOR_secundary / total_pop,
         pct_fborn_tertiary = FOR_tertiary / total_pop,
         pct_non_eu_primary = NEU_primary / total_pop,
         pct_non_eu_secundary = NEU_secundary / total_pop,
         pct_non_eu_tertiary = NEU_tertiary / total_pop,
         pct_eu_fborn_primary = EU_OTH_primary / total_pop,
         pct_eu_fborn_secundary = EU_OTH_secundary / total_pop,
         pct_eu_fborn_tertiary = EU_OTH_tertiary / total_pop
  ) %>%
  dplyr::select(-total_pop)

#write_csv(census, "data/census.csv")

#write_csv(census_edu, "data/census_edu.csv")

## Recoding tables for Poland and France, from the nuts_correspondence table included in the eurostat R package
pl_recodes <- nuts_correspondence %>% 
  filter(str_detect(resolution, "PL")) %>% 
  filter (nuts_level == 2) %>% 
  dplyr::select(code13_pl = code13, code16_pl = code16)

fr_recodes <- nuts_correspondence %>% 
  filter(str_detect(resolution, "FR")) %>% 
  filter (nuts_level == 2) %>% 
  dplyr::select(code13_fr = code13, code16_fr = code16)

## putting all the datasets together:

region_country_data08 <- gdp %>%
  mutate(level = case_when(str_length(region) == 2 ~ "country", ## create variable that tells whether a given geo unit is a country of a nuts level region
                           TRUE ~ "nuts"),
         country = str_extract(region, "[:alpha:]{2}")) %>% ## get the country code
  pivot_wider(names_from = level, values_from = c(region, log_gdp)) %>% ## pivot by level so that each row is a nuts unit and the country-information is in a separate column
  unnest(cols = c(region_country, log_gdp_country), keep_empty = T) %>% 
  unnest(c(region_nuts, log_gdp_nuts)) %>%
  rename(year = year_gdp,
         region = region_nuts) %>%
  dplyr::select(-region_country) %>%
  left_join(exp, by = c("country" = "geo", "year" = "year")) %>% ## join exp tibble by country and year.
  mutate(region_unemployment = case_when(region == "UKM8" | region == "UKM9" ~ "UKM3",
                                         region == "UKM7" ~ "UKM2",
                                         region %in% c("HU11", "HU12") ~ "HU10",
                                         TRUE ~ region)) %>%
  left_join(unemployment_lfs, by = c("region_unemployment" = "geo", "year" = "year_unemployment")) %>% ## join the unemployment tibble twice, one for regions and the other for country
  left_join(unemployment_lfs %>% 
              rename(unemployment_rate_country = unemployment_rate), ## rename so that it doesnt create a duplicate column with above
            by = c("country" = "geo", "year" = "year_unemployment")) %>%
  left_join(fr_recodes, by = c("region" = "code16_fr")) %>% 
  left_join(pl_recodes, by = c("region" = "code16_pl")) %>%
  mutate(region_census = case_when(!is.na(code13_fr) ~ code13_fr, ## because the census uses the old scheme, get correspondences for the newer data - census (same below for PL)
                                   !is.na(code13_pl) ~ code13_pl,
                                   region == "HU11" | region == "HU12" ~ "HU10",
                                   region == "UKI3" | region == "UKI4" ~ "UKI1",
                                   region == "UKI5" | region == "UKI6" | region == "UKI7" ~ "UKI2",
                                   region == "SI03" ~ "SI01",
                                   region == "SI04" ~ "SI02",
                                   country == "LT" ~ "LT00", 
                                   region == "PL9" ~ "PL12", ## PL12 in older classifications became PL9 after 2018 (but current eurostat data does not list PL12 anymore)
                                   TRUE ~ region),
         region_census2 = case_when(region == "UKM8" | region == "UKM9" ~ "UKM3",
                                    region == "UKM7" ~ "UKM2",
                                    TRUE ~ region_census),
         region_evs = case_when(str_detect(region, "FR|LT|PL") & region!= "PL9" ~ region, ## The EVS uses the nuts13 coding for France and Lithuania, otherwise the "region" variable has the right coding
                                region %in% c("HU11", "HU12") ~ region_census,
                                region %in% c("UKM8", "UKM9", "UKM7") ~ region_census2, ## the EVS uses the old coding for Scottland
                                TRUE ~ region_census)
  ) %>%
  left_join(census %>%
              dplyr::select(-country_code), by = c("region_census" = "geo")) %>%
  left_join(census %>%
              dplyr::select(geo, pct_fborn_country = pct_fborn, 
                            pct_eu_fborn_country = pct_eu_fborn,
                            pct_non_eu_fborn_country = pct_non_eu_fborn,
                            non_eu_prop_country = non_eu_prop), 
            by = c("country" = "geo")) %>%
  left_join(census_edu, by = c("region_census2" = "geo")) %>%
  left_join(census_edu %>% 
              dplyr::select(geo, pct_native_tertiary_country = pct_native_tertiary, pct_fborn_primary_country = pct_fborn_primary), 
            by = c("country" = "geo"))

## save local copy:
#write_csv(region_country_data08, "data/region_country_data08.csv")

#### Scripts for cleaning the EVS data and merging it with the Eurostat data ### -------------------------------------------------------

region_country_data08 <- read_csv("data/region_country_data08.csv")
## CSV containing the welfare type by country (from Esping Andersen, adapted by Mau and Burkhardt 2009 + former soviet country category)
welfare_type <- read_csv("welfare_types_country.csv") %>% select(-country_name)

## Loading and cleaning EVS data

EVS2008 <- read_dta("~/Desktop/Sociology/Dissertation/Analysis/EVS/EVS2008.dta") 

## Get country names from the dta file:
cntry08 <- EVS2008 %>% extract_vallab("country") %>% 
  rename(country_name = country,
         country_id = id)

## correspondence table between the EVS nuts2 random coding system and actual nuts2 names/codes. 
## Script to create it can be found in evs_nuts_convergence.R
nuts_evs_correspondence <- read_csv("~/Desktop/Sociology/Dissertation/Analysis/EVS/nuts_evs_correspondence.csv")


## Select relevant variables and do necessary transformations:

EVS_2008 <- EVS2008 %>% 
  mutate_at(vars(v276, v277, v278, v279, v280, v194, v198, v271, age, v302, v336_r, v306,
                 v353YR, v337, v351, v193, v105, v287, v371b_N2, v371b_N3, weight_g, v256),
            function(x) ifelse(x < 0, NA, x)) %>%
  mutate(individualism = case_when(v194 < 4 ~ "Individual Responsibility",
                                   v194 > 3 & v194 < 6 ~ "Neutral",
                                   v194 > 6  ~ "State responsibility",
                                   TRUE ~ NA_character_),
         individualism = fct_relevel(individualism, c("State responsibility", "Neutral", "Individual Responsibility"))) %>%
  rename(n_born_country = v276,
         n_respect_laws = v277,
         n_ancestry = v278,
         n_language = v279,
         n_lived_country = v280,
         role_govt = v194, ## higher more "progressive"
         principle_equality = v198, ## higher less "progressive" fixed later
         welfare_chauvinism = v271, ## higher more "progressive"
         gender = v302,
         lr = v193) %>%
  mutate(principle_equality = abs(principle_equality - 11), ## higher more "progressive"
         education = as.factor(v336_r),
         income = as.factor(v353YR),
         employed = case_when(v337 < 4 ~ 1,
                              v337 > 3 ~ 0, 
                              TRUE ~ NA_real_),
         foreign_born = ifelse(v306 == 2, 1, 0),
         welfare_dependent = ifelse(v351 == 1, 1, 0), 
         religious = ifelse(v105 == 1, 1, 0),
         concern_fellows = abs(v287 - 5),
         national_pride = abs(v256 - 4), ## higher more proud
         female = ifelse(gender == 2, 1, 0)) %>%
  mutate_at(vars(starts_with("n_")), function(x) abs(x - 4)) %>%
  select(year, country, principle_equality, education, income, employed, foreign_born, 
         welfare_dependent, religious, concern_fellows, female, gender, lr, 
         welfare_chauvinism, principle_equality, role_govt,
         starts_with("n_"), age,  national_pride,
         weight_g, studyno, id_cocas, v371b_N2
  ) %>%
  left_join(cntry08, by = c("country" = "country_id")) %>%
  left_join(nuts_evs_correspondence, by = c("v371b_N2" = "id")) %>%
  mutate(country_code = countrycode(country_name, "country.name", "iso2c"),
         country_code = case_when(country_code == "GR" ~ "EL",
                                  country_code == "GB" ~ "UK",
                                  TRUE ~ country_code),
         region = case_when(country_code == "NO" ~"NO0",
                            country_code == "LT" ~ "LT0",
                            TRUE ~ nuts2))

## create demeaning function:
demean <- function(x) x - mean(x, na.rm = T)

full_dataset08 <- region_country_data08 %>% 
  group_by(year, country) %>%
  ## group-meaning the variables
  mutate(pct_fborn_gc = demean(pct_fborn),
         pct_non_eu_fborn_gc = demean(pct_non_eu_fborn),
         non_eu_prop_gc = demean(non_eu_prop),
         pct_eu_fborn_gc = demean(pct_eu_fborn),
         log_gdp_gc = demean(log_gdp_nuts),
         pct_native_tertiary_gc = demean(pct_native_tertiary), 
         unemployment_gc = demean(unemployment_rate),
         mean_pct_fborn = mean(pct_non_eu_fborn, na.rm = T),
         mean_non_eu_prop = mean(non_eu_prop, na.rm = T),
         mean_unemployment = mean(unemployment_rate, na.rm = T),
         mean_native_tertiary_gc = mean(pct_native_tertiary, na.rm = T), 
         log_gdp_mean = mean(log_gdp_nuts, na.rm = T),
         mean_non_eu_fborn = mean(pct_fborn, na.rm = T),
         mean_eu_fborn = mean(pct_eu_fborn, na.rm = T)) %>%
  ungroup() %>%
  inner_join(EVS_2008, by = c("country" = "country_code", "region_evs" = "region", "year" = "year")) %>%
  left_join(welfare_type, by = c("country" = "country_code"))

## Save final dataset (includes missing data)
write_csv(full_dataset08, "data/full_dataset2008.csv")


