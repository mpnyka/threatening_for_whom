## Load necessary libraries

library(countrycode)
library(tidyverse)
library(broom)
library(lme4)
library(lmerTest)
library(sjPlot)
library(ggeffects)
library(interplot)
library(gridExtra)
library(broom.mixed)
library(texreg)

## Set working directory
setwd("~/Desktop/Sociology/Dissertation/Analysis/paper_3")

## Function to demean the variables:

scale_custom <- function(x){(x - mean(x, na.rm = T))/sd(x, na.rm = T)}

## read in data (for data cleaning script: TFW_ESR_cleaningfile.R)

d <- read_csv("~/Desktop/Sociology/Dissertation/Analysis/paper_3/data/full_dataset2008.csv") %>%
  filter(foreign_born == 0) %>% ## filter to only natives
  drop_na(pct_fborn, log_gdp_nuts, expenditure, unemployment_rate) %>% ## drop_nas of variables
  drop_na(starts_with("n_")) %>%
  mutate(country_name = ifelse(country == "CY", "Cyprus", country_name)) 

##Sanity check: how many NAs am i dropping?

d %>% 
  group_by(country) %>% 
  summarize(across(where(is.numeric), ~mean(is.na(.)))) %>% ## get the % of NAs for each column
  dplyr::select(country, starts_with("n_"), age, female, education, employed, religious, concern_fellows,
                welfare_dependent, national_pride, welfare_chauvinism,
                role_govt, principle_equality, lr, income) %>%
  mutate(m = pmax(age, female, education, employed, religious, concern_fellows,
                  welfare_dependent, national_pride, welfare_chauvinism,
                  role_govt, principle_equality, income)) %>% ## get the column with the max NAs
  arrange(-m) %>% 
  view() ## look at what % pof the data i would be dropping

## Run the factor analysis to get the ethnic boundary variable
fa <- factanal(~ n_born_country + n_respect_laws + n_ancestry + n_language + n_lived_country, 
               factors = 2,
               data = d, 
               scores = "regression",
               rotation = "varimax")

## include factor analysis results and drop nas for all the variables in the analyses:

d2 <- augment(fa, data = d) %>%
  rename(ethnic_boundary = .fs1) %>%
  drop_na(age, female, education, employed, religious, concern_fellows, welfare_dependent,
          national_pride, welfare_chauvinism, role_govt, principle_equality)  %>%
  group_by(country) %>%
  mutate(mean_ethnic_boundary = mean(ethnic_boundary, na.rm = T),
         Z_ethnic_boundary = (ethnic_boundary - mean_ethnic_boundary)/sd(ethnic_boundary, na.rm = T),
         Z_welfare_chauvinism = scale_custom(welfare_chauvinism)
  ) %>%
  ungroup() 


## Replication models

rep_simple <- lmer(role_govt ~ pct_fborn_gc 
                   + (1 | country_name) + (1 | region), 
                   data = d2)
summary(rep_simple)

rep_full <- lmer(role_govt ~ pct_fborn_gc + Z_ethnic_boundary + age + female  
                 +  as.factor(education) + employed  + religious  + welfare_type
                 + concern_fellows + welfare_dependent + Z_welfare_chauvinism + national_pride
                 + log_gdp_gc + unemployment_gc + pct_native_tertiary_gc 
                 + expenditure + pct_fborn_country + pct_native_tertiary_country + unemployment_rate_country + log_gdp_country
                 + mean_ethnic_boundary 
                 + (1|country_name) + (1 | region), 
                 data = d2)

summary(rep_full)

rep_full_beta <- lmer(role_govt ~ pct_fborn_gc + Z_ethnic_boundary + age + female  
                      +  as.factor(education) + employed  + religious  + welfare_type
                      + concern_fellows + welfare_dependent + Z_welfare_chauvinism + national_pride
                      + log_gdp_gc + unemployment_gc + pct_native_tertiary_gc 
                      + expenditure + pct_fborn_country + pct_native_tertiary_country + unemployment_rate_country + log_gdp_country
                      + mean_ethnic_boundary 
                      + (Z_ethnic_boundary|country_name) + (1 | region), 
                      data = d2)

summary(rep_full_beta)

## H1 models

h1_simple_beta <- lmer(role_govt ~ pct_fborn_gc * Z_ethnic_boundary  + mean_ethnic_boundary + mean_pct_fborn
                       + (Z_ethnic_boundary|country_name) + (1 | region), 
                       data = d2)

summary(h1_simple_beta)

h1_simple_beta_extra <- lmer(role_govt ~ pct_fborn_gc * Z_ethnic_boundary + mean_ethnic_boundary + mean_pct_fborn*Z_ethnic_boundary 
                             + (Z_ethnic_boundary|country_name) + (1 | region), 
                             data = d2)

summary(h1_simple_beta_extra)

h1_contextual <- update(h1_simple_beta_extra,  ~ . + log_gdp_gc + unemployment_gc + pct_native_tertiary_gc + welfare_type 
                        + expenditure + pct_native_tertiary_country + unemployment_rate_country + log_gdp_country
)

summary(h1_contextual)

h1_full <- update(h1_contextual,  ~ .  + age + female + 
                    as.factor(education)  + employed  + religious  
                  + concern_fellows + welfare_dependent + Z_welfare_chauvinism + national_pride)


summary(h1_full)

h1_full_income_lr <- update(h1_full,  ~ .  + as.factor(income) + lr)

ss <- getME(h1_full_income_lr, c("theta","fixef"))
h1_full_income_lr <- update(h1_full_income_lr, start = ss, control = lmerControl(optCtrl = list(maxfun = 2e4)))

summary(h1_full_income_lr)

## H2 models

h2_simple_extra <- lmer(role_govt ~ non_eu_prop_gc * pct_fborn_gc * Z_ethnic_boundary + mean_ethnic_boundary +
                          mean_pct_fborn*mean_non_eu_prop*Z_ethnic_boundary 
                        + (Z_ethnic_boundary|country_name) + (1 | region), 
                        data = d2)

summary(h2_simple_extra)

h2_contextual <- update(h2_simple_extra, ~ . + log_gdp_gc + unemployment_gc + pct_native_tertiary_gc + welfare_type 
                        + expenditure  + pct_native_tertiary_country + unemployment_rate_country + log_gdp_country + pct_fborn_primary + 
                          pct_fborn_primary_country)

summary(h2_contextual)

h2_full <- update(h2_contextual, ~ . + age + female + 
                    as.factor(education) + employed + religious  
                  + concern_fellows + welfare_dependent + Z_welfare_chauvinism + national_pride)

summary(h2_full)

h2_full_income_lr <- update(h2_full,  ~ .  + as.factor(income) + lr)

summary(h2_full_income_lr)

## Figures and tables:

## Plot the interactions' marginal effects:

ploth1 <- interplot(m = h1_full, var1 = "pct_fborn_gc", var2 = "Z_ethnic_boundary", hist = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_minimal() +
  labs(x = "Ethnic boundary", y = "Marginal effect on the foreign-born coefficient") 


ploth2A <- interplot(m = h2_full, var1 = "non_eu_prop_gc", var2 = "Z_ethnic_boundary", hist = TRUE) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") + 
  theme_minimal() +
  labs(x = "Ethnic boundary", y = "Marginal effect on the non-EU prop coefficient") 

ploth2B <- plot_model(h2_full, type = "pred", terms = c("non_eu_prop_gc", "Z_ethnic_boundary[-1, 1]"),
                      ci.lvl = 0.5) +
  theme_minimal() + aes(linetype = group, color = group) +
  scale_fill_manual(values = c("firebrick", "goldenrod"),
                    labels = c("Low ethnic nationalism", "High ethnic nationalism")) +
  scale_colour_manual(values = c("firebrick", "goldenrod"),
                      labels = c("Low ethnic nationalism", "High ethnic nationalism"),
                      name = "ethnic boundary") +
  scale_linetype_manual(values = c("twodash", "solid"),
                        labels = c("Low ethnic nationalism", "High ethnic nationalism"),
                        name = "ethnic boundary") +
  labs(y = "Support for welfare", x = "Prop. of non-EU foreign-born", title = "")

## print plots together

grid.arrange(ploth2A, ploth2B, ncol = 2)

## Create the coefficient plots

m1 <- tidy(rep_simple) %>% 
  mutate(type = "simple")

out1 <- tidy(rep_full) %>% 
  mutate(type = "controls") %>%
  bind_rows(m1) %>%
  dplyr::select(term, estimate, std.error, p.value, type) %>%
  mutate(model = "Replication") %>%
  filter(term %in% c("pct_fborn_gc"))

m2 <- tidy(h1_simple_beta_extra) %>% mutate(type = "simple")

out2 <- tidy(h1_full) %>% 
  mutate(type = "controls") %>%
  bind_rows(m2) %>%
  dplyr::select(term, estimate, std.error, p.value, type) %>%
  mutate(model = "H1") %>%
  filter(term %in% c("pct_fborn_gc", "Z_ethnic_boundary", "pct_fborn_gc:Z_ethnic_boundary"))

m3 <- tidy(h2_simple_extra) %>% 
  mutate(type = "simple")

out3 <- tidy(h2_full) %>% 
  mutate(type = "controls"
  ) %>%
  bind_rows(m3) %>%
  dplyr::select(term, estimate, std.error, p.value, type) %>%
  mutate(model = "H2") %>%
  filter(term %in% c("non_eu_prop_gc","Z_ethnic_boundary", "non_eu_prop_gc:Z_ethnic_boundary", "pct_fborn_gc:Z_ethnic_boundary",
                     "pct_fborn_gc"))

final_figure <- out1 %>% 
  bind_rows(out2) %>% bind_rows(out3) %>%
  mutate(model = factor(model, levels = c("Replication", "H1", "H2")),
         term = factor(term, levels = c("pct_fborn_gc","non_eu_prop_gc",
                                        "Z_ethnic_boundary",  
                                        "pct_fborn_gc:Z_ethnic_boundary", "non_eu_prop_gc:Z_ethnic_boundary"),
                       labels = c("foreign born", "prop. foreign born non EU", 
                                  "ethnic boundary", "foreign born : ethnic boundary",
                                  "prop.   born non EU : ethnic boundary")),
         significance = ifelse(p.value < 0.05, "yes", "no"))

final_figure %>%
  ggplot(aes(x = term, y = estimate, 
             ymin = estimate - std.error * 1.96, 
             ymax = estimate + std.error * 1.96,
             color = type, alpha = significance)) +
  geom_pointrange(position = position_dodge(width = 0.2), size = 0.5) +
  coord_flip() +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("firebrick", "darkblue"), name = "Model") +
  scale_alpha_manual(values = c(0.4, 1)) +
  guides(alpha = "none") +
  facet_grid(model ~ ., scales = "free_y", space ='free_y') +
  theme(panel.spacing = unit(3, "lines")) %>%
  labs(x = "") +
  theme(strip.background = element_rect(fill = "#ECECEC80", linetype = "blank"),
        panel.background = element_rect(fill = "#ECECEC80", linetype = "blank"),
        panel.border = element_blank(),
        panel.grid.major = element_blank())


## Factor loadings plot:

load <- fa$loadings[,1:2] %>% 
  as.data.frame() %>% 
  rownames_to_column() %>%
  mutate(rowname = case_when(rowname == "n_born_country" ~ "\nto be born \ninthe country",
                             rowname == "n_respect_laws" ~ "to respect \nlaws",
                             rowname == "n_ancestry" ~ "to have ancestry \nin the country\n\n\n",
                             rowname == "n_language" ~ "to speak \nthe language",
                             rowname == "n_lived_country" ~ "to have lived \nin the country",
                             TRUE ~ rowname))

load %>% ggplot (aes(x = Factor1, y = Factor2)) +
  geom_point(size = 0.5 ) +
  geom_text(aes(label = rowname), size = 4, nudge_y = 0.0025,
            nudge_x = .008,
            hjust = 0 ) +
  xlim(0, 1) +
  theme_minimal() +
  labs(x = "Factor 1 = 'ethnic boundary'", y = "Factor 2 = 'civic boundary'")


## Maps:

## Maps with the final dataset (excludes regions with NAs):

col1 <- colorRampPalette(c("#EAD9CB", "#E66300"))
col2 <- colorRampPalette(c("#FDF5DC", "#FFBE00"))
col3 <- colorRampPalette(c("#CDD4A1", "#5C7800"))

## Create three immigration maps in similar hues pick 1 color. for all, EU and non-EU 

census_map <- read_csv("data/census.csv") %>%
  filter(geo %in% d2$region_census | str_detect(geo, "NO|LT")) %>% ## necessary recoding to match the eurostat_geospatial dataset
  mutate(geo = case_when(geo == "SI01" ~ "SI03",
                         geo == "SI02" ~ "SI04",
                         TRUE ~ geo))

colors <- tibble(imm1 = c("#ECECEC80", col1(9)),
                 imm2 = c("#ECECEC80", col2(9)),
                 imm3 = c("#ECECEC80", col3(9)))

imm_map_ukm <- get_eurostat_geospatial(nuts = 2, year = 2016) %>% 
  filter(geo %in% c("UKM8", "UKM7", "UKM9"))

imm_dat <- get_eurostat_geospatial(nuts = 2, year = 2013) %>% 
  bind_rows(imm_map_ukm) %>%
  filter(!str_detect(geo, "TR|IS|ME|MK")) %>%
  mutate(geo = case_when(geo == "UKI3" | geo == "UKI4" ~ "UKI1",
                         geo == "UKI5" | geo == "UKI6" | geo == "UKI7" ~ "UKI2",
                         geo == "DEC0" ~ "DEC",
                         TRUE ~ geo)) %>%
  left_join(census_map) %>%
  filter(!str_detect(geo, "FRA|PT30|PT20|ES70"))

imm_map <- imm_dat %>%
  mutate(cat = cut_to_classes(pct_fborn, n = 9, decimals = 3)) %>% 
  ggplot(aes(fill = cat)) + 
  scale_fill_manual(values = colors$imm2, 
                    labels = c("No data", "0 - 0.05", "0.05 - 0.09", "0.09 - 0.14", "0.14 - 0.18", 
                               "0.18 - 0.22", "0.22 - 0.27", "0.27 - 0.31", "0.31 - 0.36", "0.36 - 0.4")) +
  geom_sf(color = "white", alpha = .9, lwd = 0.2) +
  coord_sf(crs = 3035) + 
  guides(fill = guide_legend(reverse = T, title = "Proportion")) +
  labs(title = "Percent foreign born, by NUTS2 regions",
       caption = "Data source: Eurostat census (2011)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) 

imm_map1 <- imm_dat %>%
  mutate(cat = cut_to_classes(non_eu_prop, n = 9, decimals = 2)) %>% 
  ggplot(aes(fill = cat)) + 
  scale_fill_manual(values = colors$imm3, 
                    labels = c("No data", "0 - 0.10", "0.10 - 0.21", "0.21 - 0.31", "0.31 - 0.42", 
                               "0.42 - 0.52", "0.52 - 0.63", "0.63 - 0.73", "0.73 - 0.83", "0.83 - 0.94")) +
  geom_sf(color = "white", alpha = .9, lwd = 0.2) +
  coord_sf(crs = 3035) + 
  guides(fill = guide_legend(reverse = T, title = "Proportion")) +
  labs(title = "Proportion non-EU born among foreign born populations, by NUTS2 regions",
       caption = "Data source: Eurostat census (2011)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) 

imm_map2 <- imm_dat %>%
  mutate(cat = cut_to_classes(pct_non_eu_fborn, n = 9, decimals = 3)) %>% 
  ggplot(aes(fill = cat)) + 
  scale_fill_manual(values = colors$imm2, 
                    labels = c("No data", "0 - 0.03", "0.03 - 0.05", "0.05 - 0.08", "0.08 - 0.1", 
                               "0.1 - 0.13", "0.13 - 0.15", "0.15 - 0.18", "0.18 - 0.21", "0.21 - 0.23")) +
  geom_sf(color = "white", alpha = .9, lwd = 0.2) +
  coord_sf(crs = 3035) + 
  guides(fill = guide_legend(reverse = T, title = "Proportion")) +
  labs(title = "Percent foreign born (from non-EU countries), by NUTS2 regions",
       caption = "Data source: Eurostat census (2011)") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.ticks = element_blank()) 

## Print Latex tables:

sink('latex-tables.txt') ## code will be printed in this .txt file

texreg(list(rep_simple, rep_full, rep_full_beta), 
       custom.model.names = c("Simple Model", "Full Model", "Full Model (ethnic boundary random slope)"),
       custom.coef.names = c("Intercept", "Share foreign born (region)", "Ethnic boundary",
                             "Age", "Female", "Education (ISCED 2)", "Education (ISCEd2)", "Employed", "Religious",
                             "Welfare type (Eastern)", "Welfare type (Latin Rim)", "Welfare type (Liberal)", "Welfare type (Social Democracy)",
                             "Concern for fellow citizens", "Welfare dependent", "Anti-Immigrant sentiment", "National pride", 
                             "Log GDP (region)", "Unemployment rate (region)", "Share native population with tertiary education (region)",
                             "Social welfare expenditure (country)",  "Share native population with tertiary education (country)", "Unemployment rate (country)",
                             "Log GDP (country)", "Avg. ethnic boundary (country)", "Avg. share foreign born (country)"))


texreg(list(h1_simple_beta_extra, h1_full, h1_full_income_lr), 
       custom.model.names = c("Simple Model", "Full Model", "Full model with income"),
       custom.coef.names = c("Intercept", 
                             "Share foreign born (region)",
                             "Ethnic boundary",
                             "Avg. ethnic boundary (country)", 
                             "Share foreign born : ethnic boundary",
                             
                             "Avg. share foreign born (country)",
                             "Log GDP (region)", "Unemployment rate (region)", "Share native population with tertiary education (region)",
                             "Welfare type (Eastern)", "Welfare type (Latin Rim)", "Welfare type (Liberal)", "Welfare type (Social Democracy)",
                             
                             "Social welfare expenditure (country)",  "Share native population with tertiary education (country)", 
                             "Unemployment rate (country)",
                             "Log GDP (country)", "Age", "Female", "Education (ISCED 2)", "Education (ISCEd2)",  "Employed", "Religious",
                             "Concern for fellow citizens", "Welfare dependent", "Anti-Immigrant sentiment", "National pride",
                             "Avg. share foreign born (country): ethnic boundary", 
                             "Income (2nd Quartile)", "Income (3rd Quartile)", "Income (4th Quartile)", "Income (5th Quartile)", 
                             "Income (6th Quartile)", "Income (7th Quartile)", "Income (8th Quartile)", "Income (9th Quartile)", 
                             "Income (10th Quartile)", "Income (11th Quartile)", "Income (12th Quartile)"))



texreg(list(h2_simple_extra, h2_full, h2_full_income_lr), 
       custom.model.names = c("Simple Model", "Full Model", "Full model with income"),
       custom.coef.names = c("Intercept", "Prop. non-EU born (region)", "Share foreign born (region)", "Ethnic boundary",
                             
                             "Avg. ethnic boundary (country)",  "Avg. share foreign born (country)", "Avg. non-EU born (country)",
                             "Share foreign born : Prop. non-EU born", "Prop. non-EU born : ethnic boundary",  "Share foreign born : ethnic boundary",
                             "Avg. share foreign born  : Avg. prop. non-EU born", "Avg. share foreign born  : ethnic boundary",
                             "Avg. prop. non-EU born  : ethnic boundary", 
                             "Prop. non-EU born : Share foreign born share : ethnic boundary",
                             "Avg. prop. non-EU born  : Avg. share foreign born share : ethnic boundary",
                             
                             "Log GDP (region)", "Unemployment rate (region)", "Share native population with tertiary education (region)",
                             "Welfare type (Eastern)", "Welfare type (Latin Rim)", "Welfare type (Liberal)", "Welfare type (Social Democracy)",
                             
                             "Social welfare expenditure (country)",  "Share native population with tertiary education (country)", 
                             "Unemployment rate (country)",
                             "Log GDP (country)", "Share foreign born w. primary ed (region)", "Share foreign born w. primary ed (country)", 
                             "Age", "Female", "Education (ISCED 2)", "Education (ISCEd2)",  "Employed", "Religious",
                             "Concern for fellow citizens", "Welfare dependent", "Anti-Immigrant sentiment", "National pride",
                             "Income (2nd Quartile)", "Income (3rd Quartile)", "Income (4th Quartile)", "Income (5th Quartile)", 
                             "Income (6th Quartile)", "Income (7th Quartile)", "Income (8th Quartile)", "Income (9th Quartile)", 
                             "Income (10th Quartile)", "Income (11th Quartile)", "Income (12th Quartile)"))


sink()



