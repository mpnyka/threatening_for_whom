library(surveytoolbox)
library(here)
library(tidyverse)

## Table with nuts 2 codes and their corresponding name (EVS uses the names of the NUTS regions instead of the actual NUTS code)

nuts_labels <- read_csv("nuts_labels.csv") %>%
  mutate(label = tolower(label),
         label = str_squish(str_replace(label, "\\([a-zA-Z]{2}\\)", "")),
         label = stringi::stri_trans_general(label, "Latin-ASCII"),
         nuts2 = case_when(nuts2 == "UKM7" ~ "UKM2",
                           nuts2 == "DEC0" ~ "DEC",
                           nuts2 == "SI03" ~ "SI01",
                           nuts2 == "SI04" ~ "SI02",
                           TRUE ~ nuts2)) 

## Original EVS data:

EVS2008 <- read_dta("data/EVS/EVS2008.dta") 

## Create correspondence based on the name:

nuts_evs_correspondence <- EVS2008 %>% 
  extract_vallab("v371b_N2") %>% ## includes the names of the nuts regions as a stata label
  filter(id > 0) %>%
  separate(v371b_N2, into = c("country", "more"), sep = ":") %>%
  separate(more, into = c("nuts1_name", "nuts2_name"), sep = " - | – |- | -") %>%
  filter(country != "UA") %>%
  filter(country != "BA") %>%
  filter(country != "ME") %>%
  filter(country != "RU") %>%
  filter(country != "RS") %>%
  filter(country != "MD") %>%
  filter(country != "GE") %>%
  mutate(nuts2_name = tolower(nuts2_name),
         nuts2_name = stringi::stri_trans_general(nuts2_name, "Latin-ASCII")) %>%
  left_join(nuts_labels, by = c("nuts2_name" = "label") ) %>% 
  ## manually recode those that aren't captured here:
  mutate(nuts2 = case_when(nuts2_name == "region de bruxelles-capitale/brussels hoofdstedelijk" ~ "BE10", 
                           nuts2_name == "kypros / kibris" ~ "CY00",
                           nuts2_name == "centre" ~ "FRB0",
                           nuts2_name == "pays de la loire" ~ "FRG0",
                           nuts2_name == "brandenburg-sudwest" ~ "DE40",
                           nuts2_name == "lietuva" ~ "LT00",
                           nuts2_name == "luxembourg (grand-duche)" ~ "LU00",
                           nuts2_name == "comunidad valenciana" ~ "ES52",
                           nuts2_name == "ciudad autonoma de ceuta" ~ "ES63",
                           nuts2_name == "lisboa" ~ "PT17",
                           nuts2_name == "ita-suomi" ~ "FI1D",
                           nuts2_name == "pohjois-suomi" ~ "FI1D",
                           nuts2_name == "sjeverozapadna hrvatska" ~ "HR04",
                           nuts2_name == "sredisnja i istocna (panonska) hrvatska" ~ "HR04",
                           nuts2_name == "border, midland and western" ~ "IE01",
                           nuts2_name == "southern and eastern" ~ "IE02",
                           nuts2_name == "inner london" ~ "UKI1",
                           nuts2_name == "outer london" ~ "UKI2",
                           nuts2_name == "south western scotland" ~ "UKM3",
                           nuts2_name == "mazowieckie" ~ "PL12",
                           nuts2_name == "kozep-magyarorszag" ~ "HU10",
                           nuts2_name == "bucuresti-ilfov" ~ "RO32",
                           nuts2_name == "sud-muntenia" ~ "RO31",
                           TRUE ~ nuts2)) %>%
  select(id, nuts2) %>%
  write_csv("data/nuts_evs_correspondence.csv")



