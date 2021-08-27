# Loading packages
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(corrplot)
library(haven)

# Read in datasets
raw_pheno_gest <- read_csv(file = here::here("Output/Data/raw_pheno_w_ga.csv")) %>%
  clean_names

BMI_values <- read_dta(file = here::here("Data/Phenotypic_Data/For Ravi - pre BMI.dta")) %>%
  clean_names() %>%
  select(uncchdid, prebmiz) %>%
  unique() %>%
  na.omit()

clock_data <- read_csv(file = here::here("Data/Clock_Data/cebu_long_w_baddetP_kept_in.output.csv")) %>%
  clean_names() %>%
  filter(str_detect(sample_id, "_p")) %>%
  mutate(uncchdid = str_sub(sample_id, 2, nchar(sample_id)-2)) %>%
  mutate(uncchdid = as.double(uncchdid))

full_ses_anthro <- read_csv(file = here::here("Data/Phenotypic_Data/full_ses_anthro.csv")) %>%
  clean_names() %>%
  arrange(uncchdid) %>%
  filter(nsfnumb == 1) %>%
  select(uncchdid, ses_pc1) %>%
  unique()

# Combine all
combined_data <- left_join(raw_pheno_gest, BMI_values, by = c("uncchdid")) %>%
  left_join(clock_data, by = c("uncchdid")) %>%
  left_join(full_ses_anthro, by = c("uncchdid")) %>%
  unique()

# Model building
