# Loading packages
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(corrplot)
library(haven)

# Read in datasets
raw_pheno_gest <- read_csv(file = here::here("Output/Data/raw_pheno_w_ga.csv")) %>%
  clean_names()

icc_sex_data <- read_csv(file = here::here("Data/Phenotypic_Data/full_nsf_w_covariates.csv")) %>%
  clean_names() %>%
  select(uncchdid, iccsex) %>%
  unique()

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
combined_data <- left_join(raw_pheno_gest, icc_sex_data, by = c("uncchdid")) %>%
  left_join(BMI_values, by = c("uncchdid")) %>%
  #left_join(clock_data, by = c("uncchdid")) %>%
  #left_join(full_ses_anthro, by = c("uncchdid")) %>%
  unique()

# Select relevant pieces
combined_data <- combined_data %>%
  select(uncchdid, iccsex, date_prg_term, date_icc_meas, icc_length_home, icc_weight_home, icc_armcircm_home, icc_abdom_mean_home, icc_tricep_mean_home, icc_bicep_mean_home, icc_subscap_mean_home, icc_supra_mean_home, icc_calf_mean_home, icc_thigh_mean_home, icc_headcirc_mean_home, gestage, prebmiz, dn_am_age, age_acceleration_diff, age_acceleration_residual, age, dn_am_age_hannum, bio_age4ha_static, dn_am_pheno_age, dn_am_age_skin_blood_clock, dn_am_adm, dn_am_b2m, dn_am_cystatin_c, dn_am_gdf15, dn_am_leptin, dn_am_packyrs, dn_am_pai1, dn_am_timp1, dn_am_grim_age, dn_am_tl, dn_am_age_hannum_adj_age, bio_age4ha_static_adj_age, dn_am_pheno_age_adj_age, dn_am_age_skin_blood_clock_adj_age, dn_am_adm_adj_age, dn_am_b2m_adj_age, dn_am_cystatin_c_adj_age, dn_am_gdf15adj_age, dn_am_leptin_adj_age, dn_am_packyrs_adj_age, dn_am_pai1adj_age, dn_am_timp1adj_age, dn_am_grim_age_adj_age, dn_am_tl_adj_age, ieaa, eeaa, ieaa_hannum, age_acceleration_residual_hannum, age_accel_pheno, age_accel_grim, ses_pc1)

# Z scorer
library(zscorer)

# Model building
