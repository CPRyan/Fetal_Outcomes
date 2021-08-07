# Load packages
library(tidyverse)
library(janitor)
library(skimr)
library(corrplot)
library(RColorBrewer)
library(haven)

# Input Data
raw_pheno_gest_impute <- read_csv(file = here::here("Output/Data/raw_pheno_gest_impute.csv"))

# Extract relevant vars
imp_gest_vars <- raw_pheno_gest_impute %>%
  select(uncchdid, monthprg, dayprg, yearprg, sample_group, sample_type, iccsex, date_prg_term, hour, minute, am_pm, sourcewt, date_icc_meas, icc_weight_birth, icc_length_home, icc_weight_home, icc_armcircm_home, icc_tricep_mean_home, icc_bicep_mean_home, icc_subscap_mean_home, icc_supra_mean_home, icc_calf_mean_home, icc_thigh_mean_home, icc_headcirc_mean_home, icc_abdom_mean_home, best_or_imputed_gestage_icc) %>%
  filter(sample_type == "pregnancy") %>%
  na_if(-8) %>%
  mutate(icc_total_skin_mean_home = icc_tricep_mean_home + icc_bicep_mean_home + icc_subscap_mean_home + icc_supra_mean_home + icc_calf_mean_home + icc_thigh_mean_home) %>%
  mutate(measurement_age = date_icc_meas - date_prg_term)

## Summary Statistics Overview


skimr::skim(imp_gest_vars)


## Distribution of Birth Weight


imp_gest_vars %>%
  ggplot(mapping = aes(x = icc_weight_birth)) +
  geom_histogram()


## Distribution of Length Measured at Home


imp_gest_vars %>%
  ggplot(mapping = aes(x = icc_length_home)) +
  geom_histogram()


## Distribution of Weight Measured at Home


imp_gest_vars %>%
  ggplot(mapping = aes(x = icc_weight_home)) +
  geom_histogram()


## Distribution of Arm Circumference Measured at Home


imp_gest_vars %>%
  ggplot(mapping = aes(x = icc_armcircm_home)) +
  geom_histogram()


## Distribution of Total Skin Fold Thickness Measured at Home


imp_gest_vars %>%
  ggplot(mapping = aes(x = icc_total_skin_mean_home)) +
  geom_histogram()


## Distribution of Head Circumference Measured at Home


imp_gest_vars %>%
  ggplot(mapping = aes(x = icc_headcirc_mean_home)) +
  geom_histogram()


## Distribution of Abdominal Circumference Measured at Home


imp_gest_vars %>%
  ggplot(mapping = aes(x = icc_abdom_mean_home)) +
  geom_histogram()


## Distribution of Gestational Age at Birth (New Imputed Code)


imp_gest_vars %>%
  ggplot(mapping = aes(x = best_or_imputed_gestage_icc)) +
  geom_histogram()


## Distribution of Age at Measurement at Home (Date of Measurement - Date of End of Pregnancy) --> seems fishy that there are measurements that are nearly/over a month after birth. What to do here?


imp_gest_vars %>%
  ggplot(mapping = aes(x = measurement_age)) +
  geom_histogram()


## Correlation Plot between various Phenotypic Variables


imp_gest_vars2 <- imp_gest_vars %>% select(icc_weight_birth, icc_length_home, icc_weight_home, icc_armcircm_home, icc_headcirc_mean_home, icc_abdom_mean_home, icc_total_skin_mean_home)
M <- cor(imp_gest_vars2, use = "complete.obs")
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

