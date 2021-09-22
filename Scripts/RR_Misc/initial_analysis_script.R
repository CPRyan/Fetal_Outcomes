# Loading packages
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(corrplot)
library(haven)
library(ggpubr)
library(sjlabelled)
library(sjPlot)

# Read in datasets
raw_pheno_gest <- read_csv(file = here::here("Output/Data/raw_pheno_w_ga.csv")) %>%
  clean_names() %>%
  select(-iccsex) %>%
  mutate(uncchdid = as_character(uncchdid)) %>% 
  rename(blood_spot_date = date)

icc_sex_data <- read_csv(file = here::here("Data/Phenotypic_Data/full_nsf_w_covariates.csv")) %>%
  clean_names() %>%
  filter(sample_type == "pregnancy") %>% 
  select(uncchdid, iccsex, nsfnumb) %>%
  mutate(uncchdid = as_character(uncchdid))  # don't use double here


BMI_values <- read_dta(file = here::here("Data/Phenotypic_Data/For Ravi - pre BMI.dta")) %>%
  clean_names() %>%
  select(uncchdid, prebmiz) %>%
  mutate(uncchdid = as_character(uncchdid)) %>% 
  na.omit() %>%  # And remove the NAs, because they always have at least one pre-preg BMI and one NA
  unique() # Here I can remove duplicates, as they're redundant and the same for all pregnancies. 
  


clock_data <- read_csv(file = here::here("Data/Clock_Data/cebu_long_w_baddetP_kept_in.output.csv")) %>%
  clean_names() %>%
  filter(str_detect(sample_id, "_p")) %>%
  mutate(uncchdid = str_sub(sample_id, 2, 6)) %>% # because a few samples have _rep4 at the end, we need 2nd to 6th character, no 2nd to length -1
  mutate(uncchdid = as_character(uncchdid))

full_ses_anthro <- read_csv(file = here::here("Data/Phenotypic_Data/full_ses_anthro.csv")) %>%
  clean_names() %>%
  arrange(uncchdid) %>%
  mutate(uncchdid = as_character(uncchdid)) %>% 
  select(uncchdid, ses_pc1, nsfnumb, gradeic)

# Combine all
combined_data1 <- left_join(raw_pheno_gest, icc_sex_data, by = c("uncchdid", "nsfnumb")) %>%
  left_join(full_ses_anthro, by = c("uncchdid", "nsfnumb")) %>% 
  left_join(clock_data, by = c("uncchdid"))

 
combined_data1 <-left_join(combined_data1, BMI_values, by = c("uncchdid"))

# How many filtered out for gestation age? 
combined_data1 %>% 
  filter(best_gestage > 308)
# 10 longer than 308
# 
combined_data1 %>% 
  filter(best_gestage < 224)
# 5 shorter than 224 days
# 
combined_data1 %>% 
  filter(is.na(best_gestage))
# Used to be 2 missing data for gestage - now with inferred 'best_gestage' there are none.

combined_data1 %>% 
  filter(best_gestage > 308 | best_gestage < 224 | is.na(best_gestage))
# 15 missing in total

combined_data_ga_fixed <-combined_data1 %>%
  filter(best_gestage <= 308 & best_gestage >= 224 & !is.na(best_gestage))

# Select relevant pieces
combined_data <- combined_data_ga_fixed %>%
  select(uncchdid, blood_spot_date, iccsex, date_prg_term, date_icc_meas, icc_length_home, icc_weight_home, icc_armcircm_home, icc_abdom_mean_home, icc_tricep_mean_home, icc_bicep_mean_home, icc_subscap_mean_home, icc_supra_mean_home, icc_calf_mean_home, icc_thigh_mean_home, icc_headcirc_mean_home, best_gestage, prebmiz, dn_am_age, age_acceleration_diff, age_acceleration_residual, age, dn_am_age_hannum, bio_age4ha_static, dn_am_pheno_age, dn_am_age_skin_blood_clock, dn_am_adm, dn_am_b2m, dn_am_cystatin_c, dn_am_gdf15, dn_am_leptin, dn_am_packyrs, dn_am_pai1, dn_am_timp1, dn_am_grim_age, dn_am_tl, dn_am_age_hannum_adj_age, bio_age4ha_static_adj_age, dn_am_pheno_age_adj_age, dn_am_age_skin_blood_clock_adj_age, dn_am_adm_adj_age, dn_am_b2m_adj_age, dn_am_cystatin_c_adj_age, dn_am_gdf15adj_age, dn_am_leptin_adj_age, dn_am_packyrs_adj_age, dn_am_pai1adj_age, dn_am_timp1adj_age, dn_am_grim_age_adj_age, dn_am_tl_adj_age, ieaa, eeaa, ieaa_hannum, age_acceleration_residual_hannum, age_accel_pheno, age_accel_grim, ses_pc1, gradeic, smoke) %>%
  mutate(icc_total_skin_mean_home = icc_tricep_mean_home + icc_bicep_mean_home + icc_subscap_mean_home + icc_supra_mean_home + icc_calf_mean_home + icc_thigh_mean_home) %>%
  mutate(measurement_age = as.double(date_icc_meas - date_prg_term)) %>%
  na.omit(uncchdid, iccsex, icc_weight_home, best_gestage, prebmiz, dn_am_age, age_acceleration_diff, age_acceleration_residual, age, dn_am_age_hannum, bio_age4ha_static, dn_am_pheno_age, dn_am_age_skin_blood_clock, dn_am_adm, dn_am_b2m, dn_am_cystatin_c, dn_am_gdf15, dn_am_leptin, dn_am_packyrs, dn_am_pai1, dn_am_timp1, dn_am_grim_age, dn_am_tl, dn_am_age_hannum_adj_age, bio_age4ha_static_adj_age, dn_am_pheno_age_adj_age, dn_am_age_skin_blood_clock_adj_age, dn_am_adm_adj_age, dn_am_b2m_adj_age, dn_am_cystatin_c_adj_age, dn_am_gdf15adj_age, dn_am_leptin_adj_age, dn_am_packyrs_adj_age, dn_am_pai1adj_age, dn_am_timp1adj_age, dn_am_grim_age_adj_age, dn_am_tl_adj_age, ieaa, eeaa, ieaa_hannum, age_acceleration_residual_hannum, age_accel_pheno, age_accel_grim, ses_pc1)

# Quick check of who's getting thrown out. 
`%nin%` <- function (x, y) 
{
  return(!(x %in% y))
  }

# Show women not in combined_data (i.e. after removing ga anomalies)
combined_data_ga_fixed %>% 
  filter(uncchdid %nin% combined_data$uncchdid) %>%
  select(-pregord_presnf, -hrshome, -notes) %>% 
  filter(uncchdid != "22814") %>%  ## 22814 did not pass quality control.
  select_if(function(x) any(is.na(x))) %>%
  visdat::vis_miss()
# 17 women still missing after removing ga anomalies

# 15 of the remaining women are missing pre-bmi
combined_data_ga_fixed %>% 
  filter(is.na(prebmiz))
  
# The rest (3) come from women missing
combined_data_ga_fixed %>% 
  filter(uncchdid %nin% combined_data$uncchdid) %>%
  select(-pregord_presnf, -hrshome, -notes) %>% 
  filter(!is.na(prebmiz)) %>% # Filter to women NOT missing prebmiz
  filter(uncchdid != "22814") %>%  ## 22814 did not pass quality control.
  select_if(function(x) any(is.na(x))) %>%
  visdat::vis_miss()

# 1 woman (22814) didn't pass epigenetic QC
# 15 women were missing prebmi
# 2 women were missing all (or nearly all such as skinfolds) icc data


# Create new dataset
write_csv(combined_data, here::here("Data", "combined_final_data.csv"))

# Scatter plots of Ep Age vs. Weeks Preg
combined_data <- combined_data %>%
  mutate(days_blood_preg = best_gestage - (date_prg_term - blood_spot_date)) %>%
  mutate(days_blood_preg = as.double(days_blood_preg))

## Scatter plot with correlation coefficient
ggplot(data = combined_data, mapping = aes(x = days_blood_preg, y = ieaa)) +
  geom_point() +
  stat_cor(method = "pearson") +
  geom_smooth(method='lm', formula= y~x)

ggplot(data = combined_data, mapping = aes(x = days_blood_preg, y = eeaa)) +
  geom_point() +
  stat_cor(method = "pearson") +
  geom_smooth(method='lm', formula= y~x)

ggplot(data = combined_data, mapping = aes(x = days_blood_preg, y = age_accel_pheno)) +
  geom_point() +
  stat_cor(method = "pearson") +
  geom_smooth(method='lm', formula= y~x)

ggplot(data = combined_data, mapping = aes(x = days_blood_preg, y = age_accel_grim)) +
  geom_point() +
  stat_cor(method = "pearson") +
  geom_smooth(method='lm', formula= y~x)

ggplot(data = combined_data, mapping = aes(x = prebmiz, y = ieaa)) +
  geom_point() +
  stat_cor(method = "pearson") +
  geom_smooth(method='lm', formula= y~x)

ggplot(data = combined_data, mapping = aes(x = prebmiz, y = eeaa)) +
  geom_point() +
  stat_cor(method = "pearson") +
  geom_smooth(method='lm', formula= y~x)

ggplot(data = combined_data, mapping = aes(x = prebmiz, y = age_accel_pheno)) +
  geom_point() +
  stat_cor(method = "pearson") +
  geom_smooth(method='lm', formula= y~x)

ggplot(data = combined_data, mapping = aes(x = prebmiz, y = age_accel_grim)) +
  geom_point() +
  stat_cor(method = "pearson") +
  geom_smooth(method='lm', formula= y~x)

