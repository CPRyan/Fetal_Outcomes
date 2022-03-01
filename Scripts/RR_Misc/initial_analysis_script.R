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
  filter(gestage > 308) %>% nrow()
# 10 longer than 308
# 
combined_data1 %>% 
  filter(gestage < 224) %>% nrow()
# 5 shorter than 224 days
# 
combined_data1 %>% 
  filter(is.na(gestage)) %>% nrow()
# 2 missing data for gestage (Do not infer these using the best_gestage variable)

combined_data1 %>% 
  filter(gestage > 308 | gestage < 224 | is.na(gestage)) %>% nrow()
# 17 missing in total

combined_data_ga_fixed <-combined_data1 %>%
  filter(gestage <= 308 & gestage >= 224 & !is.na(gestage))



# Select relevant pieces
combined_data <- combined_data_ga_fixed %>%
  select(uncchdid, blood_spot_date, iccsex, date_prg_term, date_icc_meas, icc_length_home, icc_weight_home, icc_armcircm_home, icc_abdom_mean_home, icc_tricep_mean_home, icc_bicep_mean_home, icc_subscap_mean_home, icc_supra_mean_home, icc_calf_mean_home, icc_thigh_mean_home, icc_headcirc_mean_home, gestage, prebmiz, dn_am_age, age_acceleration_diff, age_acceleration_residual, age, dn_am_age_hannum, bio_age4ha_static, dn_am_pheno_age, dn_am_age_skin_blood_clock, dn_am_adm, dn_am_b2m, dn_am_cystatin_c, dn_am_gdf15, dn_am_leptin, dn_am_packyrs, dn_am_pai1, dn_am_timp1, dn_am_grim_age, dn_am_tl, dn_am_age_hannum_adj_age, bio_age4ha_static_adj_age, dn_am_pheno_age_adj_age, dn_am_age_skin_blood_clock_adj_age, dn_am_adm_adj_age, dn_am_b2m_adj_age, dn_am_cystatin_c_adj_age, dn_am_gdf15adj_age, dn_am_leptin_adj_age, dn_am_packyrs_adj_age, dn_am_pai1adj_age, dn_am_timp1adj_age, dn_am_grim_age_adj_age, dn_am_tl_adj_age, ieaa, eeaa, ieaa_hannum, age_acceleration_residual_hannum, age_accel_pheno, age_accel_grim, cd8p_cd28n_cd45r_an, ses_pc1, gradeic, smoke, max_preg_all) %>%
  mutate(icc_total_skin_mean_home = icc_tricep_mean_home + icc_bicep_mean_home + icc_subscap_mean_home + icc_supra_mean_home + icc_calf_mean_home + icc_thigh_mean_home) %>%
  mutate(measurement_age = date_icc_meas - date_prg_term) %>%
  mutate(days_blood_preg = gestage - (date_prg_term - blood_spot_date)) %>% # CPR added
  na.omit(uncchdid, iccsex, icc_weight_home, gestage, prebmiz, dn_am_age, age_acceleration_diff, age_acceleration_residual, age, dn_am_age_hannum, bio_age4ha_static, dn_am_pheno_age, dn_am_age_skin_blood_clock, dn_am_adm, dn_am_b2m, dn_am_cystatin_c, dn_am_gdf15, dn_am_leptin, dn_am_packyrs, dn_am_pai1, dn_am_timp1, dn_am_grim_age, dn_am_tl, dn_am_age_hannum_adj_age, bio_age4ha_static_adj_age, dn_am_pheno_age_adj_age, dn_am_age_skin_blood_clock_adj_age, dn_am_adm_adj_age, dn_am_b2m_adj_age, dn_am_cystatin_c_adj_age, dn_am_gdf15adj_age, dn_am_leptin_adj_age, dn_am_packyrs_adj_age, dn_am_pai1adj_age, dn_am_timp1adj_age, dn_am_grim_age_adj_age, dn_am_tl_adj_age, ieaa, eeaa, ieaa_hannum, age_acceleration_residual_hannum, age_accel_pheno, age_accel_grim, cd8p_cd28n_cd45r_an, ses_pc1)

combined_data_ga_fixed %>% nrow()
# 334-17 = 317
# Check

combined_data %>% nrow()
# 300
# Find 17 missing values.

# Quick check of who's getting thrown out. 
`%nin%` <- function (x, y) 
{
  return(!(x %in% y))
  }

# Show women not in combined_data (i.e. after removing ga anomalies)
combined_data_ga_fixed %>% 
  filter(uncchdid %nin% combined_data$uncchdid) %>% # Another 17. 
  select(-pregord_presnf, -hrshome, -notes) %>% 
  filter(uncchdid != "22814") %>%  ## 22814 did not pass quality control.
  select_if(function(x) any(is.na(x))) %>%
  visdat::vis_miss()
# 16 women still missing after removing ga anomalies and 22814 who did not pass QC.

# 14 of the remaining women are missing pre-bmi
combined_data_ga_fixed %>% 
  filter(is.na(prebmiz))
  
# The rest (2) others come from women missing all IC data
combined_data_ga_fixed %>% 
  filter(uncchdid %nin% combined_data$uncchdid) %>%
  select(-pregord_presnf, -hrshome, -notes) %>% 
  filter(!is.na(prebmiz)) %>% # Filter to women NOT missing prebmiz
  filter(uncchdid != "22814") %>%  ## 22814 did not pass quality control.
  select_if(function(x) any(is.na(x))) %>%
  visdat::vis_miss()

# 1 woman (22814) didn't pass epigenetic QC
# 14 women were missing prebmi
# 2 women were missing all (or nearly all such as skinfolds) icc data
# This is 17 women. 
# 
# Compare those ICs in the ga_fixed with the combined data. The missing ones


combined_data_ga_fixed %>% 
  filter(uncchdid %nin% combined_data$uncchdid) %>% # Find the ones that are not in the reduced dataset
  select(-contains("Probability")) %>% 
  select_if(function(x) any(is.na(x))) %>% # select any colums with missing values
  visdat::vis_miss()

# Ok, so 
# all but 3 are missing pre-preg BMI
# 1 woman didn't pass epigenetic QC
# 2 women were missing IC data. 
# 
# # Yes this is consistent with what I see.

# Just check measurement age now:
combined_data %>% 
  filter(measurement_age < 15) 
# There are 4 individuals with late fetal measurements



############################################################
# Check scatter plots for days_blood_preg
############################################################

# Scatter plots of Ep Age vs. Weeks Preg
combined_data <- combined_data %>%
  mutate(days_blood_preg = gestage - (date_prg_term - blood_spot_date)) %>%
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


############################################################
# Pretty consistent effect - remove from AgeAccel Measures
############################################################

# First remove late measurement age. 
combined_data <-combined_data %>% 
  filter(measurement_age < 15)




m1 <-lm(ieaa ~ days_blood_preg + smoke, combined_data);summary(m1)

m2 <-update(m1, eeaa ~ . );summary(m2)

m3 <-update(m1, age_accel_pheno ~ . );summary(m3)

m4 <-update(m1, age_accel_grim ~ . );summary(m4)

m5 <-update(m1, dn_am_adm_adj_age ~ . );summary(m5)

m6 <-update(m1, dn_am_b2m_adj_age ~ . );summary(m6)

m7 <-update(m1, dn_am_cystatin_c_adj_age ~ .);summary(m7)

m8 <-update(m1, dn_am_gdf15adj_age ~ .);summary(m8)

m9 <-update(m1, dn_am_leptin_adj_age ~ .);summary(m9)

m10 <-update(m1, dn_am_packyrs_adj_age ~ . );summary(m10)

m11 <-update(m1, dn_am_pai1adj_age ~ . );summary(m11)

m12 <-update(m1, dn_am_timp1adj_age ~ . );summary(m12)

m13 <-update(m1, dn_am_tl_adj_age ~ . );summary(m13)

m14 <-update(m1, age_acceleration_residual ~ . );summary(m14)

m15 <-update(m1, cd8p_cd28n_cd45r_an ~ . + age );summary(m15)


# tab_model (also see tab_regression from the gtsummary packages for single results)
sjPlot::tab_model(m1, m2, m3, m4, m5, m6, m7)
sjPlot::tab_model(m8, m9, m10, m11, m12, m13, m14, m15)


data.frame(cn = names(combined_data)) %>%
  group_by(cn) %>%
  summarize(cnt = n()) %>% 
  print(n = Inf)

combined_data <-modelr::spread_residuals(combined_data, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15) %>%
  rename(ieaa_pregresid = 'm1', 
         eeaa_pregresid = 'm2', 
         age_accel_pheno_pregresid = 'm3', 
         age_accel_grim_pregresid = 'm4', 
         dn_am_adm_adj_age_pregresid = 'm5', 
         dn_am_b2m_adj_age_pregresid = 'm6', 
         dn_am_cystatin_c_adj_age_pregresid = 'm7',
         dn_am_gdf15adj_age_pregresid = 'm8',          
         dn_am_leptin_adj_age_pregresid = 'm9', 
         dn_am_packyrs_adj_age_pregresid = 'm10', 
         dn_am_pai1adj_age_pregresid = 'm11', 
         dn_am_timp1adj_age_pregresid = 'm12',
         dn_am_tl_adj_age_pregresid = 'm13', 
         age_acceleration_residual_pregresid = 'm14', 
         cd8p_cd28n_cd45r_an_age_pregresid = 'm15')



############################################################
# Write new CSV
############################################################

# Create new dataset
write_csv(combined_data, here::here("Data", "combined_final_data.csv"))

