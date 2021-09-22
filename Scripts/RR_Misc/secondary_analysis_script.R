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

# Create new dataset
combined_data <- read_csv(here::here("Data/combined_final_data.csv"))

# Model building

## No Pre-BMI adjustments, IEAA
ieaa_no_bmi_icc_weight_home <- lm(icc_weight_home ~ ieaa + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

ieaa_no_bmi_icc_length_home <- update(ieaa_no_bmi_icc_weight_home, icc_length_home ~ .)

ieaa_no_bmi_icc_armcircm_home <- update(ieaa_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

ieaa_no_bmi_icc_abdom_mean_home <- update(ieaa_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

ieaa_no_bmi_icc_headcirc_mean_home <- update(ieaa_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

ieaa_no_bmi_icc_total_skin_mean_home <- update(ieaa_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(ieaa_no_bmi_icc_weight_home, ieaa_no_bmi_icc_length_home, ieaa_no_bmi_icc_armcircm_home, ieaa_no_bmi_icc_abdom_mean_home, ieaa_no_bmi_icc_headcirc_mean_home, ieaa_no_bmi_icc_total_skin_mean_home)


## No Pre-BMI adjustments, EEAA
eeaa_no_bmi_icc_weight_home <- lm(icc_weight_home ~ eeaa + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

eeaa_no_bmi_icc_length_home <- update(eeaa_no_bmi_icc_weight_home, icc_length_home ~ .)

eeaa_no_bmi_icc_armcircm_home <- update(eeaa_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

eeaa_no_bmi_icc_abdom_mean_home <- update(eeaa_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

eeaa_no_bmi_icc_headcirc_mean_home <- update(eeaa_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

eeaa_no_bmi_icc_total_skin_mean_home <- update(eeaa_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(eeaa_no_bmi_icc_weight_home, eeaa_no_bmi_icc_length_home, eeaa_no_bmi_icc_armcircm_home, eeaa_no_bmi_icc_abdom_mean_home, eeaa_no_bmi_icc_headcirc_mean_home, eeaa_no_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, phenoage
age_accel_pheno_no_bmi_icc_weight_home <- lm(icc_weight_home ~ age_accel_pheno + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

age_accel_pheno_no_bmi_icc_length_home <- update(age_accel_pheno_no_bmi_icc_weight_home, icc_length_home ~ .)

age_accel_pheno_no_bmi_icc_armcircm_home <- update(age_accel_pheno_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

age_accel_pheno_no_bmi_icc_abdom_mean_home <- update(age_accel_pheno_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

age_accel_pheno_no_bmi_icc_headcirc_mean_home <- update(age_accel_pheno_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

age_accel_pheno_no_bmi_icc_total_skin_mean_home <- update(age_accel_pheno_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(age_accel_pheno_no_bmi_icc_weight_home, age_accel_pheno_no_bmi_icc_length_home, age_accel_pheno_no_bmi_icc_armcircm_home, age_accel_pheno_no_bmi_icc_abdom_mean_home, age_accel_pheno_no_bmi_icc_headcirc_mean_home, age_accel_pheno_no_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, grimage
age_accel_grim_no_bmi_icc_weight_home <- lm(icc_weight_home ~ age_accel_grim + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

age_accel_grim_no_bmi_icc_length_home <- update(age_accel_grim_no_bmi_icc_weight_home, icc_length_home ~ .)

age_accel_grim_no_bmi_icc_armcircm_home <- update(age_accel_grim_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

age_accel_grim_no_bmi_icc_abdom_mean_home <- update(age_accel_grim_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

age_accel_grim_no_bmi_icc_headcirc_mean_home <- update(age_accel_grim_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

age_accel_grim_no_bmi_icc_total_skin_mean_home <- update(age_accel_grim_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(age_accel_grim_no_bmi_icc_weight_home, age_accel_grim_no_bmi_icc_length_home, age_accel_grim_no_bmi_icc_armcircm_home, age_accel_grim_no_bmi_icc_abdom_mean_home, age_accel_grim_no_bmi_icc_headcirc_mean_home, age_accel_grim_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, IEAA
ieaa_bmi_icc_weight_home <- lm(icc_weight_home ~ ieaa + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

ieaa_bmi_icc_length_home <- update(ieaa_bmi_icc_weight_home, icc_length_home ~ .)

ieaa_bmi_icc_armcircm_home <- update(ieaa_bmi_icc_weight_home, icc_armcircm_home ~ .)

ieaa_bmi_icc_abdom_mean_home <- update(ieaa_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

ieaa_bmi_icc_headcirc_mean_home <- update(ieaa_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

ieaa_bmi_icc_total_skin_mean_home <- update(ieaa_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(ieaa_bmi_icc_weight_home, ieaa_bmi_icc_length_home, ieaa_bmi_icc_armcircm_home, ieaa_bmi_icc_abdom_mean_home, ieaa_bmi_icc_headcirc_mean_home, ieaa_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, EEAA
eeaa_bmi_icc_weight_home <- lm(icc_weight_home ~ eeaa + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

eeaa_bmi_icc_length_home <- update(eeaa_bmi_icc_weight_home, icc_length_home ~ .)

eeaa_bmi_icc_armcircm_home <- update(eeaa_bmi_icc_weight_home, icc_armcircm_home ~ .)

eeaa_bmi_icc_abdom_mean_home <- update(eeaa_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

eeaa_bmi_icc_headcirc_mean_home <- update(eeaa_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

eeaa_bmi_icc_total_skin_mean_home <- update(eeaa_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(eeaa_bmi_icc_weight_home, eeaa_bmi_icc_length_home, eeaa_bmi_icc_armcircm_home, eeaa_bmi_icc_abdom_mean_home, eeaa_bmi_icc_headcirc_mean_home, eeaa_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, phenoage
age_accel_pheno_bmi_icc_weight_home <- lm(icc_weight_home ~ age_accel_pheno + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

age_accel_pheno_bmi_icc_length_home <- update(age_accel_pheno_bmi_icc_weight_home, icc_length_home ~ .)

age_accel_pheno_bmi_icc_armcircm_home <- update(age_accel_pheno_bmi_icc_weight_home, icc_armcircm_home ~ .)

age_accel_pheno_bmi_icc_abdom_mean_home <- update(age_accel_pheno_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

age_accel_pheno_bmi_icc_headcirc_mean_home <- update(age_accel_pheno_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

age_accel_pheno_bmi_icc_total_skin_mean_home <- update(age_accel_pheno_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(age_accel_pheno_bmi_icc_weight_home, age_accel_pheno_bmi_icc_length_home, age_accel_pheno_bmi_icc_armcircm_home, age_accel_pheno_bmi_icc_abdom_mean_home, age_accel_pheno_bmi_icc_headcirc_mean_home, age_accel_pheno_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, grimage
age_accel_grim_bmi_icc_weight_home <- lm(icc_weight_home ~ age_accel_grim + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

age_accel_grim_bmi_icc_length_home <- update(age_accel_grim_bmi_icc_weight_home, icc_length_home ~ .)

age_accel_grim_bmi_icc_armcircm_home <- update(age_accel_grim_bmi_icc_weight_home, icc_armcircm_home ~ .)

age_accel_grim_bmi_icc_abdom_mean_home <- update(age_accel_grim_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

age_accel_grim_bmi_icc_headcirc_mean_home <- update(age_accel_grim_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

age_accel_grim_bmi_icc_total_skin_mean_home <- update(age_accel_grim_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(age_accel_grim_bmi_icc_weight_home, age_accel_grim_bmi_icc_length_home, age_accel_grim_bmi_icc_armcircm_home, age_accel_grim_bmi_icc_abdom_mean_home, age_accel_grim_bmi_icc_headcirc_mean_home, age_accel_grim_bmi_icc_total_skin_mean_home)

# Diagnostic Plots



######

## No Pre-BMI adjustments, dn_am_adm_adj_age
dn_am_adm_adj_age_no_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_adm_adj_age + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

dn_am_adm_adj_age_no_bmi_icc_length_home <- update(dn_am_adm_adj_age_no_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_adm_adj_age_no_bmi_icc_armcircm_home <- update(dn_am_adm_adj_age_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_adm_adj_age_no_bmi_icc_abdom_mean_home <- update(dn_am_adm_adj_age_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_adm_adj_age_no_bmi_icc_headcirc_mean_home <- update(dn_am_adm_adj_age_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_adm_adj_age_no_bmi_icc_total_skin_mean_home <- update(dn_am_adm_adj_age_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_adm_adj_age_no_bmi_icc_weight_home, dn_am_adm_adj_age_no_bmi_icc_length_home, dn_am_adm_adj_age_no_bmi_icc_armcircm_home, dn_am_adm_adj_age_no_bmi_icc_abdom_mean_home, dn_am_adm_adj_age_no_bmi_icc_headcirc_mean_home, dn_am_adm_adj_age_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, dn_am_adm_adj_age
dn_am_adm_adj_age_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_adm_adj_age + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

dn_am_adm_adj_age_bmi_icc_length_home <- update(dn_am_adm_adj_age_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_adm_adj_age_bmi_icc_armcircm_home <- update(dn_am_adm_adj_age_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_adm_adj_age_bmi_icc_abdom_mean_home <- update(dn_am_adm_adj_age_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_adm_adj_age_bmi_icc_headcirc_mean_home <- update(dn_am_adm_adj_age_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_adm_adj_age_bmi_icc_total_skin_mean_home <- update(dn_am_adm_adj_age_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_adm_adj_age_bmi_icc_weight_home, dn_am_adm_adj_age_bmi_icc_length_home, dn_am_adm_adj_age_bmi_icc_armcircm_home, dn_am_adm_adj_age_bmi_icc_abdom_mean_home, dn_am_adm_adj_age_bmi_icc_headcirc_mean_home, dn_am_adm_adj_age_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, dn_am_b2m_adj_age
clock_1_no_bmi_icc_weight_home <- lm(icc_weight_home ~ clock_1 + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

clock_1_no_bmi_icc_length_home <- update(clock_1_no_bmi_icc_weight_home, icc_length_home ~ .)

clock_1_no_bmi_icc_armcircm_home <- update(clock_1_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

clock_1_no_bmi_icc_abdom_mean_home <- update(clock_1_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

clock_1_no_bmi_icc_headcirc_mean_home <- update(clock_1_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

clock_1_no_bmi_icc_total_skin_mean_home <- update(clock_1_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(clock_1_no_bmi_icc_weight_home, clock_1_no_bmi_icc_length_home, clock_1_no_bmi_icc_armcircm_home, clock_1_no_bmi_icc_abdom_mean_home, clock_1_no_bmi_icc_headcirc_mean_home, clock_1_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, dn_am_b2m_adj_age
dn_am_b2m_adj_age_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_b2m_adj_age + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

dn_am_b2m_adj_age_bmi_icc_length_home <- update(dn_am_b2m_adj_age_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_b2m_adj_age_bmi_icc_armcircm_home <- update(dn_am_b2m_adj_age_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_b2m_adj_age_bmi_icc_abdom_mean_home <- update(dn_am_b2m_adj_age_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_b2m_adj_age_bmi_icc_headcirc_mean_home <- update(dn_am_b2m_adj_age_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_b2m_adj_age_bmi_icc_total_skin_mean_home <- update(dn_am_b2m_adj_age_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_b2m_adj_age_bmi_icc_weight_home, dn_am_b2m_adj_age_bmi_icc_length_home, dn_am_b2m_adj_age_bmi_icc_armcircm_home, dn_am_b2m_adj_age_bmi_icc_abdom_mean_home, dn_am_b2m_adj_age_bmi_icc_headcirc_mean_home, dn_am_b2m_adj_age_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, dn_am_cystatin_c_adj_age
dn_am_cystatin_c_adj_age_no_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_cystatin_c_adj_age + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

dn_am_cystatin_c_adj_age_no_bmi_icc_length_home <- update(dn_am_cystatin_c_adj_age_no_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_cystatin_c_adj_age_no_bmi_icc_armcircm_home <- update(dn_am_cystatin_c_adj_age_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_cystatin_c_adj_age_no_bmi_icc_abdom_mean_home <- update(dn_am_cystatin_c_adj_age_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_cystatin_c_adj_age_no_bmi_icc_headcirc_mean_home <- update(dn_am_cystatin_c_adj_age_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_cystatin_c_adj_age_no_bmi_icc_total_skin_mean_home <- update(dn_am_cystatin_c_adj_age_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_cystatin_c_adj_age_no_bmi_icc_weight_home, dn_am_cystatin_c_adj_age_no_bmi_icc_length_home, dn_am_cystatin_c_adj_age_no_bmi_icc_armcircm_home, dn_am_cystatin_c_adj_age_no_bmi_icc_abdom_mean_home, dn_am_cystatin_c_adj_age_no_bmi_icc_headcirc_mean_home, dn_am_cystatin_c_adj_age_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, dn_am_cystatin_c_adj_age
dn_am_cystatin_c_adj_age_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_cystatin_c_adj_age + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

dn_am_cystatin_c_adj_age_bmi_icc_length_home <- update(dn_am_cystatin_c_adj_age_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_cystatin_c_adj_age_bmi_icc_armcircm_home <- update(dn_am_cystatin_c_adj_age_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_cystatin_c_adj_age_bmi_icc_abdom_mean_home <- update(dn_am_cystatin_c_adj_age_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_cystatin_c_adj_age_bmi_icc_headcirc_mean_home <- update(dn_am_cystatin_c_adj_age_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_cystatin_c_adj_age_bmi_icc_total_skin_mean_home <- update(dn_am_cystatin_c_adj_age_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_cystatin_c_adj_age_bmi_icc_weight_home, dn_am_cystatin_c_adj_age_bmi_icc_length_home, dn_am_cystatin_c_adj_age_bmi_icc_armcircm_home, dn_am_cystatin_c_adj_age_bmi_icc_abdom_mean_home, dn_am_cystatin_c_adj_age_bmi_icc_headcirc_mean_home, dn_am_cystatin_c_adj_age_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, dn_am_gdf15adj_age
dn_am_gdf15adj_age_no_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_gdf15adj_age + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

dn_am_gdf15adj_age_no_bmi_icc_length_home <- update(dn_am_gdf15adj_age_no_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_gdf15adj_age_no_bmi_icc_armcircm_home <- update(dn_am_gdf15adj_age_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_gdf15adj_age_no_bmi_icc_abdom_mean_home <- update(dn_am_gdf15adj_age_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_gdf15adj_age_no_bmi_icc_headcirc_mean_home <- update(dn_am_gdf15adj_age_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_gdf15adj_age_no_bmi_icc_total_skin_mean_home <- update(dn_am_gdf15adj_age_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_gdf15adj_age_no_bmi_icc_weight_home, dn_am_gdf15adj_age_no_bmi_icc_length_home, dn_am_gdf15adj_age_no_bmi_icc_armcircm_home, dn_am_gdf15adj_age_no_bmi_icc_abdom_mean_home, dn_am_gdf15adj_age_no_bmi_icc_headcirc_mean_home, dn_am_gdf15adj_age_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, dn_am_gdf15adj_age
dn_am_gdf15adj_age_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_gdf15adj_age + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

dn_am_gdf15adj_age_bmi_icc_length_home <- update(dn_am_gdf15adj_age_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_gdf15adj_age_bmi_icc_armcircm_home <- update(dn_am_gdf15adj_age_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_gdf15adj_age_bmi_icc_abdom_mean_home <- update(dn_am_gdf15adj_age_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_gdf15adj_age_bmi_icc_headcirc_mean_home <- update(dn_am_gdf15adj_age_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_gdf15adj_age_bmi_icc_total_skin_mean_home <- update(dn_am_gdf15adj_age_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_gdf15adj_age_bmi_icc_weight_home, dn_am_gdf15adj_age_bmi_icc_length_home, dn_am_gdf15adj_age_bmi_icc_armcircm_home, dn_am_gdf15adj_age_bmi_icc_abdom_mean_home, dn_am_gdf15adj_age_bmi_icc_headcirc_mean_home, dn_am_gdf15adj_age_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, dn_am_leptin_adj_age
dn_am_leptin_adj_age_no_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_leptin_adj_age + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

dn_am_leptin_adj_age_no_bmi_icc_length_home <- update(dn_am_leptin_adj_age_no_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_leptin_adj_age_no_bmi_icc_armcircm_home <- update(dn_am_leptin_adj_age_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_leptin_adj_age_no_bmi_icc_abdom_mean_home <- update(dn_am_leptin_adj_age_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_leptin_adj_age_no_bmi_icc_headcirc_mean_home <- update(dn_am_leptin_adj_age_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_leptin_adj_age_no_bmi_icc_total_skin_mean_home <- update(dn_am_leptin_adj_age_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_leptin_adj_age_no_bmi_icc_weight_home, dn_am_leptin_adj_age_no_bmi_icc_length_home, dn_am_leptin_adj_age_no_bmi_icc_armcircm_home, dn_am_leptin_adj_age_no_bmi_icc_abdom_mean_home, dn_am_leptin_adj_age_no_bmi_icc_headcirc_mean_home, dn_am_leptin_adj_age_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, dn_am_leptin_adj_age
dn_am_leptin_adj_age_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_leptin_adj_age + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

dn_am_leptin_adj_age_bmi_icc_length_home <- update(dn_am_leptin_adj_age_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_leptin_adj_age_bmi_icc_armcircm_home <- update(dn_am_leptin_adj_age_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_leptin_adj_age_bmi_icc_abdom_mean_home <- update(dn_am_leptin_adj_age_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_leptin_adj_age_bmi_icc_headcirc_mean_home <- update(dn_am_leptin_adj_age_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_leptin_adj_age_bmi_icc_total_skin_mean_home <- update(dn_am_leptin_adj_age_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_leptin_adj_age_bmi_icc_weight_home, dn_am_leptin_adj_age_bmi_icc_length_home, dn_am_leptin_adj_age_bmi_icc_armcircm_home, dn_am_leptin_adj_age_bmi_icc_abdom_mean_home, dn_am_leptin_adj_age_bmi_icc_headcirc_mean_home, dn_am_leptin_adj_age_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, dn_am_packyrs_adj_age
dn_am_packyrs_adj_age_no_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_packyrs_adj_age + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

dn_am_packyrs_adj_age_no_bmi_icc_length_home <- update(dn_am_packyrs_adj_age_no_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_packyrs_adj_age_no_bmi_icc_armcircm_home <- update(dn_am_packyrs_adj_age_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_packyrs_adj_age_no_bmi_icc_abdom_mean_home <- update(dn_am_packyrs_adj_age_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_packyrs_adj_age_no_bmi_icc_headcirc_mean_home <- update(dn_am_packyrs_adj_age_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_packyrs_adj_age_no_bmi_icc_total_skin_mean_home <- update(dn_am_packyrs_adj_age_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_packyrs_adj_age_no_bmi_icc_weight_home, dn_am_packyrs_adj_age_no_bmi_icc_length_home, dn_am_packyrs_adj_age_no_bmi_icc_armcircm_home, dn_am_packyrs_adj_age_no_bmi_icc_abdom_mean_home, dn_am_packyrs_adj_age_no_bmi_icc_headcirc_mean_home, dn_am_packyrs_adj_age_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, dn_am_packyrs_adj_age
dn_am_packyrs_adj_age_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_packyrs_adj_age + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

dn_am_packyrs_adj_age_bmi_icc_length_home <- update(dn_am_packyrs_adj_age_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_packyrs_adj_age_bmi_icc_armcircm_home <- update(dn_am_packyrs_adj_age_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_packyrs_adj_age_bmi_icc_abdom_mean_home <- update(dn_am_packyrs_adj_age_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_packyrs_adj_age_bmi_icc_headcirc_mean_home <- update(dn_am_packyrs_adj_age_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_packyrs_adj_age_bmi_icc_total_skin_mean_home <- update(dn_am_packyrs_adj_age_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_packyrs_adj_age_bmi_icc_weight_home, dn_am_packyrs_adj_age_bmi_icc_length_home, dn_am_packyrs_adj_age_bmi_icc_armcircm_home, dn_am_packyrs_adj_age_bmi_icc_abdom_mean_home, dn_am_packyrs_adj_age_bmi_icc_headcirc_mean_home, dn_am_packyrs_adj_age_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, dn_am_pai1adj_age
dn_am_pai1adj_age_no_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_pai1adj_age + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

dn_am_pai1adj_age_no_bmi_icc_length_home <- update(dn_am_pai1adj_age_no_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_pai1adj_age_no_bmi_icc_armcircm_home <- update(dn_am_pai1adj_age_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_pai1adj_age_no_bmi_icc_abdom_mean_home <- update(dn_am_pai1adj_age_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_pai1adj_age_no_bmi_icc_headcirc_mean_home <- update(dn_am_pai1adj_age_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_pai1adj_age_no_bmi_icc_total_skin_mean_home <- update(dn_am_pai1adj_age_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_pai1adj_age_no_bmi_icc_weight_home, dn_am_pai1adj_age_no_bmi_icc_length_home, dn_am_pai1adj_age_no_bmi_icc_armcircm_home, dn_am_pai1adj_age_no_bmi_icc_abdom_mean_home, dn_am_pai1adj_age_no_bmi_icc_headcirc_mean_home, dn_am_pai1adj_age_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, dn_am_pai1adj_age
dn_am_pai1adj_age_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_pai1adj_age + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

dn_am_pai1adj_age_bmi_icc_length_home <- update(dn_am_pai1adj_age_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_pai1adj_age_bmi_icc_armcircm_home <- update(dn_am_pai1adj_age_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_pai1adj_age_bmi_icc_abdom_mean_home <- update(dn_am_pai1adj_age_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_pai1adj_age_bmi_icc_headcirc_mean_home <- update(dn_am_pai1adj_age_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_pai1adj_age_bmi_icc_total_skin_mean_home <- update(dn_am_pai1adj_age_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_pai1adj_age_bmi_icc_weight_home, dn_am_pai1adj_age_bmi_icc_length_home, dn_am_pai1adj_age_bmi_icc_armcircm_home, dn_am_pai1adj_age_bmi_icc_abdom_mean_home, dn_am_pai1adj_age_bmi_icc_headcirc_mean_home, dn_am_pai1adj_age_bmi_icc_total_skin_mean_home)

## No Pre-BMI adjustments, dn_am_timp1adj_age
dn_am_timp1adj_age_no_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_timp1adj_age + best_gestage + measurement_age + iccsex + ses_pc1, data = combined_data)

dn_am_timp1adj_age_no_bmi_icc_length_home <- update(dn_am_timp1adj_age_no_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_timp1adj_age_no_bmi_icc_armcircm_home <- update(dn_am_timp1adj_age_no_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_timp1adj_age_no_bmi_icc_abdom_mean_home <- update(dn_am_timp1adj_age_no_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_timp1adj_age_no_bmi_icc_headcirc_mean_home <- update(dn_am_timp1adj_age_no_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_timp1adj_age_no_bmi_icc_total_skin_mean_home <- update(dn_am_timp1adj_age_no_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_timp1adj_age_no_bmi_icc_weight_home, dn_am_timp1adj_age_no_bmi_icc_length_home, dn_am_timp1adj_age_no_bmi_icc_armcircm_home, dn_am_timp1adj_age_no_bmi_icc_abdom_mean_home, dn_am_timp1adj_age_no_bmi_icc_headcirc_mean_home, dn_am_timp1adj_age_no_bmi_icc_total_skin_mean_home)

## Pre-BMI adjustments, dn_am_timp1adj_age
dn_am_timp1adj_age_bmi_icc_weight_home <- lm(icc_weight_home ~ dn_am_timp1adj_age + best_gestage + measurement_age + iccsex + ses_pc1 + prebmiz, data = combined_data)

dn_am_timp1adj_age_bmi_icc_length_home <- update(dn_am_timp1adj_age_bmi_icc_weight_home, icc_length_home ~ .)

dn_am_timp1adj_age_bmi_icc_armcircm_home <- update(dn_am_timp1adj_age_bmi_icc_weight_home, icc_armcircm_home ~ .)

dn_am_timp1adj_age_bmi_icc_abdom_mean_home <- update(dn_am_timp1adj_age_bmi_icc_weight_home, icc_abdom_mean_home ~ .)

dn_am_timp1adj_age_bmi_icc_headcirc_mean_home <- update(dn_am_timp1adj_age_bmi_icc_weight_home, icc_headcirc_mean_home ~ .)

dn_am_timp1adj_age_bmi_icc_total_skin_mean_home <- update(dn_am_timp1adj_age_bmi_icc_weight_home, icc_total_skin_mean_home ~ .)

sjPlot::tab_model(dn_am_timp1adj_age_bmi_icc_weight_home, dn_am_timp1adj_age_bmi_icc_length_home, dn_am_timp1adj_age_bmi_icc_armcircm_home, dn_am_timp1adj_age_bmi_icc_abdom_mean_home, dn_am_timp1adj_age_bmi_icc_headcirc_mean_home, dn_am_timp1adj_age_bmi_icc_total_skin_mean_home)

