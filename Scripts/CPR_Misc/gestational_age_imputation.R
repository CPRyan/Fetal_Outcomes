#Loading packages
library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(corrplot)
library(haven)

# Load raw pheno
raw_pheno <- read_csv(file = here::here("Data/Phenotypic_Data/full_nsf_w_covariates.csv")) %>%
  clean_names()

# Check for missing lmp or dur
raw_pheno %>% select(contains("lmp"))
# Not present.

# Load pregnancy data for dates and ids

currpreg_dates <- read_dta("Data/Phenotypic_Data/final NSF IC Female Pregnancy Tracking files/Pregnancy files/corrected Stata files/currpreg20141016.dta") %>% 
  select(uncchdid, nsfnumb, pregord, lmpdate, durmonth)

# Check for missing lmp or dur
dim(currpreg_dates)
currpreg_dates %>% 
  summary()
# Only missing 4 for lmp date...

currpreg_dates <- read_dta(file = here::here("Data/Phenotypic_Data/final NSF IC Female Pregnancy Tracking files/Pregnancy files/corrected Stata files/currpreg20141016.dta")) %>% 
  select(uncchdid, nsfnumb, pregord, lmpdate, durmonth, )



# Left join pregnancy dates to raw pheno
raw_pheno_new <-left_join(raw_pheno, 
                      currpreg_dates, 
                      by = c("uncchdid", "nsfnumb", "pregord_all" = "pregord"))

raw_pheno_new %>% 
  summary()
# Ok so I'm missing 125 for lmpdate, and 123 for durmonth. 
# The issue may be arising where pregord_all and pregord are supposed to merge


# Join but remove pregord_all = pregord requirement (i.e. assume that my calculation of pregord all is right or pregord is, but not both)
raw_pheno <-left_join(raw_pheno, 
          currpreg_dates, 
          by = c("uncchdid", "nsfnumb"))

summary(raw_pheno)
# Only 2 NAs now. 

# Now quickly check pregord_all/pregord
raw_pheno %>% 
  filter(pregord_all != pregord) %>% 
  gather(., key = pregord_type, value = number, c(pregord,pregord_all)) %>% 
  ggplot(., aes(pregord_type, number, group = uncchdid))+
  geom_point(alpha = 0.2)+
  geom_line(alpha = 0.1)

# Ok, pregord_all is higher for many than pregord. Also, pregord_all is meant to include any pregnancies that took place during the NSF survey. Use pregord_all.





# Build to be consistent with Chris's code (below)


####################################################
####################################################
# ** generating gestational age variable
# gen gestage = iccbirthdate-lmpdate
# gen gestagem = gestage/30
 

# ** there are 30 values above 301 (43 weeks) and some quite high.  Will infer an approx birth date
# ** based upon durmonth asked during preg interview and birth date.
# ** Will assume 30 day month and set each woman to middle of her reported month


# gen durmonthdays = 150+15 if durmonth==6
# replace durmonthdays = 180+15 if durmonth==7
# replace durmonthdays = 210+15 if durmonth==8
# replace durmonthdays = 240+15 if durmonth==9

# * generate date of pregnancy interview variable
# gen daypregmeas = mdy(monthmeas, daymeas, yearmeas)
# gen lmpinferred = daypregmeas-durmonthdays
# gen gestageinfer = iccbirthdate-lmpinferred


# gen bestgaicc = gestage
# replace bestgaicc = gestageinfer if gestage<0
# replace bestgaicc = gestageinfer if gestage==. & gestageinfer~=.
# replace bestgaicc = gestageinfer if gestage>301

####################################################
####################################################

# CPR: 


# Create ga_file
###############
ga_file <-raw_pheno %>% 
  mutate(gestage = date_preg_term - lmpdate) %>% 
  select(uncchdid, lmpdate, date_preg_term, gestage, durmonth, outcome, date_preg_anthro)


# Look at GA distributions
###############
ga_file %>%   
  ggplot(., aes(x = gestage))+
  geom_histogram()
# Ok, so one is -100 - makes no sense.

# Who is it?
ga_file %>% 
  filter(gestage <0)
# Ok, so clearly the year was just coded wrong. 

# Fix the year of lmp for 23228
ga_file[ga_file$uncchdid == "23228", "lmpdate"] <- lubridate::as_date("2010-02-15")

# Recalculate gestage
ga_file <-ga_file %>% 
  mutate(gestage = date_preg_term - lmpdate) 

mean(ga_file$gestage, na.rm = T)
# 276.7 days. Ok. That seems biased towards earlier

range(ga_file$gestage, na.rm = TRUE)
# That's a crazy difference, even after removing the year error above. 

# Check
ga_file %>% 
  ggplot(., aes(x = gestage))+
  geom_histogram()+
  geom_vline(xintercept = c(269, 308)) # based on the paper cited below
# Yeah. Looks oddly left skewed.

# What if I filter to only those with consistent pregord = pregord_all
raw_pheno[raw_pheno$uncchdid == "23228", "lmpdate"] <- lubridate::as_date("2010-02-15")

raw_pheno %>% 
  mutate(gestage = date_preg_term - lmpdate) %>% 
  filter(pregord_all == pregord) %>%
  ggplot(., aes(x = gestage))+
  geom_histogram()+
  geom_vline(xintercept = c(269, 308))
# Even including only those where pregord and pregord_all fit it looks left skewed. I take it these are the data. 

# Check using only pregnancy samples  
raw_pheno %>% 
  mutate(gestage = date_preg_term - lmpdate) %>% 
  filter(sample_type == "pregnancy") %>% 
  nrow()
# 334 - Do I have some duplicates? 

raw_pheno %>% 
  mutate(gestage = date_preg_term - lmpdate) %>% 
  filter(sample_type == "pregnancy") %>% 
  group_by(uncchdid) %>% 
  filter(n() > 1)
# Nope!


# A few checks...


raw_pheno_w_ga <-raw_pheno %>% 
  mutate(gestage = date_preg_term - lmpdate) %>% 
  filter(sample_type == "pregnancy")

raw_pheno_w_ga %>% 
  ggplot(., aes(x = gestage, y = icc_weight_birth, col = iccsex)) + 
  geom_point(alpha = 0.8)+
  theme_bw()


raw_pheno_w_ga %>%
  mutate(icc_age_at_meas = date_icc_meas-date_prg_term) %>% 
  filter(icc_age_at_meas < 21) %>% 
  ggplot(., aes(x = icc_age_at_meas, y = icc_length_home, col = iccsex)) +
  geom_point(size = 3)+
  scale_color_brewer(type = "qual", palette = 1)+
  facet_grid(~iccsex)+
  theme_bw()
# Ok looks decent. 
# 
# 

# write_csv(raw_pheno_w_ga, here::here("Output/Data", "raw_pheno_w_ga.csv")







###################################################
# Below is a section focused on imputing. We don't really need this after fixing the issue with pregord and pregord_all merging creating missing values. 
###################################################

# Check relationship between durmonth and gestage
###############

ga_file$gestage_days <-
  sjlabelled::remove_all_labels(ga_file$gestage)

ga_file %>% 
  ggplot(., aes(x = durmonth, y = gestage_days, group = durmonth))+
  geom_boxplot(alpha = 0.2)+
  geom_jitter(width = 0.1, alpha = 0.2)
# Ok I have very little faith in durmonth as a variable - it shows almost no relationship with number of dates between lmp and date_preg_term. Only use if absolutely necessary. 

 
# Deal with outlier gestation ages
###############
# Use 269-308 based on Jukic, A. M., Baird, D. D., Weinberg, C. R., McConnaughey, D. R., & Wilcox, A. J. (2013). Length of human pregnancy and contributors to its natural variation. Human Reproduction, 28(10), 2848–2855. https://doi.org/10.1093/humrep/det297

# Early births
###############
ga_file %>% 
  filter(gestage < 269)
# Ok, so quite a few, but fair enough to say these are likely to be pre-term
# May be interesting even just to have a category of 'preterm' or not

# Late Births
#############
ga_file %>% 
  filter(gestage > 308)
# Ok, so 6 still quite late

ga_file <-ga_file %>% 
  mutate(back_calc_lmp = as_date(date_preg_anthro - dmonths(as.numeric(durmonth)))) 

# Create new gestation age variable
##############

# Create a new variable best_gestage_icc
# If is.na(gestage) | gestage > 308
# # If there is a back_calc_lmp (from durmonth), mutate(gestage_rm_long = date_preg_term - back_calc_lmp)
# # Otherwise use rnorm(mean(gestage), sd = sd(gestage))
# Otherwise, use gestage

# Create rnorm for gestation ages
impute_gest_ages <-rnorm(n = 400, mean = mean(ga_file$gestage, na.rm = TRUE), sd = sd(ga_file$gestage, na.rm = TRUE))

# First, get the back calculations and true gestage wherever we can.
ga_file <-ga_file %>% 
  mutate(best_gestage_icc = if_else(is.na(gestage) | 
                   gestage > 308, 
                 date_preg_term - back_calc_lmp, gestage))


# Now, fill in the missing gestation ages. 
# Impute using 'mice' package
# install.packages("mice") <- already installed
library(mice)


ga_file_for_impute <-left_join(ga_file, 
                               raw_pheno %>% 
                                 filter(sample_type == "pregnancy")) %>% 
  select(best_gestage_icc, pregord_all, age_nsf_sample, height, weight_nsf)

imputed_ga <- mice(ga_file_for_impute %>% 
                     mutate(best_gestage_icc = sjlabelled::as_numeric(best_gestage_icc)), 
                            m=5, maxit = 50, method = 'pmm', seed = 500)

## Look at imputed results (5 iterations)
imputed_ga$imp$best_gestage_icc
rowMeans(imputed_ga$imp$best_gestage_icc)

# Try it
ga_file %>%
  filter(is.na(gestage) & is.na(best_gestage_icc))%>% 
  mutate(imputed_ga_means = rowMeans(imputed_ga$imp$best_gestage_icc))
# Works, but not to make a new dataset

# First create column and fill in with zero
ga_file$imputed_ga_means <-0

# Add the imputed values
ga_file[is.na(ga_file$gestage) & is.na(ga_file$best_gestage_icc), "imputed_ga_means"] <-rowMeans(imputed_ga$imp$best_gestage_icc)

ga_file_done_imp <-ga_file %>% 
  mutate(best_or_imputed_gestage_icc = if_else(is.na(gestage) & 
                                                 is.na(best_gestage_icc), 
                                               imputed_ga_means, 
                                               sjlabelled::as_numeric(best_gestage_icc)))



# Check it out. 
ga_file_done_imp %>% 
  ggplot(., aes(x = best_or_imputed_gestage_icc))+
  geom_histogram()

# What is going on
summary(ga_file_done_imp)

# Ok, now I have to filter out the final ones that didn't get fixed...
ga_file_done_imp %>% 
  filter(best_or_imputed_gestage_icc > 308)

# Ok, I only lost 2. Pretty decent!
# 

raw_pheno_gest_impute <-left_join(raw_pheno, 
          ga_file_done_imp %>% 
            select(uncchdid, best_or_imputed_gestage_icc))

#write_csv(raw_pheno_gest_impute, file = here::here("Output/Data/raw_pheno_gest_impute.csv"))
# Written as csv