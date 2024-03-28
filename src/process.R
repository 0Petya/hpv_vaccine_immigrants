library(foreign)
library(tidyverse)

# Download data for each questionnaire.

# Demographics
# SEQN: Respondent sequence number
# RIAGENDR: Gender
# DMDBORN4: Country of birth
# RIDAGEYR: Age in years at screening
# RIDRETH1: Race/Hispanic origin
# DMDEDUC3: Education level - Children/Youth 6-19
# DMDEDUC2: Education level - Adults 20+
# DMDMARTL: Martial status (REMOVED)
# INDFMPIR: Ratio of family income to poverty
# DMDHHSIZ: Total number of people in the Household
# DMDCITZN: Citizenship status
# DMDYRSUS: Length of time in US
# SIALANG: Language of SP interview
# SDMVPSU: Masked variance pseudo-PSU
# SDMVSTRA: Masked variance pseudo-stratum
# WTINT2YR: Full sample 2 year interview weight

demo_variables <- c("SEQN", "RIAGENDR", "DMDBORN4", "RIDAGEYR", "RIDRETH1", "DMDEDUC3", "DMDEDUC2", "INDFMPIR", "DMDHHSIZ", "DMDYRSUS", "DMDCITZN", "SIALANG", "SDMVPSU", "SDMVSTRA", "WTINT2YR")

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode = "wb")
demo_2017 <- read.xport(tf) %>% select(demo_variables) %>% mutate(year = 2017)
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode = "wb")
demo_2015 <- read.xport(tf) %>% select(demo_variables) %>% mutate(year = 2015)
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT", tf <- tempfile(), mode = "wb")
demo_2013 <- read.xport(tf) %>% select(demo_variables) %>% mutate(year = 2013)
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT", tf <- tempfile(), mode = "wb")
demo_2011 <- read.xport(tf) %>% select(demo_variables) %>% mutate(year = 2011)
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT", tf <- tempfile(), mode = "wb")
demo_2009 <- read.xport(tf) %>% select(c("SEQN", "RIAGENDR", "DMDBORN2", "RIDAGEYR", "RIDRETH1", "DMDEDUC3", "DMDEDUC2", "INDFMPIR", "DMDHHSIZ", "DMDYRSUS", "DMDCITZN", "SIALANG", "SDMVPSU", "SDMVSTRA", "WTINT2YR")) %>%
  mutate(DMDBORN4 = case_when(
    DMDBORN2 %in% c(2, 4, 5) ~ 2,
    DMDBORN2 == 77 ~ 7,
    DMDBORN2 == 99 ~ 9,
    T ~ DMDBORN2
  )) %>%
  select(-"DMDBORN2") %>% mutate(year = 2009)
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.XPT", tf <- tempfile(), mode = "wb")
demo_2007 <- read.xport(tf) %>% select(c("SEQN", "RIAGENDR", "DMDBORN2", "RIDAGEYR", "RIDRETH1", "DMDEDUC3", "DMDEDUC2", "INDFMPIR", "DMDHHSIZ", "DMDYRSUS", "DMDCITZN", "SIALANG", "SDMVPSU", "SDMVSTRA", "WTINT2YR")) %>%
  mutate(DMDBORN4 = case_when(
    DMDBORN2 %in% c(2, 4, 5) ~ 2,
    DMDBORN2 == 77 ~ 7,
    DMDBORN2 == 99 ~ 9,
    T ~ DMDBORN2
  )) %>%
  select(-"DMDBORN2") %>% mutate(year = 2007)

# Convert DMDEDUC2 values to DMDEDUC3 values and rename to education.

demo <- bind_rows(demo_2017, demo_2015, demo_2013, demo_2011, demo_2009, demo_2007) %>%
  mutate(education = case_when(
    is.na(DMDEDUC3) ~ DMDEDUC2,
    DMDEDUC3 <= 8 ~ 1,
    DMDEDUC3 >= 9 & DMDEDUC3 <= 12 ~ 2,
    DMDEDUC3 >= 13 & DMDEDUC3 <= 14 ~ 3,
    DMDEDUC3 == 15 ~ 4,
    DMDEDUC3 == 55 | DMDEDUC3 == 66 ~ 1,
    DMDEDUC3 == 77 ~ 7,
    DMDEDUC3 == 99 ~ 9,
    T ~ DMDEDUC2
  )) %>%
  select(-c("DMDEDUC2", "DMDEDUC3"))

# Health Insurance
# HIQ011: Covered by health insurance

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HIQ_J.XPT", tf <- tempfile(), mode = "wb")
insurance_2017 <- read.xport(tf) %>% select("SEQN", "HIQ011")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HIQ_I.XPT", tf <- tempfile(), mode = "wb")
insurance_2015 <- read.xport(tf) %>% select("SEQN", "HIQ011")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/HIQ_H.XPT", tf <- tempfile(), mode = "wb")
insurance_2013 <- read.xport(tf) %>% select("SEQN", "HIQ011")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HIQ_G.XPT", tf <- tempfile(), mode = "wb")
insurance_2011 <- read.xport(tf) %>% select("SEQN", "HIQ011")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/HIQ_F.XPT", tf <- tempfile(), mode = "wb")
insurance_2009 <- read.xport(tf) %>% select("SEQN", "HIQ011")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/HIQ_E.XPT", tf <- tempfile(), mode = "wb")
insurance_2007 <- read.xport(tf) %>% select("SEQN", "HIQ011")

insurance <- bind_rows(insurance_2017, insurance_2015, insurance_2013, insurance_2011, insurance_2009, insurance_2007)

# Hospital Utilization & Access to Care
# HUQ050: #times receive healthcare over past year
# HUQ051: #times receive healthcare over past year
# HUQ030: Routine place to go for healthcare

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HUQ_J.XPT", tf <- tempfile(), mode = "wb")
access_2017 <- read.xport(tf) %>% select("SEQN", "HUQ051", "HUQ030")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HUQ_I.XPT", tf <- tempfile(), mode = "wb")
access_2015 <- read.xport(tf) %>% select("SEQN", "HUQ051", "HUQ030")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/HUQ_H.XPT", tf <- tempfile(), mode = "wb")
access_2013 <- read.xport(tf) %>% select("SEQN", "HUQ051", "HUQ030")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HUQ_G.XPT", tf <- tempfile(), mode = "wb")
access_2011 <- read.xport(tf) %>% select("SEQN", "HUQ050", "HUQ030")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/HUQ_F.XPT", tf <- tempfile(), mode = "wb")
access_2009 <- read.xport(tf) %>% select("SEQN", "HUQ050", "HUQ030")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/HUQ_E.XPT", tf <- tempfile(), mode = "wb")
access_2007 <- read.xport(tf) %>% select("SEQN", "HUQ050", "HUQ030")

# Convert HUQ051 values to HUQ050 values.

access <- bind_rows(access_2017, access_2015, access_2013) %>%
  mutate(HUQ050 = case_when(
    HUQ051 >= 3 & HUQ051 <= 5 ~ 3,
    HUQ051 == 6 ~ 4,
    HUQ051 %in% c(7, 8) ~ 5,
    T ~ HUQ051
  )) %>%
  select("SEQN", "HUQ050", "HUQ030") %>%
  bind_rows(access_2011, access_2009, access_2007)

# Immunization
# IMQ011: Received Hepatitis A vaccine
# IMQ020: Received Hepatitis B 3 dose series
# IMQ040: Received HPV vaccine
# IMQ060: Received HPV vaccine (Females)

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/IMQ_J.XPT", tf <- tempfile(), mode = "wb")
vax_2017 <- read.xport(tf) %>% select("SEQN", "IMQ011", "IMQ020", "IMQ060")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/IMQ_I.XPT", tf <- tempfile(), mode = "wb")
vax_2015 <- read.xport(tf) %>% select("SEQN", "IMQ011", "IMQ020", "IMQ060")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/IMQ_H.XPT", tf <- tempfile(), mode = "wb")
vax_2013 <- read.xport(tf) %>% select("SEQN", "IMQ011", "IMQ020", "IMQ040")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/IMQ_G.XPT", tf <- tempfile(), mode = "wb")
vax_2011 <- read.xport(tf) %>% select("SEQN", "IMQ011", "IMQ020", "IMQ040")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/IMQ_F.XPT", tf <- tempfile(), mode = "wb")
vax_2009 <- read.xport(tf) %>% select("SEQN", "IMQ011", "IMQ020", "IMQ040")
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/IMQ_E.XPT", tf <- tempfile(), mode = "wb")
vax_2007 <- read.xport(tf) %>% select("SEQN", "IMQ011", "IMQ020", "IMQ040")

# Rename IMQ060 to IMQ040. This is ok because we remove males from the data later.

vax <- bind_rows(vax_2017, vax_2015, vax_2013) %>%
  mutate(IMQ040 = IMQ060) %>%
  select(-"IMQ060") %>%
  bind_rows(vax_2011, vax_2009, vax_2007)



# Merge all data together, leaving NAs where applicable.
# Divide weight by 6 because there are 6 survey cycles, as indicated by the NHANES analysis documentation.
# Rename variables.
# Map all refuse to answer (7, 77) and unknown values (9, 99) to NA.
# Transform time_in_us so that US-born women have a value based on their age.
# Create factors for categorical variables.
nhanes <- merge(demo, insurance, "SEQN", all = T) %>%
  merge(access, "SEQN", all = T) %>%
  merge(vax, "SEQN", all = T) %>%
  mutate(WTINT12YR = WTINT2YR / 6) %>%
  select(-"WTINT2YR") %>%
  rename_with(~ c("id", "gender", "birth_country", "age", "race", "poverty_ratio", "household_number", "time_in_us", "citizen", "interview_language", "SDMVPSU", "SDMVSTRA", "year", "education", "insurance", "healthcare_visits", "routine_place_healthcare", "hep_a_vax", "hep_b_vax", "hpv_vax", "WTINT12YR")) %>%
  mutate(birth_country = ifelse(birth_country %in% c(77, 99), NA, birth_country)) %>%
  mutate(time_in_us = ifelse(birth_country == 1, case_when(
    age < 1 ~ 1,
    age >= 1 & age < 5 ~ 2,
    age >= 5 & age < 10 ~ 3,
    age >= 10 & age < 15 ~ 4,
    age >= 15 & age < 20 ~ 5,
    age >= 20 & age < 30 ~ 6,
    age >= 30 & age < 40 ~ 7,
    age >= 40 & age < 50 ~ 8,
    age >= 50 ~ 9,
    T ~ time_in_us
  ), time_in_us)) %>%
  mutate(time_in_us = ifelse(time_in_us %in% c(77, 99), NA, time_in_us)) %>%
  mutate(citizen = ifelse(citizen %in% c(7, 9), NA, citizen)) %>%
  mutate(insurance = ifelse(insurance %in% c(7, 9), NA, insurance)) %>%
  mutate(healthcare_visits = ifelse(healthcare_visits %in% c(77, 99), NA, healthcare_visits)) %>%
  mutate(routine_place_healthcare = ifelse(routine_place_healthcare %in% c(7, 9), NA, routine_place_healthcare)) %>%
  mutate(hep_a_vax = ifelse(hep_a_vax %in% c(7, 9), NA, hep_a_vax)) %>%
  mutate(hep_b_vax = ifelse(hep_b_vax %in% c(7, 9), NA, hep_b_vax)) %>%
  mutate(hpv_vax = ifelse(hpv_vax %in% c(7, 9), NA, hpv_vax)) %>%
  mutate(birth_country = factor(birth_country, levels = c(1, 2), labels = c("US", "Others"))) %>%
  mutate(race = factor(race, levels = c(1, 2, 3, 4, 5), labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other Race - Including Multi-Racial"))) %>%
  mutate(time_in_us = factor(time_in_us, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9), labels = c("[0,1) years", "[1,5) years", "[5,10) years", "[10,15) years", "[15,20) years", "[20,30) years", "[30,40) years", "[40,50) years", "[50,INF) years"))) %>%
  mutate(citizen = factor(citizen, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  mutate(interview_language = factor(interview_language, levels = c(1, 2), labels = c("English", "Spanish"))) %>%
  mutate(education = factor(education, levels = c(1, 2, 3, 4, 5), labels = c("Less than 9th grade", "9-11th grade (Includes 12th grade with no diploma)", "High school graduate/GED or equivalent", "Some college or AA degree", "College graduate or above"))) %>%
  mutate(insurance = factor(insurance, levels = c(1, 2), labels = c("Yes", "No"))) %>%
  mutate(routine_place_healthcare = factor(routine_place_healthcare, levels = c(1, 2, 3), labels = c("Yes", "No", "More than one place"))) %>%
  mutate(hep_a_vax = factor(hep_a_vax, levels = c(1, 2, 3), labels = c("1 dose", "2 doses", "No"))) %>%
  mutate(hep_b_vax = factor(hep_b_vax, levels = c(1, 2, 3), labels = c("1 dose", "2 doses", "No"))) %>%
  mutate(hpv_vax = factor(hpv_vax, levels = c(1, 2), labels = c("Yes", "No")))

write_csv(nhanes, "./data/nhanes.csv")
write_rds(nhanes, "./data/nhanes.rds")
