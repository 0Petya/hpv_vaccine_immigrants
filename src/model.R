library(Amelia)
library(arsenal)
library(cowplot)
library(mitools)
library(survey)
library(tidyverse)

nhanes <- read_rds("./data/nhanes.rds")

# Missing data analysis.
proportion_of_missing_values <- data.frame(variable = colnames(nhanes), missingness = colSums(is.na(nhanes)) / nrow(nhanes)) %>%
  filter(missingness > 0) %>%
  ggplot(aes(x = reorder(variable, -missingness), y = missingness)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Proportion") + 
  ylab("Variable") +
  ggtitle("Proportion of Missing Values")
proportion_of_missing_values
ggsave("./figures/proportion_of_missing_values.png", proportion_of_missing_values)

missmap(nhanes)
png("./figures/missingness_map.png")
missmap(nhanes)
dev.off()



# Design for the survey, and subset with taking weights into account.
# Filter each demographic cycle by the ages that would've been eligibe for HPV vaccination at some point in their lives.
# Subset to only observations that include age (needed because we subset by age by cycle earlier).
# Subset to only females.
# Subset to observations that have a value for hpv_vax, because that is our dependent variable.
design <- svydesign(data = nhanes, id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTINT12YR, nest = T) %>%
  subset((year == 2007 & age >= 9 & age <= 28) |
           (year == 2009 & age >= 9 & age <= 30) |
           (year == 2011 & age >= 9 & age <= 32) |
           (year == 2013 & age >= 9 & age <= 34) |
           (year == 2015 & age >= 9 & age <= 36) |
           (year == 2017 & age >= 9 & age <= 38)) %>%
  subset(gender == 2) %>%
  subset(hpv_vax %in% c("Yes", "No")) %>%
  subset(!is.na(gender) & !is.na(birth_country) & !is.na(age) & !is.na(race) & !is.na(poverty_ratio) & !is.na(household_number) & !is.na(time_in_us) & !is.na(citizen) & !is.na(interview_language) & !is.na(education) & !is.na(insurance) & !is.na(healthcare_visits) & !is.na(routine_place_healthcare) & !is.na(hep_a_vax) & !is.na(hep_b_vax))



# Descriptive statistics
descriptive_table <- tableby(birth_country ~ ., weights = WTINT12YR, data = design$variables %>% select(birth_country, age, race, time_in_us, citizen, insurance, healthcare_visits, routine_place_healthcare, hpv_vax, WTINT12YR))
summary(descriptive_table)
setwd("./figures/")
write2pdf(descriptive_table, "./descriptive_statistics.pdf")
setwd("../")



# A listwise deletion, incorporating weights.
# Start with a crude model, with only birth_country.
crude_listwise_deletion_model <- svyglm(hpv_vax ~ birth_country, family = "binomial", design = design)
summary(crude_listwise_deletion_model)
crude_listwise_deletion_model_results <- exp(cbind(OR = coef(crude_listwise_deletion_model), confint(crude_listwise_deletion_model)))
crude_listwise_deletion_model_results
write.csv(as.data.frame(crude_listwise_deletion_model_results), "./data/crude_listwise_deletion_model_results.csv")



# Incorporate other covariates.
listwise_deletion_model <- svyglm(hpv_vax ~ birth_country + age + race + poverty_ratio + household_number + time_in_us + citizen + interview_language + education + insurance + healthcare_visits + routine_place_healthcare + hep_a_vax + hep_b_vax, family = "binomial", design = design)
summary(listwise_deletion_model)
listwise_deletion_model_results <- exp(cbind(OR = coef(listwise_deletion_model), confint(listwise_deletion_model))) %>%
  as.data.frame() %>%
  mutate(sig = ifelse(1 < `2.5 %` | 1 > `97.5 %`, "*", ""))
listwise_deletion_model_results
write.csv(listwise_deletion_model_results, "./data/listwise_deletion_model_results.csv")

listwise_deletion_model_pred <- data.frame(hpv_prob = listwise_deletion_model$fitted.values, hpv = design$variables$hpv_vax) %>%
  arrange(hpv_prob) %>%
  mutate(rank = 1:nrow(.))

ggplot(listwise_deletion_model_pred, aes(x = rank, y = hpv_prob)) +
  geom_point(aes(color = hpv), alpha = 1, shape = 19, stroke = 1) +
  xlab("Index") +
  ylab("Predicted probability of not getting the HPV vaccine")

ggsave("./figures/listwise_deletion_model.png")



# Examine insurance distribution among birth_country.
country_insurance_xtabs <- xtabs(~ birth_country + insurance, data = design$variables)
country_insurance_xtabs
capture.output(country_insurance_xtabs, file = "./data/country_insurance_xtabs.txt")

country_insurance_chisq <- chisq.test(design$variables$insurance, design$variables$birth_country)
country_insurance_chisq
capture.output(country_insurance_chisq, file = "./data/country_insurance_chisq.txt")



# Examine model without insurance.
without_insurance_listwise_deletion_model <- svyglm(hpv_vax ~ birth_country + age + race + poverty_ratio + household_number + time_in_us + citizen + interview_language + education + healthcare_visits + routine_place_healthcare + hep_a_vax + hep_b_vax, family = "binomial", design = design)
summary(without_insurance_listwise_deletion_model)
without_insurance_listwise_deletion_model_results <- exp(cbind(OR = coef(without_insurance_listwise_deletion_model), confint(without_insurance_listwise_deletion_model))) %>%
  as.data.frame() %>%
  mutate(sig = ifelse(1 < `2.5 %` | 1 > `97.5 %`, "*", ""))
without_insurance_listwise_deletion_model_results
write.csv(without_insurance_listwise_deletion_model_results, "./data/without_insurance_listwise_deletion_model_results.csv")
