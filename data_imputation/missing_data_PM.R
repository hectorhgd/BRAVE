######################################################################## #
# This file is part of the benefit-risk analysis of COVID-19 vaccines
# in the EU.
# 
# This R file includes:
#     - to pre-process reference data
#     - to store rds or Rdata file(s) with input data for the App.
# 
# Copyright: DSI, UHasselt, Belgium, 2022.
######################################################################## #

# clear workspace
rm(list=ls()[ls()!='args'])

# load packages
library(data.table)
library(tidyverse)
library(zoo)  # for "rollmean"

######################################################################## #
# LOAD DATA ----
vaccine_dat <- read.table('raw_vaccine_dat.csv',sep=',',header=T)
country_list <- read.table('raw_country_list.csv',sep=',',header=T)
population_data<- read.table('raw_population_data.csv',sep=',',header=T)
variant_data<- read.table('raw_variant_data.csv',sep=',',header=T)
aggregated_covid_data<- read.table('raw_aggregated_covid_data.csv',sep=',',header=T)
age_specific_covid_data<- read.table('raw_age_specific_covid_data.csv',sep=',',header=T)
age_specific_aggregated<- read.table('raw_age_specific_aggregated.csv',sep=',',header=T)


######################################################################## #
## POPULATION DATA: Liechtenstein ----
population_data <- rbind(population_data,
                         population_data %>%
                           filter(country=='Switzerland') %>%
                           mutate(population = population / sum(population) * 39137) %>%
                           mutate(country = 'Liechtenstein')) %>%
  filter(country %in% country_list$country_name)


######################################################################## #
## RESCALE VACCINE UPTAKE: default ----
# convert default uptake age groups into the PM age groups
age_specific_vaccine_data <- vaccine_dat %>%
  filter(TargetGroup %in% c('Age<18', 'Age18_24', 'Age25_49', 'Age50_59', 'Age60_69', 'Age70_79', 'Age80+')) %>%
  left_join(country_list %>% select(country_name, abbreviation),
            by = c('ReportingCountry' = 'abbreviation')) 

translate_age <- matrix(
  c(1, 2/7, 0, 0, 0, 0, 0,
    0, 5/7, 5/25, 0, 0, 0, 0,
    0, 0, 10/25, 0, 0, 0, 0,
    0, 0, 10/25, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0,
    0, 0, 0, 0, 1, 0, 0,
    0, 0, 0, 0, 0, 1, 0,
    0, 0, 0, 0, 0, 0, 1),
  byrow = TRUE,
  ncol = 7
) 
colnames(translate_age) <- c('Age<18', "Age18_24", "Age25_49", "Age50_59", "Age60_69", "Age70_79", "Age80+")
rownames(translate_age) <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')

translate_age <- translate_age %>% 
  melt %>%
  rename(age_group = Var1,
         TargetGroup = Var2,
         weight = value)

age_specific_vaccine_data <- age_specific_vaccine_data %>% 
  left_join(translate_age) %>%
  group_by(YearWeekISO, country_name, age_group, Vaccine) %>%
  summarise(first_dose = sum(weight * FirstDose),
            second_dose = sum(weight * SecondDose),
            booster_dose = sum(weight * DoseAdditional1)) %>%
  ungroup()

######################################################################## #
## RESCALE VACCINE UPTAKE: GERMANY: only 1_Age<60 and 1_Age>60 ----  
age_specific_vaccine_data_germany <- vaccine_dat %>%
  filter(ReportingCountry == 'DE',
         TargetGroup %in% c('1_Age<60', '1_Age60+')) %>%
  left_join(country_list %>% select(country_name, abbreviation),
            by = c('ReportingCountry' = 'abbreviation')) 

translate_age_germany <- matrix(
  c(1/5, 0,
    1/5, 0,
    1/5, 0, 
    1/5, 0,
    1/5, 0,
    0, 1/3,
    0, 1/3,
    0, 1/3),
  byrow = TRUE,
  ncol = 2
) 
colnames(translate_age_germany) <- c('1_Age<60', "1_Age60+")
rownames(translate_age_germany) <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')

age_specific_vaccine_data_germany <- age_specific_vaccine_data_germany %>% 
  left_join(translate_age_germany %>% melt %>%
              rename(age_group = Var1,
                     TargetGroup = Var2,
                     weight = value) ) %>%
  group_by(YearWeekISO, country_name, age_group, Vaccine) %>%
  summarise(first_dose = sum(weight * FirstDose),
            second_dose = sum(weight * SecondDose),
            booster_dose = sum(weight * DoseAdditional1)) %>%
  ungroup()


age_specific_vaccine_data <- rbind(
  age_specific_vaccine_data,
  age_specific_vaccine_data_germany
)


######################################################################## #
## RESCALE VACCINE UPTAKE:  help function ----
impute_uptake_adult_ages <- function(vaccine_dat,population_data,country_ISO, country_name){
  
  age_specific_vaccine_data_country <- vaccine_dat %>%
    filter(ReportingCountry == country_ISO) %>%
    left_join(country_list %>% select(country_name, abbreviation),
              by = c('ReportingCountry' = 'abbreviation')) 
  
  translate_age_country <- as.matrix(population_data %>%
                                       filter(country == country_name) %>%
                                       mutate(ALL = population * c(0,rep(1,7))) %>%
                                       summarise('Age<18' = c(1,rep(0,7)),
                                                 ALL = ALL / sum(ALL)))
  
  colnames(translate_age_country) <- c('Age<18', "ALL")
  rownames(translate_age_country) <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+')
  
  age_specific_vaccine_data_country <- age_specific_vaccine_data_country %>% 
    left_join(translate_age_country %>% melt %>%
                rename(age_group = Var1,
                       TargetGroup = Var2,
                       weight = value) ) %>%
    group_by(YearWeekISO, country_name, age_group, Vaccine) %>%
    summarise(first_dose = sum(weight * FirstDose),
              second_dose = sum(weight * SecondDose),
              booster_dose = sum(weight * DoseAdditional1)) %>%
    ungroup()
  
  return(age_specific_vaccine_data_country)
}

######################################################################## #
## RESCALE VACCINE UPTAKE:  Netherlands with only <18 and Adults ----
age_specific_vaccine_data_netherlands <- impute_uptake_adult_ages(vaccine_dat,population_data,'NL','Netherlands')
age_specific_vaccine_data <- rbind(
  age_specific_vaccine_data[age_specific_vaccine_data$country_name != 'Netherlands',],
  age_specific_vaccine_data_netherlands
)

######################################################################## #
## RESCALE VACCINE UPTAKE:  Liechtenstein with only <18 and Adults ----
age_specific_vaccine_data_liechtenstein <- impute_uptake_adult_ages(vaccine_dat,population_data,'LI','Liechtenstein')

age_specific_vaccine_data <- rbind(
  age_specific_vaccine_data[age_specific_vaccine_data$country_name != 'Liechtenstein',],
  age_specific_vaccine_data_liechtenstein
)

######################################################################## #
## IMPUTE VACCINE BRAND INFO ----
# filter vaccine data
age_specific_vaccine_data <- age_specific_vaccine_data %>%
  filter(Vaccine %in% c('COM', 'UNK', 'MOD', 'AZ', 'JANSS'))

# replace UNK by distribution vaccines used in other countries
vaccine_distribution <- age_specific_vaccine_data %>%
  filter(Vaccine != 'UNK') %>%
  group_by(YearWeekISO, age_group, Vaccine) %>%
  summarise(count = sum(first_dose + second_dose + booster_dose)) %>%
  group_by(YearWeekISO, age_group) %>%
  mutate(total = sum(count)) %>%
  group_by(YearWeekISO, age_group, Vaccine) %>%
  summarise(percentage = count / total[1]) %>%
  ungroup() %>%
  mutate(percentage = replace_na(percentage, 0)) 

age_specific_vaccine_data <- rbind(
  age_specific_vaccine_data %>% filter(Vaccine != 'UNK'),
  age_specific_vaccine_data %>%
    filter(Vaccine == 'UNK') %>%
    select(-Vaccine) %>%
    left_join(vaccine_distribution) %>%
    mutate(first_dose = first_dose * percentage,
           second_dose = second_dose * percentage,
           booster_dose = booster_dose * percentage) %>%
    select(-percentage))

week_date <- data.frame(date = seq(as.Date('2020-01-01'), as.Date('2022-01-01'), by = 'day')) %>%
  mutate(YearWeekISO = format(date, '%Y-W%U'))

age_specific_vaccine_data <- age_specific_vaccine_data %>%
  left_join(week_date) %>%
  pivot_longer(c(first_dose, second_dose,booster_dose),
               values_to = 'count',
               names_to = 'dose') %>%
  mutate(count = count / 7) %>%
  mutate(dose = gsub('first_dose', 1, dose)) %>%
  mutate(dose = gsub('second_dose', 2, dose)) %>%
  mutate(dose = gsub('booster_dose', 3, dose)) %>%
  mutate(dose = as.numeric(dose)) %>%
  select(-YearWeekISO) %>%
  rename(vaccine = Vaccine,
         country = country_name)

age_specific_vaccine_data <- age_specific_vaccine_data %>%
  filter(!is.na(date))

age_specific_vaccine_data$vaccine <-
  factor(age_specific_vaccine_data$vaccine,
         c('COM', 'MOD', 'AZ', 'JANSS'),
         c('Comirnaty', 'Spikevax', 'Vaxzevria', 'Janssen'))

# Exclude Janssen vaccine from the benefit-risk analysis
age_specific_vaccine_data <- age_specific_vaccine_data %>%
  filter(vaccine != 'Janssen') %>%
  mutate(vaccine = droplevels(vaccine))

# aggregate by age group
names(age_specific_vaccine_data)
age_specific_vaccine_data <- age_specific_vaccine_data %>%
  group_by(country,age_group,vaccine,date,dose) %>%
  summarise(count = sum(count)) %>%
  ungroup()

######################################################################## #
## VARIANTS OF CONCERN ----
predict_voc_prevalence <- function(successes, totals, date,start_date,end_date){
  range <- seq(start_date, end_date, by = 'day')
  day <- match(as.Date(date), range)
  
  
  require(mgcv)
  gam_fit_alpha = gam(cbind(successes, totals - successes)  ~ s(day), family = binomial)
  predict_prop_voc = predict(gam_fit_alpha, newdata = list(day = 1:length(range)), type = "response", se.fit = TRUE)
  
  return(data.frame(date = range,
                    proportion = predict_prop_voc$fit))
}

variant_data <- variant_data %>%
  filter(variant %in% c('Alpha', 'Delta'))

# set start and end date
start_date  <- as.Date('2020-01-01')
end_date    <- as.Date('2022-01-01')

# predict prevalence per VOC and country
variant_proportion <- variant_data %>%
  group_by(variant, location) %>%
  nest %>%
  ungroup() %>%
  mutate(data = map(data, function(data){
    predict_voc_prevalence(data$num_sequences, data$num_sequences_total, data$date,start_date,end_date)
  })) %>%
  unnest(cols = data)

# aggregate and summarise VOC data
variant_data <- variant_proportion %>%
  complete(location, variant, date, fill = list(proportion = 0)) %>%
  pivot_wider(names_from = variant, values_from = proportion) %>%
  mutate(denom = max(Alpha + Delta, 1),
         Alpha = Alpha / denom,
         Delta = Delta / denom,
         Original = 1 - Alpha - Delta) %>%
  select(-denom) %>%
  pivot_longer(c(Alpha, Delta, Original),
               values_to = 'proportion',
               names_to = 'variant') %>%
  rename(country = location)

######################################################################## #
## MISSING DATA: HOSPITAL & ICU ADMISSIONS OVER TIME ----
hosp_imputed <- aggregated_covid_data %>% 
  filter(!is.na(hospitalization),
         cases > 0) %>% 
  group_by(date) %>% 
  summarise(hospital_ratio = median(hospitalization / cases,na.rm=T),
            icu_ratio = median(icu / cases,na.rm=T)) %>%
  mutate(hospital_ratio_rollmedian = rollmedian(hospital_ratio,k=7,align='center',fill=NA),
         icu_ratio_rollmedian = rollmedian(icu_ratio,k=7,align='center',fill=NA))


# # check which aggregated data is available
# aggregated_covid_data %>% group_by(country) %>% summarise(cases = !all(is.na(cases)),
#                                                                 hosp = !all(is.na(hospitalization)),
#                                                                 icu = !all(is.na(icu)),
#                                                                 mortality = !all(is.na(mortality)))

# use time-specific hospital and icu rate if data is missing
aggregated_covid_data <- (aggregated_covid_data %>% 
                            right_join(hosp_imputed))  %>%
                          mutate(hospitalization = replace(hospitalization,is.na(hospitalization),(cases * hospital_ratio)[is.na(hospitalization)]),
                                 icu = replace(icu,is.na(icu),(cases * icu_ratio)[is.na(icu)])) %>%
                          select(-hospital_ratio,-icu_ratio)


######################################################################## #
## MISSING DATA: AGE-DISTRIBUTION HOSPITAL & ICU ADMISSIONS ----
# make sure all country and age combinations are present
age_specific_covid_data <- age_specific_covid_data %>% 
  filter(country != 'aggregated') %>% 
  full_join(expand_grid(country=country_list$country_name,
                        age_group= unique(age_specific_covid_data$age_group)))

# # ## check which age-specific data is available
# age_specific_covid_data %>% group_by(country) %>% summarise(cases = !all(is.na(cases)),
#                                                                 hosp = !all(is.na(hospitalization)),
#                                                                 icu = !all(is.na(icu)),
#                                                                 mortality = !all(is.na(mortality)))

# use aggregated statistics if country-data is missing
age_specific_covid_data <- age_specific_covid_data %>%
  group_by(country) %>%
  mutate(cases = if_else(is.na(cases),age_specific_aggregated$cases,cases),
         hospitalization = if_else(is.na(hospitalization),age_specific_aggregated$hospitalization,hospitalization),
         icu = if_else(is.na(icu),age_specific_aggregated$icu,icu),
         mortality = if_else(is.na(mortality),age_specific_aggregated$mortality,mortality))


######################################################################## #
## ADDITIONAL CORRECTIONS ----
# explicitly set NA's to 0
aggregated_covid_data$cases[is.na(aggregated_covid_data$cases)] <- 0
aggregated_covid_data$hospitalization[is.na(aggregated_covid_data$hospitalization)] <- 0
aggregated_covid_data$icu[is.na(aggregated_covid_data$icu)] <- 0
aggregated_covid_data$mortality[is.na(aggregated_covid_data$mortality)] <- 0


######################################################################## #
## SAVE DIFFERENT DATA SETS ----
saveRDS(aggregated_covid_data,file = 'aggregated_covid_data.rds')
saveRDS(age_specific_covid_data,file = 'age_specific_covid_data.rds')
saveRDS(population_data,file = 'population_data.rds')
saveRDS(age_specific_vaccine_data,file = 'age_specific_vaccine_data.rds')
saveRDS(variant_data,file = 'variant_data.rds')


