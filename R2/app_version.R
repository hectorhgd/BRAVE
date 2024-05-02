######################################################################## #
# This file is part of the benefit-risk analysis of COVID-19 vaccines
# in the EU.
# 
# This R file includes:
#     - placeholders for the Rshiny App flow
# 
# Copyright: DSI, UHasselt, Belgium, 2022.
######################################################################## #

# clear workspace
rm(list=ls()[ls()!='args'])

# dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(paste0(dir));

# load packages
library(tidyverse)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Data format used in the Shiny app
# default or uploaded by the user
aggregated_covid_data     <- readRDS(file = 'data/aggregated_covid_data_default.rds')
age_specific_covid_data   <- readRDS(file = 'data/age_specific_covid_data_default.rds')
population_data           <- readRDS(file = 'data/population_data_default.rds')
age_specific_vaccine_data <- readRDS(file = 'data/age_specific_vaccine_data_default.rds')
variant_data              <- readRDS(file = 'data/variant_data_default.rds')


# load source code
# source('R2/benefit_model.R')
source('R/benefits.R')

# set dummy function for progress-bar
incProgress <- function(...){}

# load parameters
# parameters <- read.csv('data/default_parameters.csv',sep=';',header=T)
parameters <- read.csv('data/default_param_ve.csv',sep=';',header=T)

# set start and end date
start_date <- min(age_specific_vaccine_data$date)
end_date   <- max(age_specific_vaccine_data$date)

# set aggregation level (for development)
bool_by_country <- TRUE

# compute benefits
benefits <- Compute_benefits(age_specific_covid_data,
                             aggregated_covid_data,
                             age_specific_vaccine_data,
                             variant_data,
                             population_data,
                             start_date,
                             end_date,
                             parameters,
                             bool_by_country = bool_by_country)

# explore output
head(benefits$prevented_benefits_age_vaccine)

head(benefits$prevented_benefits_age_vaccine_dose_100k)

summary_stats <- benefits$prevented_benefits_age_vaccine_dose_100k %>%
                    group_by(country) %>%
                    summarise(cases = sum(prevented_cases),
                              hospitalization = sum(prevented_hospitalization),
                              icu = sum(prevented_icu),
                              mortality = sum(prevented_mortality))

p1 <- ggplot(summary_stats, aes(x = country, y = cases)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2 <- ggplot(summary_stats, aes(x = country, y = hospitalization)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p2b <- ggplot(summary_stats, aes(x = country, y = icu)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p3 <- ggplot(summary_stats, aes(x = country, y = mortality)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(p1)
print(p2)
print(p2b)
print(p3)


# aggregate
if(!bool_by_country){
  benefits$prevented_benefits_age_vaccine %>%
    group_by(scenario,vaccine,age_group,date) %>%
    summarise(prevented_cases = sum(prevented_cases,na.rm=T),
              sprevented_hospitalization = sum(prevented_hospitalization,na.rm=T),
              prevented_mortality = sum(prevented_mortality,na.rm=T))
  
  benefits$prevented_benefits_age_vaccine_dose_100k %>%
    group_by(scenario,vaccine,age_group) %>%
    summarise(prevented_cases = sum(prevented_cases,na.rm=T),
              sprevented_hospitalization = sum(prevented_hospitalization,na.rm=T),
              prevented_mortality = sum(prevented_mortality,na.rm=T))
}


# compare with previous output
if(!file.exists('results/benefits.rds')){
  saveRDS(benefits,'results/benefits.rds')
}
benefits_previous <- readRDS('results/benefits.rds')

# sum(benefits_previous$prevented_benefits_age_vaccine$prevented_cases,na.rm=T)
# sum(benefits$prevented_benefits_age_vaccine$prevented_cases,na.rm=T)
# sum(benefits_previous$prevented_benefits_age_vaccine_dose_100k$prevented_cases,na.rm=T)
# sum(benefits$prevented_benefits_age_vaccine_dose_100k$prevented_cases,na.rm=T)

library(useful)
for(i in 1:length(benefits)){
  bool_equal <- compare.list(benefits[[i]],benefits_previous[[i]])
  if(any(!bool_equal)){
    warning(paste0('Changes found in "', 
                  names(benefits_previous)[i]),
            '": [',
            paste0(names(benefits_previous[[i]][!bool_equal]),collapse=','),
            ']')
    warning()
  } else{
    print(paste('No changes found in:', names(benefits_previous)[i]))
  }
}


# debug code: create parameter variable to develop code and mock output
if(0==1){
  


parameters <- data.frame(
  vaccine = c('Comirnaty', 'Spikevax', 'Vaxzevria', 'Comirnaty', 'Spikevax', 'Vaxzevria','Comirnaty', 'Spikevax',
              'Comirnaty', 'Spikevax', 'Vaxzevria', 'Comirnaty', 'Spikevax', 'Vaxzevria','Comirnaty', 'Spikevax',
              'Comirnaty', 'Spikevax', 'Vaxzevria', 'Comirnaty', 'Spikevax', 'Vaxzevria','Comirnaty', 'Spikevax'),
  dose = c(1, 1, 1, 2, 2, 2, 3, 3,
           1, 1, 1, 2, 2, 2, 3, 3,
           1, 1, 1, 2, 2, 2, 3, 3),
  variant = c('Original', 'Original', 'Original', 'Original', 'Original', 'Original', 'Original', 'Original',
              'Alpha', 'Alpha', 'Alpha', 'Alpha', 'Alpha', 'Alpha', 'Alpha', 'Alpha',
              'Delta', 'Delta', 'Delta', 'Delta', 'Delta', 'Delta', 'Delta', 'Delta'),
  against = c('cases'),
  efficiency = c(0.46, 0.46, 0.47, 0.92, 0.92, 0.72, 0.92, 0.92,
                 0.44, 0.44, 0.36, 0.89, 0.89, 0.60, 0.89, 0.89,
                 0.36, 0.36, 0.30, 0.87, 0.87, 0.47, 0.87, 0.87),
  waning_delay = rep(180, 24),
  
  protection_delay = rep(21,24)
)

# TODO: Provide default values for the hospitalization and mortality parameters
parameters <- rbind(
  parameters,
  parameters %>% mutate(against = 'hospitalization'),
  parameters %>% mutate(against = 'icu'),
  parameters %>% mutate(against = 'mortality')
)

# Create three scenarios with all the same parameters to test methodology
parameters <- rbind(
  parameters %>% mutate(scenario = 'default')#,
  #parameters %>% mutate(scenario = 'testA')
)

# # temporary: store parameter values
# write.table(parameters, 
#             file = 'data/default_parameters.csv', 
#             row.names = FALSE, 
#             col.names = TRUE, 
#             sep = ';')



#### Construction of mock data ####
## Will be deleted when inserting the code in the app

benefits_old <- readRDS('results/default_output_benefits.rds')

prevented_benefits_age_vaccine <- map_df(benefits_old, function(country_data) {
  
  date <- country_data$date
  
  # All adeno --> Vaxzevria for the mock data
  prevented_cases_adeno <- reshape2::melt(country_data$pred_prev_cases_adeno) %>%
    rename(scenario = Var1,
           date = Var2,
           age_group = Var3,
           prevented_cases = value) %>%
    mutate(age_group = factor(age_group, 1:8, c('0-19', '20-29', '30-39', '40-49',
                                                '50-59', '60-69', '70-79', '80+')),
           date = (!!date)[date]) %>%
    mutate(vaccine = 'Vaxzevria')
  
  # Equal split Comirnaty and Spikevax for the Mock data
  prevented_cases_rna <- reshape2::melt(country_data$pred_prev_cases_rna) %>%
    rename(scenario = Var1,
           date = Var2,
           age_group = Var3,
           prevented_cases = value) %>%
    mutate(age_group = factor(age_group, 1:8, c('0-19', '20-29', '30-39', '40-49',
                                                '50-59', '60-69', '70-79', '80+')),
           date = (!!date)[date],
           prevented_cases = 0.5*prevented_cases) %>%
    mutate(vaccine = 'Comirnaty')
  
  # For the mock data prevented hospitalization and mortality is set relative to prevented cases
  prevented <- rbind(prevented_cases_adeno,
                     prevented_cases_rna,
                     prevented_cases_rna %>% mutate(vaccine = 'Spikevax')) %>%
    mutate(prevented_hospitalization = prevented_cases * 0.1,
           prevented_mortality = prevented_cases * 0.02) %>%
    mutate(country = country_data$country)
    
  return(prevented)
})

# Include the dose here as well 
prevented_benefits_age_vaccine_dose_100k <- map_df(benefits_old, function(country_data) {
  
  date <- country_data$date
  
  # All adeno --> Vaxzevria for the mock data
  prevented_cases_adeno <- reshape2::melt(country_data$pred_prev_cases_adeno_100k) %>%
    rename(scenario = Var1,
           age_group = Var2,
           prevented_cases = value) %>%
    mutate(age_group = factor(age_group, 1:8, c('0-19', '20-29', '30-39', '40-49',
                                                '50-59', '60-69', '70-79', '80+'))) %>%
    mutate(vaccine = 'Vaxzevria')
  
  # Equal split Comirnaty and Spikevax for the Mock data
  prevented_cases_rna <- reshape2::melt(country_data$pred_prev_cases_mrna_100k) %>%
    rename(scenario = Var1,
           age_group = Var2,
           prevented_cases = value) %>%
    mutate(age_group = factor(age_group, 1:8, c('0-19', '20-29', '30-39', '40-49',
                                                '50-59', '60-69', '70-79', '80+')),
           prevented_cases = 0.5*prevented_cases) %>%
    mutate(vaccine = 'Comirnaty')
  
  # For the mock data prevented hospitalization and mortality is set relative to prevented cases
  # For the mock data the first dose provides 70% of the full protection
  prevented <- rbind(prevented_cases_adeno %>% mutate(dose = 1, prevented_cases = 0.7 * prevented_cases),
                     prevented_cases_adeno %>% mutate(dose = 2),
                     prevented_cases_rna %>% mutate(dose = 1, prevented_cases = 0.7 * prevented_cases),
                     prevented_cases_rna %>% mutate(dose = 2),
                     prevented_cases_rna %>% mutate(dose = 1, prevented_cases = 0.7 * prevented_cases, vaccine = 'Spikevax'),
                     prevented_cases_rna %>% mutate(dose = 2, vaccine = 'Spikevax')) %>%
    mutate(prevented_hospitalization = prevented_cases * 0.1,
           prevented_mortality = prevented_cases * 0.02) %>%
    mutate(country = country_data$country)
  
  return(prevented)
})

output_benefits <- list(
  prevented_benefits_age_vaccine = prevented_benefits_age_vaccine,
  prevented_benefits_age_vaccine_dose_100k = prevented_benefits_age_vaccine_dose_100k
)

names(output_benefits)
names(output_benefits$prevented_benefits_age_vaccine)
names(output_benefits$prevented_benefits_age_vaccine_dose_100k)

saveRDS('output_benefits', 
        file = 'results/output_benefits_mock_data.rds')
}




