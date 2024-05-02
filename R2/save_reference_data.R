######################################################################## #
# This file is part of the benefit-risk analysis of COVID-19 vaccines
# in the EU.
# 
# This R file includes:
#     - to retreive reference data
#     - to pre-process reference data
#     - to store rds or Rdata file(s) with input data for the App.
# 
# Copyright: DSI, UHasselt, Belgium, 2022.
######################################################################## #

# clear workspace
rm(list=ls()[ls()!='args'])

# # Setting the working directory to the parent directory of this file
# dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(paste0(dir));

# load packages
library(data.table)
library(tidyverse)
library(countrycode)
library(wpp2019)
library(zoo)

## SETTINGS ----

# force_download ?
force_download <- FALSE

## HELP FUNCTIONS ----
download_ref_file <- function(ref_url,data_dir = 'data_raw/local',col_names = NULL,force_download = FALSE){
  
  ref_file_name   <- file.path( 
                               data_dir,
                               paste(format(Sys.Date(),format='%Y%m%d'),   # date
                                     basename(dirname(dirname(ref_url))),   # source
                                     basename(ref_url),
                                     sep='_')
  )                    # file name
  if(!file.exists(ref_file_name) || force_download){
    if(!dir.exists(data_dir)){dir.create(data_dir,recursive = T)}
    
    if(is.null(col_names)){
      col_names <- names(read_csv(ref_url,n_max=1,show_col_types = F))
    }
    write.csv(read_csv(ref_url,col_select = all_of(col_names),show_col_types = F),
              file = ref_file_name,
              row.names = F)
    print(ref_file_name)
  } 
  
  return(ref_file_name)
}

##' Get age-specific population data according to the World Population Prospects 2019 edition
##'
##' This uses data from the \code{wpp2019} package but combines male and female,
##' and converts age groups to lower age limits
##' @return data frame of age-specific population data
##' @import wpp2017, countrycode
##' @importFrom data.table data.table dcast melt
##' @importFrom countrycode countrycode
##' @param countries countries, will return all if not given
##' @param years years, will return all if not given
##' @examples
##' wpp_age("Italy", c(1990, 2000))
##' @export
wpp_age <- function(countries, years)
{
  
  data(popF, package = "wpp2019", envir = environment())
  data(popM, package = "wpp2019", envir = environment())
  
  popM <- data.table(popM)
  popF <- data.table(popF)
  
  popM <- popM[, sex := "male"]
  popF <- popF[, sex := "female"]
  
  pop <- rbind(popM, popF)
  
  # change column names to remain compatible with the 2015 package
  pop[,country:=name]
  pop[,name:=NULL]
  
  bool_liechtenstein <- countries %in% c("Liechtenstein", "LIE")
  if(any(bool_liechtenstein)){
    liechtenstein_sub <- 'Switzerland'
    countries[bool_liechtenstein] <- liechtenstein_sub
  }
  
  if (!missing(countries))
  {
    ## match by UN country code
    pop <- suppressWarnings(pop[country_code %in% countrycode(countries, "country.name", "iso3n")])
  }
  
  if (nrow(pop) > 0) {
    pop <- melt(pop, id.vars = c("country", "country_code", "age", "sex"), variable.name = "year")
    pop <- data.table(dcast(pop, country + country_code + age + year ~ sex, value.var = "value"))
    
    pop[, year := as.integer(as.character(year))]
    
    if (!missing(years))
    {
      if (any(pop$year %in% years))
      {
        pop <- pop[year %in% years]
      } else {
        available.years <- unique(pop$year)
        nearest.year <- available.years[which.min(abs(available.years - years))]
        warning("Don't have population data available for ", years, ". Will return nearest year (", nearest.year, ").")
        pop <- pop[year %in% nearest.year]
      }
    }
    
    pop <- pop[, lower.age.limit := as.integer(sub("[-+].*$", "", age))]
    pop <- pop[, list(country, lower.age.limit, year, population = (female + male) * 1000)]
    
    # aggregate by age groups
    pop$age.group <- cut(pop$lower.age.limit,c(0,20,30,40,50,60,70,80,100),include.lowest = T, right=F)
    pop <- aggregate (population ~ age.group + year + country, data = pop, sum)
    
    if(any(bool_liechtenstein)){
      ind_lie_sub <- which(pop$country == liechtenstein_sub)
      pop$country[ind_lie_sub] <- 'Liechtenstein'
      # source: https://www.worldometers.info/world-population/liechtenstein-population/
      pop$population[ind_lie_sub] <- 38128 * (pop$population / sum(pop$population))  # 2020 population
      message("Inverted population structure for Liechtenstein based on age-structre of Switzerland")
    }
  }
  
  return(as.data.frame(pop))
}

predict_voc_prevalence <- function(successes, totals, date,start_date,end_date){
  range <- seq(start_date, end_date, by = 'day')
  day <- match(date, range)
  
  
  require(mgcv)
  gam_fit_alpha = gam(cbind(successes, totals - successes)  ~ s(day), family = binomial)
  predict_prop_voc = predict(gam_fit_alpha, newdata = list(day = 1:length(range)), type = "response", se.fit = TRUE)
  
  return(data.frame(date = range,
                    proportion = predict_prop_voc$fit))
}


# LOAD DATA ----
# load country list
country_list = read.csv("data_raw/country_list_eu.csv", 
                        sep = ";", 
                        fileEncoding = "UTF-8-BOM")
end_date <- Sys.Date()

# REPORTED CASES + HOSPITAL AND ICU ADMISSONS (OWID) ----
owid_col_names <- c("iso_code","location","date",
                    "new_cases","total_cases",
                    "weekly_hosp_admissions",
                    "weekly_icu_admissions",
                    "hosp_patients",
                    "icu_patients",
                    "new_deaths")

path = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data"
file = "owid-covid-data.csv"
url_file = paste0(path, "/", file)

full_owid_dat = read_csv(download_ref_file(ref_url = url_file, 
                                           col_names = owid_col_names,
                                           force_download = force_download), 
                         show_col_types = FALSE)
full_owid_dat$new_cases[full_owid_dat$new_cases<0] <- 0

# age-specific data
full_case_dat <- readRDS("data_raw/local/case_dataset2_selection.rds")
covid_dat <- read.table("data_raw/local/covid_dataset.csv", 
                       header = T, sep = ";", 
                       fileEncoding = "UTF-8-BOM")

## VARIANTS OF CONCERN (OWID) ----
# note: this data is not available anymore, no updates possible
# note: switch to ECDC data is recommended
variant_data <-  read_csv(file.path('data_raw/20211217_covid-variants.csv'), 
                          show_col_types = FALSE)


## VACCINE UPTAKE (ECDC) ----
url <- "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv"
vaccine_col <- c("YearWeekISO","ReportingCountry","FirstDose","SecondDose","DoseAdditional1","TargetGroup","Vaccine","Region")

vaccine_dat <- read_csv(download_ref_file(url, 
                                          col_names = vaccine_col,
                                          force_download = FALSE),
                        show_col_types = FALSE)

# select nationwide statistics
vaccine_dat <- vaccine_dat %>% 
  filter(Region == ReportingCountry)

unique(vaccine_dat$TargetGroup)
country_age0_4  <- unique(vaccine_dat %>% filter(TargetGroup %in% c("Age0_4")) %>% pull(ReportingCountry))
country_age0_18 <- unique(vaccine_dat %>% filter(TargetGroup %in% c("Age<18")) %>% pull(ReportingCountry))
country_age0_4  <- country_age0_4[!(country_age0_4 %in% country_age0_18)]

# merge <18y age groups (if not given by Age<18)
vaccine_dat$TargetGroup[!(vaccine_dat$ReportingCountry %in% country_age0_18) & vaccine_dat$TargetGroup %in% c("Age0_4","Age5_9","Age10_14","Age15_17")] <- "Age<18"
# 


message("Vaccine uptake dataset successfully loaded")

## DATA SELECTION: COUNTRIES ----
data_europe <- full_owid_dat %>%
  filter(location %in% country_list$country_name)

length(unique(data_europe$iso_code))

covid_dat <- covid_dat %>%
  mutate(Country = fct_recode(Country,
                              'Czechia' = 'Czech Republic')) %>%
  filter(Country %in% country_list$country_name) 


full_case_dat <- full_case_dat %>%
  left_join(country_list %>% select(country_name, abbreviation),
            by = c('ReportingCountry' = 'abbreviation')) %>%
  filter(!is.na(Age),
         year(DateUsedForStatisticsISO) == 2021)

full_case_dat$age_group <- factor(full_case_dat$agegp30, 
                                  labels = c('0-19', '0-19', '20-29', 
                                             '30-39', '40-49', '50-59', 
                                             '60-69', '70-79', '80+'))

variant_data <- variant_data %>% 
  filter(location %in% country_list$country_name)


# SET FINAL TABLES: aggregated_covid_data ----
aggregated_covid_data <- full_owid_dat %>%
  filter(location %in% country_list$country_name) %>%
  rename(country = location,
         cases = new_cases,
         hospitalization = weekly_hosp_admissions,
         icu = weekly_icu_admissions,
         mortality = new_deaths) %>%
  mutate(hospitalization = hospitalization / 7,
         icu = icu / 7) %>%
  select(country, date, cases, hospitalization, icu, mortality)
  
# some hospital and ICU data are provided once per week
# hence, select hospital and icu data (not NA)
weekly_covid_data <- aggregated_covid_data %>%
  filter(!is.na(hospitalization) | !is.na(icu)) %>%
  mutate(date_week = date)  %>%
  select(-cases,-mortality,-date)

# merge with all dates, and copy weekly figures if needed (= impute)
date = seq(as.Date('2020-01-05'), as.Date('2022-02-05'), by = 'day')
date_week = seq(as.Date('2020-01-05'), as.Date('2022-02-05'), by = 'week')
date_all <- data.frame(date = date,
                        date_week = rep(date_week,each = 7))

daily_covid_data <- weekly_covid_data %>% 
  right_join(date_all) %>%
  arrange(country,date) %>%
  select(-date_week)

# select all imputed data
daily_covid_data <- daily_covid_data %>% 
  anti_join(weekly_covid_data %>% 
              select(date = date_week,country))

# join the original and imputed hospital and icu data
daily_covid_data <- rbind(daily_covid_data,
                          weekly_covid_data %>%
                            mutate(date=date_week) %>%
                            select(-date_week)) %>%
                    arrange(country,date)

# join hospital and icu data with other burden of disease data
aggregated_covid_data <- aggregated_covid_data %>% 
  select(-hospitalization, -icu) %>%
  left_join(daily_covid_data)

# SET FINAL TABLES: age_specific_covid_data ----

covid_pct <- rbind(covid_dat,   # include additional "country" level to obtain overall statistisc
                   covid_dat%>%mutate(Country = 'aggregated')) %>%
  filter(Agegroup != 'Unknown',
         Agegroup != 'TOTAL') %>%
  group_by(Country,Agegroup) %>%
  summarise(COVID.19.hospitalisation = sum(COVID.19.hospitalisation, na.rm = TRUE),
            COVID.19.ICU.admission   = sum(COVID.19.ICU.admission,na.rm = TRUE),
            COVID.19.deaths = sum(COVID.19.deaths, na.rm = TRUE)) %>%
  group_by(Country) %>%
  mutate(hospitalization_freq = COVID.19.hospitalisation / sum(COVID.19.hospitalisation),
         icu_freq   = COVID.19.ICU.admission / sum(COVID.19.ICU.admission),
         mortality_freq = COVID.19.deaths / sum(COVID.19.deaths)) %>%
  ungroup() %>%
  mutate(Agegroup = factor(Agegroup, labels = c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+'))) %>%
  rename(country = Country,
         age_group = Agegroup) %>%
  select(-c(COVID.19.hospitalisation,COVID.19.ICU.admission,COVID.19.deaths))


age_specific_covid_data <- rbind(full_case_dat,
                                 full_case_dat %>% mutate(country_name = 'aggregated')) %>%
  rename(country = country_name,
         date = DateUsedForStatisticsISO) %>%
  group_by(country, age_group) %>%
  summarise(cases = n()) %>%
  mutate(cases_freq = cases / sum(cases)) %>%
  select(-c(cases)) %>%
  ungroup()

age_specific_covid_data <- age_specific_covid_data %>%
  full_join(covid_pct)

# rename
age_specific_covid_data <- age_specific_covid_data %>%
  rename(cases = cases_freq,
         hospitalization = hospitalization_freq,
         icu = icu_freq,
         mortality = mortality_freq)


# separate aggregated from country-specific statistics
age_specific_aggregated <- age_specific_covid_data %>% filter(country == 'aggregated')
age_specific_covid_data <- age_specific_covid_data %>% filter(country != 'aggregated')

## SET FINAL TABLE: population_data ----
population_data <- wpp_age(c(country_list$country_name[country_list$country_name != 'Liechtenstein'],"Switzerland"), 2020) %>%
                        # rbind(wpp_age(country_list$country_name[country_list$country_name != 'Liechtenstein'], 2020),
                        #  wpp_age("Switzerland", 2020) %>%
                        #    mutate(population = population / sum(population) * 39137) %>%
                        #    mutate(country = 'Liechtenstein')) %>%
  mutate(age.group = factor(age.group,
                            label = c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80+'))) %>%
  select(country, age.group, population) %>%
  rename(age_group = age.group)




write.table(vaccine_dat,file='data_imputation/raw_vaccine_dat.csv',sep=',',row.names = F)
write.table(country_list,file='data_imputation/raw_country_list.csv',sep=',',row.names = F)
write.table(population_data,file='data_imputation/raw_population_data.csv',sep=',',row.names = F)
write.table(variant_data,file='data_imputation/raw_variant_data.csv',sep=',',row.names = F)
write.table(aggregated_covid_data,file='data_imputation/raw_aggregated_covid_data.csv',sep=',',row.names = F)
write.table(age_specific_covid_data,file='data_imputation/raw_age_specific_covid_data.csv',sep=',',row.names = F)
write.table(age_specific_aggregated,file='data_imputation/raw_age_specific_aggregated.csv',sep=',',row.names = F)


## START ----
vaccine_dat <- read.table('data_imputation/raw_vaccine_dat.csv',sep=',',header=T)
country_list <- read.table('data_imputation/raw_country_list.csv',sep=',',header=T)
population_data<- read.table('data_imputation/raw_population_data.csv',sep=',',header=T)
#variant_data<- read.table('data_imputation/variant_data.csv',sep=',',header=T)
aggregated_covid_data<- read.table('data_imputation/raw_aggregated_covid_data.csv',sep=',',header=T)
age_specific_covid_data<- read.table('data_imputation/raw_age_specific_covid_data.csv',sep=',',header=T)
age_specific_aggregated<- read.table('data_imputation/raw_age_specific_aggregated.csv',sep=',',header=T)

population_data

population_data <- rbind(population_data,
                          population_data %>%
                          filter(country=='Switzerland') %>%
                            mutate(population = population / sum(population) * 39137) %>%
                            mutate(country = 'Liechtenstein')) %>%
                    filter(country %in% country_list$country_name)



## SET FINAL TABLE: age_specific_vaccine_data ----
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

## GERMANY: only 1_Age<60 and 1_Age>60
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

## Netherlands only "ALL" (>18) is specified
country_ISO <- 'NL';country_name <- "Netherlands"
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

# Netherlands: only <18 and Adults
age_specific_vaccine_data_netherlands <- impute_uptake_adult_ages(vaccine_dat,population_data,'NL','Netherlands')

age_specific_vaccine_data <- rbind(
  age_specific_vaccine_data[age_specific_vaccine_data$country_name != 'Netherlands',],
  age_specific_vaccine_data_netherlands
)


# Liechtenstein: only <18 and Adults
age_specific_vaccine_data_liechtenstein <- impute_uptake_adult_ages(vaccine_dat,population_data,'LI','Liechtenstein')

age_specific_vaccine_data <- rbind(
  age_specific_vaccine_data[age_specific_vaccine_data$country_name != 'Liechtenstein',],
  age_specific_vaccine_data_liechtenstein
)

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


## SET FINAL TABLE: variant_data ----

variant_data <- variant_data %>% 
  filter(variant %in% c('Alpha', 'Delta'))

unique(variant_data$variant)


start_date  <- as.Date('2020-01-01')
end_date    <- as.Date('2022-01-01')

variant_proportion <- variant_data %>%
  group_by(variant, location) %>%
  nest %>%
  ungroup() %>%
  mutate(data = map(data, function(data){
    predict_voc_prevalence(data$num_sequences, data$num_sequences_total, data$date,start_date,end_date)
  })) %>%
  unnest(cols = data)

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


## MISSING DATA: HOSPITAL & ICU ADMISSIONS ----

hosp_imputed <- aggregated_covid_data %>% 
  filter(!is.na(hospitalization),
         cases > 0) %>% 
  group_by(date) %>% 
  summarise(hospital_ratio = median(hospitalization / cases,na.rm=T),
            # hospital_ratio_min = min(hospitalization / cases,na.rm=T),
            # hospital_ratio_max = max(hospitalization / cases,na.rm=T),
            icu_ratio = median(icu / cases,na.rm=T)) %>%
  mutate(hospital_ratio_rollmedian = rollmedian(hospital_ratio,k=7,align='center',fill=NA),
         icu_ratio_rollmedian = rollmedian(icu_ratio,k=7,align='center',fill=NA))

# plot(hosp_imputed$date,
#      hosp_imputed$hospital_ratio)
# lines(hosp_imputed$date,
#      hosp_imputed$hospital_ratio_min,col='grey')
# lines(hosp_imputed$date,
#       hosp_imputed$hospital_ratio_max,col='grey')
# lines(hosp_imputed$date,
#       hosp_imputed$hospital_ratio_rollmedian,
#       col=3,lwd=4)
# 
# plot(hosp_imputed$date,
#      hosp_imputed$icu_ratio)
# lines(hosp_imputed$date,
#      hosp_imputed$icu_ratio_rollmedian,
#      col=3,lwd=4)

aggregated_covid_data <- (aggregated_covid_data %>% 
                            right_join(hosp_imputed))  %>%
  mutate(hospitalization = replace(hospitalization,
                                   is.na(hospitalization),
                                   (cases * hospital_ratio)[is.na(hospitalization)]),
         icu = replace(icu,
                       is.na(icu),
                       (cases * icu_ratio)[is.na(icu)])) %>%
  select(-hospital_ratio,-icu_ratio)

## MISSING DATA: AGE-DISTRIBUTION HOSPITAL & ICU ADMISSIONS ----

# make sure all country and age combinations are present
age_specific_covid_data <- age_specific_covid_data %>% 
  filter(country != 'aggregated') %>% 
  full_join(expand_grid(country=country_list$country_name,
                        age_group= unique(age_specific_covid_data$age_group)))

# use aggregated statistics if country-data is missing
age_specific_covid_data <- age_specific_covid_data %>%
  group_by(country) %>%
  mutate(cases = if_else(is.na(cases),age_specific_aggregated$cases,cases),
         hospitalization = if_else(is.na(hospitalization),age_specific_aggregated$hospitalization,hospitalization),
         icu = if_else(is.na(icu),age_specific_aggregated$icu,icu),
         mortality = if_else(is.na(mortality),age_specific_aggregated$mortality,mortality))


## ADDITIONAL CORRECTIONS ----

# explicitly set NA's to 0
aggregated_covid_data$cases[is.na(aggregated_covid_data$cases)] <- 0
aggregated_covid_data$hospitalization[is.na(aggregated_covid_data$hospitalization)] <- 0
aggregated_covid_data$icu[is.na(aggregated_covid_data$icu)] <- 0
aggregated_covid_data$mortality[is.na(aggregated_covid_data$mortality)] <- 0


## SAVE FINAL DATA SETS ----
save(aggregated_covid_data,
     age_specific_covid_data,
     population_data,
     age_specific_vaccine_data,
     variant_data,
     #file = file.path(format(Sys.Date(),format='data_raw/local/%Y%m%d_preprocessed_data.RData')))
     file = 'data/preprocessed_data.RData')

## SAVE DIFFERENT DATA SETS ----
saveRDS(aggregated_covid_data,file = 'data/aggregated_covid_data.rds')
saveRDS(age_specific_covid_data,file = 'data/age_specific_covid_data.rds')
saveRDS(population_data,file = 'data/population_data.rds')
saveRDS(age_specific_vaccine_data,file = 'data/age_specific_vaccine_data.rds')
saveRDS(variant_data,file = 'data/variant_data.rds')

