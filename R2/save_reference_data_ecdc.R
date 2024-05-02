######################################################################## #
# This file is part of the benefit-risk analysis of COVID-19 vaccines
# in the EU.
# 
# This R file includes:
#     - to retreive additional reference data from ECDC
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

## VARIANTS OF CONCERN (ECDC) ----
path = "https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/"
file = "data.csv"

url_variants = file.path(path,file)
full_variants_col <- c("location","variant","date","num_sequences","perc_sequences","num_sequences_total")
#full_variants_col <- c("country","variant","year_week","number_detections_variant","percent_variant","number_sequenced")
#full_variants_col <- NULL
full_variants_dat = read_csv(download_ref_file(ref_url = url_variants, 
                                               force_download = force_download), 
                             show_col_types = FALSE)


## CASES & MORTALITY DATA (ECDC) ----
url <- "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath_eueea_daily_ei/csv/data.csv"
incidence_ecdc_col <- c("dateRep","countriesAndTerritories","cases","deaths", "popData2020")

incidence_ecdc <- read_csv(download_ref_file(url, 
                                          col_names = incidence_ecdc_col,
                                          force_download = force_download),
                        show_col_types = FALSE)
names(incidence_ecdc)
head(incidence_ecdc)
message("Incidence dataset ECDC (cases + mortality) successfully loaded")

## HOSPITAL DATA (ECDC) ----
url <- "https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv"
hospital_ecdc_col <- c("country", "indicator", "date" , "value")

hospital_ecdc <- read_csv(download_ref_file(url, 
                                          #col_names = hospital_ecdc_col,
                                          force_download = force_download),
                        show_col_types = FALSE)
names(hospital_ecdc)
message("Hospital dataset ECDC successfully loaded")

## ECDC pre-processing ----
incidence_ecdc <- incidence_ecdc %>% 
                    mutate(country = countriesAndTerritories) %>% 
                    filter(country %in% country_list$country_name)

hospital_ecdc <- hospital_ecdc %>% 
                   filter(country %in% country_list$country_name) %>% 
                   left_join(incidence_ecdc %>% distinct(country,popData2020))

names(hospital_ecdc)
unique(hospital_ecdc$indicator)

tmp_hosp <- hospital_ecdc %>% 
              filter(indicator == "Weekly new hospital admissions per 100k") %>%
              mutate(hospitalization = value /1e5 * popData2020)

tmp_icu <- hospital_ecdc %>% 
              filter(indicator == "Weekly new ICU admissions per 100k") %>%
              mutate(icu = value /1e5 * popData2020)

admission_ecdc <- tmp_hosp %>% 
                      select(country, date_week = date, hospitalization) %>% 
                      left_join(tmp_icu %>% select(country,date,icu)) %>% 
                      select(-date)

date = seq(as.Date('2020-01-05'), as.Date('2022-02-05'), by = 'day')
date_week = seq(as.Date('2020-01-05'), as.Date('2022-02-05'), by = 'week')
date_ecdc <- data.frame(date = date,
                        date_week = rep(date_week,each = 7))

admission_ecdc <- admission_ecdc %>%
  right_join(date_ecdc) %>%
  mutate(icu = icu / 7,
         hospitalization = hospitalization / 7) 

hosp_load <- hospital_ecdc %>% 
  filter(indicator == "Daily hospital occupancy") %>%
  select(country,date,value) %>%
  rename(hospital_load = value)

icu_load <- hospital_ecdc %>% 
  filter(indicator == "Daily ICU occupancy") %>%
  select(country,date,value) %>%
  rename(icu_load = value)

load_ecdc <- hosp_load %>%
  right_join(icu_load)  %>%
  right_join(admission_ecdc) %>%
  mutate(hosp_load_diff = c(NA,diff(hospital_load)),
         icu_load_diff = c(NA,diff(icu_load)),
         country_diff = c(0,diff(as.factor(country)))) %>%
  mutate(hosp_load_diff = replace(hosp_load_diff,country_diff>0,NA),
         icu_load_diff  = replace(icu_load_diff,country_diff>0,NA))


load_ecdc_week <- load_ecdc %>%
  group_by(country,date_week) %>%
  summarise(hospitalization = sum(hospitalization),
            icu = sum(icu),
            hosp_load_diff = sum(hosp_load_diff),
            icu_load_diff = sum(icu_load_diff))


## EXPLORE ECDC DATA ----

# points(aggregated_covid_data %>% 
#   filter(country == 'Croatia') %>%
#   pull(hospitalization))
# 
# plot(admission_ecdc %>%
#   filter(country == 'Austria') %>%
#   pull(hospitalization))
# 
# 
# names(incidence_ecdc)
# 
# names(hospital_ecdc)
# hospital_ecdc %>% left_join(incidence_ecdc %>% filter(country,popData2020))
# 
# unique(hospital_ecdc$indicator[hospital_ecdc$country == 'Austria'])
# 
# hospital_admission_ecdc <- unique(hospital_ecdc %>% 
#                             filter(indicator == "Weekly new hospital admissions per 100k") %>% 
#                             mutate(hosp_adm = value / 100000 * popData2020))


##### tmp #### 
#full_variants_dat %>%
#   str_replace_all('B.1.1.7','Alpha')
  # week_date <- data.frame(date = seq(as.Date('2020-01-01'), as.Date('2022-01-01'), by = 'week')) %>%
  #   mutate(year_week = format(date, '%Y-%U'))
  # 
  # df <- full_variants_dat
  # df <- df %>% mutate_if(is.character, str_replace_all, pattern = 'B.1.1.7', replacement = 'Alpha') %>% 
  #               mutate_if(is.character, str_replace_all, pattern = 'B.1.617', replacement = 'Delta') %>% 
  #               mutate_if(is.character, str_replace_all, pattern = 'B.1.529', replacement = 'Omicron') %>%
  #               right_join(week_date)
  # 
  # names(df)
  # df <- df %>% filter(variant %in% c('Alpha','Delta','Omicron')) %>%
  #              select(location = country,
  #                     variant,
  #                     date,
  #                     num_sequences = number_detections_variant,
  #                     perc_sequences = percent_variant,
  #                     num_sequences_total = number_sequenced)
  #       
  #
  ##### #

       
