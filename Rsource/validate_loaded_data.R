is_loaded_data_valid <- observe({
  
  ## benefits
  valid_population <- Init_validation_status()
  valid_age_specific_covid <- Init_validation_status()
  valid_aggregated_covid <- Init_validation_status()
  valid_age_specific_vaccine <- Init_validation_status()
  valid_variant <- Init_validation_status()
  
  # Avoid having to call the reactive function each time
  population <- population_data()
  age_specific_covid <- age_specific_covid_data()
  aggregated_covid <- aggregated_covid_data()
  age_specific_vaccine <- age_specific_vaccine_data()
  variant <- variant_data()
  
  countries <- NULL
  age_groups <- NULL
  
  ## population
  valid_population <- Validate_data_loaded(population, valid_population, required = TRUE)
  if(!is.null(population)) {
    valid_population <- Validate_column(population, valid_population,
                                        required_columns =  c('country', 'age_group', 'population'))
    
    if('country' %in% colnames(population)) {
      countries <- unique(population$country)  
    }
    
    if('age_group' %in% colnames(population)) {
      age_groups <- unique(population$age_group)  
      
      valid_population <- Validate_age_group(valid_population, age_groups)
      
    }
  }
  
  Set_valid_status("status_upload_benefits_population", 
                   "error_msg_upload_benefits_population",
                   valid_population)
  
  
  ## age specific covid
  valid_age_specific_covid <- Validate_data_loaded(age_specific_covid, valid_age_specific_covid, 
                                                   required = FALSE)
  
  if(!is.null(age_specific_covid)) {
    valid_age_specific_covid <- Validate_column(age_specific_covid, valid_age_specific_covid,
                                                required_columns =  c('country', 'age_group', 'cases', 'hospitalization', 'icu', 'mortality'))

    if('country' %in% colnames(age_specific_covid)) {
      valid_age_specific_covid <- Validate_levels(age_specific_covid$country,
                      valid_age_specific_covid,
                      countries,
                      'country')
    }

    if('age_group' %in% colnames(age_specific_covid)) {
      valid_age_specific_covid <- Validate_levels(age_specific_covid$age_group,
                                                  valid_age_specific_covid,
                                                  age_groups,
                                                  'age_group')
    }
    
    # check proportions don't exceed 1
    if(all(c('cases', 'hospitalization', 'icu', 'mortality') %in% colnames(age_specific_covid))) {
      max_total_prop <- age_specific_covid %>%
        group_by(country) %>%
        summarise(max_prop = max(sum(cases), sum(hospitalization), sum(icu), sum(mortality))) %>%
        filter(max_prop > 1.01)
      
      if(nrow(max_total_prop) > 0) {
        valid_age_specific_covid$error <- c(valid_age_specific_covid$error, 
                                     paste0('Combined proportion exceeds 1.01 for country ', max_total_prop$country))
        
        valid_age_specific_covid$status <- max(valid_age_specific_covid$status, STATUS_ERROR)
      }
      
    }
  }
  
  Set_valid_status("status_upload_benefits_age_specific_covid", 
                   "error_msg_upload_benefits_age_specific_covid",
                   valid_age_specific_covid)
  
  ## aggregated covid
  valid_aggregated_covid <- Validate_data_loaded(aggregated_covid, valid_aggregated_covid, 
                                                   required = FALSE)
  
  if(!is.null(aggregated_covid)) {
    valid_aggregated_covid <- Validate_column(aggregated_covid, valid_aggregated_covid, 
                                                required_columns =  c('date', 'country', 'cases', 'hospitalization', 'icu', 'mortality'))
    
    if('country' %in% colnames(aggregated_covid)) {
      valid_aggregated_covid <- Validate_levels(aggregated_covid$country, 
                                                  valid_aggregated_covid,
                                                  countries,
                                                  'country')
    }
  }
  
  ## age specific vaccine
  valid_age_specific_vaccine <- Validate_data_loaded(age_specific_vaccine, valid_age_specific_vaccine, 
                                                   required = FALSE)
  
  if(!is.null(age_specific_vaccine)) {
    valid_age_specific_vaccine <- Validate_column(age_specific_vaccine, valid_age_specific_vaccine, 
                                                required_columns =  c('date', 'country', 'age_group', 'vaccine', 'dose', 'count'))
    
    if('country' %in% colnames(age_specific_vaccine) & !is.null(countries)) {
      valid_age_specific_vaccine <- Validate_levels(age_specific_vaccine$country,
                      valid_age_specific_vaccine,
                      countries,
                      'country')
    }
    
    if('age_group' %in% colnames(age_specific_vaccine) & !is.null(age_groups)) {
      valid_age_specific_vaccine <- Validate_levels(age_specific_vaccine$age_group, 
                                                  valid_age_specific_vaccine,
                                                  age_groups,
                                                  'age_group')
    }
    
    if('dose' %in% colnames(age_specific_vaccine)) {
      if(!all(age_specific_vaccine$dose %in% c(1, 2, 3))) {
        valid_age_specific_vaccine$error <- c(valid_age_specific_vaccine$error, 
                                              paste0('Only dose 1, 2 and 3 are supported'))
        
        valid_age_specific_vaccine$status <- STATUS_ERROR
      }
    }
    
    # Check no country has vaccinated more than 100% of its residents
    if(valid_age_specific_vaccine$status == STATUS_VALID & valid_population$status == STATUS_VALID) {
        
      exceed_pop <- population %>% 
        left_join(age_specific_vaccine,
                  by = c('country', 'age_group')) %>%
        group_by(country, age_group, dose) %>%
        summarise(count = sum(count),
                  pop = population[1]) %>%
        ungroup() %>%
        filter(count > pop)
      
      if(nrow(exceed_pop) > 0) {
        valid_age_specific_vaccine$status <- STATUS_ERROR
        valid_age_specific_vaccine$error <- c(valid_age_specific_vaccine$error, 
                                              paste0('Vaccine coverage exceeds population size for ',
                                                     paste(exceed_pop$country, exceed_pop$age_group, 'dose', exceed_pop$dose)))
      }
        
    }
    
  }
  

  ## covid variant
  valid_variant <- Validate_data_loaded(variant, valid_variant, 
                                                     required = FALSE)
  
  if(!is.null(variant)) {
    valid_variant <- Validate_column(variant, valid_variant, 
                                                  required_columns =  c('date', 'country', 'variant', 'proportion') )
    
    if('country' %in% colnames(variant)) {
      valid_variant <- Validate_levels(variant$country,
                                                    valid_variant,
                                                    countries,
                                                    'country')
    }
    
    if('age_group' %in% colnames(variant)) {
      valid_variant <- Validate_levels(variant$age_group, 
                                                    valid_variant,
                                                    age_groups,
                                                    'age_group')
    }
  }
  
  Set_valid_status("status_upload_benefits_variant", 
                   "error_msg_upload_benefits_variant",
                   valid_variant)
  
  ## risks
  
  valid_observed_risks <- Init_validation_status()
  valid_bgr <- Init_validation_status()
  valid_aggregated_vaccine <- Init_validation_status()
  
  # Avoid having to call the reactive function each time
  observed_risks <- observed_risks_data()
  bgr <- bgr_data()
  aggregated_vaccine <- aggregated_vaccine_data()
  
  levels <- NULL
  
  ## observed risks
  valid_observed_risks <- Validate_data_loaded(observed_risks, valid_observed_risks, required = TRUE)
  if(!is.null(observed_risks)) {
    valid_observed_risks <- Validate_column(observed_risks, valid_observed_risks,
                                        required_columns =  c('age_group', 'sex', 'cases', 'condition', 'vaccine'))
    
    if(all(c('age_group', 'condition', 'vaccine') %in% colnames(observed_risks))) {
      levels <- observed_risks %>% select(age_group, condition, vaccine) %>% distinct
      
      valid_observed_risks <- Validate_age_group(valid_observed_risks, unique(observed_risks$age_group))
      
      if('vaccine' %in% colnames(age_specific_vaccine)) {
        vaccines <- unique(observed_risks$vaccine) %>% as.character
        missing_levels <- vaccines[!(vaccines %in% age_specific_vaccine$vaccine)]
        
        if(length(missing_levels) > 0) {
          valid_age_specific_vaccine$error <- c(valid_age_specific_vaccine$error, 
                                       paste0('Missing vaccine ', missing_levels))
          
          valid_age_specific_vaccine$status <- STATUS_ERROR
        }
      }
    }
    
    if('sex' %in% colnames(observed_risks)) {
      if(!all(observed_risks$sex %in% c('F', 'Female', 'M', 'Male'))) {
        valid_observed_risks$error <- c(valid_observed_risks$error, 
                                     paste0('Invalid specification of sex. Allowed levels are M, Male, F and Female'))
        
        valid_observed_risks$status <- max(valid_observed_risks$status, STATUS_ERROR)    
      }
    }
  }
  
  Set_valid_status("status_upload_risks_observed", 
                   "error_msg_upload_risks_observed",
                   valid_observed_risks)
  
  Set_valid_status("status_upload_benefits_age_specific_vaccine", 
                   "error_msg_upload_benefits_age_specific_vaccine",
                   valid_age_specific_vaccine)
  
  ## bgr
  valid_bgr <- Validate_data_loaded(bgr, valid_bgr, required = TRUE)
  if(!is.null(bgr)) {
    valid_bgr <- Validate_column(bgr, valid_bgr,
                                            required_columns =  c('source', 'age_group', 'sex', 'condition', 'cases', 'person_years', 'period'))
    
    if(!is.null(levels) & all(c('age_group', 'condition') %in% colnames(bgr))) {
      levels_obs <- bgr %>% select(age_group, condition) %>% distinct
      
      valid_bgr <- Validate_age_condition(valid_bgr, levels_obs, levels)
    }
  }
  
  Set_valid_status("status_upload_risks_bgr", 
                   "error_msg_upload_risks_bgr",
                   valid_bgr)
  
  ## vaccin_aggregated
  valid_aggregated_vaccine<- Validate_data_loaded(aggregated_vaccine, valid_aggregated_vaccine, required = TRUE)
  if(!is.null(aggregated_vaccine)) {
    valid_aggregated_vaccine <- Validate_column(aggregated_vaccine, valid_aggregated_vaccine,
                                 required_columns =  c('vaccine', 'sex', 'age_group', 'imputation_id', 'cov', 'var', 'imputation_method'))
    
    if(!is.null(levels) & all(c('age_group', 'vaccine') %in% colnames(aggregated_vaccine))) {
      levels_obs <- aggregated_vaccine %>% select(age_group, vaccine) %>% distinct
      
      valid_aggregated_vaccine <- Validate_age_vaccine(valid_aggregated_vaccine, levels_obs, levels)
    }
    
    if('imputation_method' %in% colnames(aggregated_vaccine)) {
      lev <- unique(aggregated_vaccine$imputation_method)
      lev <- lev[!(lev %in% c('multiple imputation', 'fixed proportion'))]
      
      if(length(lev) > 0) {
        valid_aggregated_vaccine$error <- c(valid_aggregated_vaccine$error,
                                            paste0('Unrecognized imputation method ', lev))
        valid_aggregated_vaccine$status <- max(valid_aggregated_vaccine$status, STATUS_ERROR)
      }
      
      if('imputation_id' %in% colnames(aggregated_vaccine)) {
        if(nrow(aggregated_vaccine %>% filter(imputation_id > 1, imputation_method == 'fixed proportion')) > 0) {
          valid_aggregated_vaccine$error <- c(valid_aggregated_vaccine$error,
                                              paste0('Imputation_id == 1 expected when imputation_method == "fixed proportion"'))
          valid_aggregated_vaccine$status <- max(valid_aggregated_vaccine$status, STATUS_ERROR)
        }
      }
      
    }
  }
  
  Set_valid_status("status_upload_risks_aggregated_vaccine", 
                   "error_msg_upload_risks_aggregated_vaccine",
                   valid_aggregated_vaccine)
  
  
  if(max(valid_variant$status,
         valid_population$status,
         valid_age_specific_covid$status,
         valid_aggregated_covid$status,
         valid_age_specific_vaccine$status) != STATUS_ERROR) {
    # determine valid date range
    variant_first <- min(variant$date, na.rm = TRUE)
    variant_last <- max(variant$date, na.rm = TRUE)
    
    aggregated_covid_summary <- aggregated_covid %>%
      filter(!is.na(cases), !is.na(hospitalization), !is.na(icu), !is.na(mortality)) %>%
      group_by(country) %>%
      summarise(first = min(date, na.rm = TRUE),
                last = max(date, na.rm = TRUE)) %>%
      ungroup() %>%
      summarise(first = max(first),
                last = min(last))
    
    aggregated_covid_first <- aggregated_covid_summary$first
    aggregated_covid_last <- aggregated_covid_summary$last
    
    age_specific_vaccine_first <- min(age_specific_vaccine$date, na.rm = TRUE)
    age_specific_vaccine_last <- max(age_specific_vaccine$date, na.rm = TRUE)
    
    first <- max(age_specific_vaccine_first, aggregated_covid_first, variant_first)
    last <- min(age_specific_vaccine_last, aggregated_covid_last, variant_last)
    
    if(first < last) {
      html('status_upload_benefits', "Ready")
      removeClass('status_upload_benefits', "msginvalid")
      addClass('status_upload_benefits', "msgvalid")
      
      updateSliderInput(session, 'input_time_window_analyses', value = c(first, last),
                        min = first, max = last, timeFormat = '%Y-%m-%d')
    } else {
      valid_aggregated_covid$error <- c(valid_aggregated_covid$error,
                                          paste0('No overlapping timewindow found in which all data sources are available'))
      valid_aggregated_covid$status <- max(valid_aggregated_covid$status, STATUS_ERROR)
      
      html('status_upload_benefits', "Fix error to proceed")
      removeClass('status_upload_benefits', "msgvalid")
      addClass('status_upload_benefits', "msginvalid")
    }
    
    
    
  } else {
    html('status_upload_benefits', "Fix error to proceed")
    removeClass('status_upload_benefits', "msgvalid")
    addClass('status_upload_benefits', "msginvalid")
  }
  
  Set_valid_status("status_upload_benefits_aggregated_covid", 
                   "error_msg_upload_benefits_aggregated_covid",
                   valid_aggregated_covid)
  
  if(max(valid_observed_risks$status,
         valid_bgr$status,
         valid_aggregated_vaccine$status) != STATUS_ERROR) {
    html('status_upload_risks', "Ready")
    removeClass('status_upload_risks', "msginvalid")
    addClass('status_upload_risks', "msgvalid")
  } else {
    html('status_upload_risks', "Fix error to proceed")
    removeClass('status_upload_benefits', "msgvalid")
    addClass('status_upload_risks', "msginvalid")
  }
  
  if(max(valid_observed_risks$status,
         valid_bgr$status,
         valid_aggregated_vaccine$status,
         valid_variant$status,
         valid_population$status,
         valid_age_specific_covid$status,
         valid_aggregated_covid$status,
         valid_age_specific_vaccine$status) == STATUS_ERROR) {
    shinyjs::disable("btn_finish_data_upload")
  } else {
    shinyjs::enable("btn_finish_data_upload")
    
    if(auto_proceed_to_results) {
      click("btn_finish_data_upload")
    }
  }
  
  
  
})