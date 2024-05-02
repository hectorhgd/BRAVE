Compute_benefits_mock <- function(age_specific_covid_data,
                             aggregated_covid_data,
                             age_specific_vaccine_data,
                             variant_data,
                             population_data,
                             start_date,
                             end_date,
                             parameters,
                             bool_by_country,
                             progress,
                             interruptor) {
 
  output_mock <- readRDS('results/output_benefits_mock_data3.rds')
  output_mock$interrupted <- FALSE
  # output_mock$prevented_benefits_age_vaccine_100k <- output_mock$prevented_benefits_age_vaccine_dose_100k %>%
  #   filter(dose == 2) %>%
  #   select(-dose) %>%
  #   mutate(prevented_icu = 0.1 * prevented_hospitalization)
  # 
  # output_mock$prevented_benefits_age_vaccine$prevented_icu <- 0.1 * output_mock$prevented_benefits_age_vaccine$prevented_hospitalization 
  # 
  # output_mock$prevented_benefits_age_vaccine_dose_100k <- NULL
  
  return(output_mock)
}

Observed_burden <- function(age_specific_covid_data,
                            aggregated_covid_data,
                            start_date,
                            end_date) {
  
  age_specific_burden <- aggregated_covid_data %>%
    pivot_longer(c(cases, mortality, hospitalization, icu),
                 names_to = 'burden',
                 values_to = 'total_count') %>%
    left_join(age_specific_covid_data %>%
                pivot_longer(c(cases, mortality, hospitalization, icu),
                             names_to = 'burden',
                             values_to = 'proportion')) %>%
    mutate(count = total_count * proportion) %>%
    select(-c(proportion, total_count)) %>%
    pivot_wider(names_from = burden, values_from = count) %>%
    filter(date >= start_date,
           date <= end_date)
  
  return(age_specific_burden)
}


Compute_benefits <- function(age_specific_covid_data,
                             aggregated_covid_data,
                             age_specific_vaccine_data,
                             variant_data,
                             population_data,
                             start_date,
                             end_date,
                             parameters,
                             bool_by_country = TRUE,
                             progress = NULL,
                             interruptor = NULL) {
  
  
  is_interrupted <- FALSE
  
  parameters <- parameters %>% 
    pivot_longer(cols = -c(vaccine, dose, variant, against, parameter),
                names_to = 'scenario',
                values_to = 'value') %>%
    pivot_wider(names_from = parameter,
                values_from = value)
  
  # initialise final tables
  prevented_benefits_daily <- c()
  prevented_benefits_total <- c()
  
  # burden of disease
  opt_burden <- unique(parameters$against)
  
  # create design of experiments
  model_design <- expand_grid(country_name = as.character(unique(population_data$country)), 
                              scenario = as.character(unique(parameters$scenario)))
  dim(model_design)
  
  # make sure reference data is available from start_date till end_date
  date_all                   <- aggregated_covid_data %>% expand(country,date = seq(start_date,end_date,1))
  aggregated_covid_data_time <- aggregated_covid_data %>% dplyr::right_join(date_all)
  aggregated_covid_data_time <- aggregated_covid_data_time[order(aggregated_covid_data_time$date),]
  
  date_all <- age_specific_vaccine_data %>% expand(date = seq(start_date,end_date,1),country,age_group,vaccine,dose)
  age_specific_vaccine_data_time <- age_specific_vaccine_data %>% dplyr::right_join(date_all)
  age_specific_vaccine_data_time <- age_specific_vaccine_data_time[order(age_specific_vaccine_data_time$date),]
  age_specific_vaccine_data_time$count[is.na(age_specific_vaccine_data_time$count)] <- 0
  
  variant_data_time <- variant_data  %>% expand(date = seq(start_date,end_date,1),variant)
  variant_data_time <- variant_data  %>% dplyr::right_join(variant_data_time)
  
  # loop over the country x vaccine x scenario
  i_experiment <- 30
  i_burden <- 1
  for(i_experiment in 1:nrow(model_design)){
    
    if(!is.null(progress)) {
      progress$set(value = i_experiment / nrow(model_design),
                   message = paste0('Loading results ', round(i_experiment / nrow(model_design)*100), '%'))
    }
   
    
    # if(!is.null(interruptor)) {
    #   int <- interruptor$consumer$consume()$interrupt
    #   if(!is.null(int)) {
    #     is_interrupted <- TRUE
    #     break;
    #   }
    # }
    
    # set parameters
    country_name     = model_design$country_name[i_experiment]
    sel_scenario     = model_design$scenario[i_experiment]
    
    # select reference data
    country_data <- population_data %>% 
      filter(country == country_name)
    
    vaccine_uptake <- age_specific_vaccine_data_time %>%
      filter(country == country_name)
    
    variant_all <- variant_data_time  %>% 
      filter(country == country_name) 
    
    reported_burden_all <- aggregated_covid_data_time %>% 
      filter(country == country_name)
    
    for(i_burden in 1:length(opt_burden)){
      
      sel_burden       = opt_burden[i_burden]
      
      # select reference data
      vaccine_param <- parameters %>% filter(scenario ==sel_scenario,
                                             against == sel_burden)
      
      reported_burden <- reported_burden_all %>% 
        select(date,country,count = eval(sel_burden))
      
      reported_age_distribution <- age_specific_covid_data %>%
        filter(country == country_name) %>%
        select(country,age_group,frequency = eval(sel_burden)) 
      
      # calculate (prevented) burden 
      country_output <- compute_benefits_country(reported_age_distribution,
                                                 reported_burden,
                                                 vaccine_uptake,
                                                 variant_all,
                                                 country_data,
                                                 start_date,
                                                 end_date,
                                                 vaccine_param)
      
      if(i_burden == 1){
        country_benefits_daily <- country_output$benefits_daily
        country_benefits_total     <- country_output$benefits_total
      } else{
        country_benefits_daily <- merge(country_benefits_daily,
                                           country_output$benefits_daily)
        country_benefits_total     <- merge(country_benefits_total,
                                           country_output$benefits_total)
      }
    }
    
    prevented_benefits_daily <- rbind(prevented_benefits_daily,
                                      country_benefits_daily)
    
    prevented_benefits_total <- rbind(prevented_benefits_total,
                                                      country_benefits_total)
  } # end for-loop
  
  # option to aggregate over all countries
  if(!bool_by_country){
    
    prevented_benefits_daily <- prevented_benefits_daily %>%
      group_by(scenario,age_group,vaccine,date) %>%
      summarise(prevented_cases = sum(prevented_cases,na.rm=T),
                sprevented_hospitalization = sum(prevented_hospitalization,na.rm=T),
                prevented_mortality = sum(prevented_mortality,na.rm=T))
    
    prevented_benefits_total <- prevented_benefits_total %>%
      group_by(scenario,age_group,vaccine,dose) %>%
      summarise(prevented_cases = sum(prevented_cases,na.rm=T),
                prevented_hospitalization = sum(prevented_hospitalization,na.rm=T),
                prevented_mortality = sum(prevented_mortality,na.rm=T))
  }
  
  # return
  return(output_benefits = list(prevented_benefits_daily = prevented_benefits_daily,
                                prevented_benefits_total = prevented_benefits_total,
                                interrupted = is_interrupted))
}


compute_benefits_country <- function(reported_age_distribution,
                                     reported_burden,
                                     vaccine_uptake,
                                     variant_all,
                                     country_data,
                                     start_date,
                                     end_date,
                                     vaccine_param){
  
  # get age groups
  opt_age                 <- levels(country_data$age_group)
  num_age                 <- length(opt_age)
  
  # get dates
  num_days                <- as.numeric(end_date - start_date + 1)
  
  # get vaccines
  opt_vaccine <- unique(vaccine_param$vaccine)
  # opt_vaccine             <- unique(vaccine_param %>% filter(vaccine != 'Janssen') %>% pull(vaccine))
  num_vaccine             <- length(opt_vaccine)
  
  # get vaccine paramete subset
  #vaccine_param_original <- vaccine_param %>% filter(variant == "Original")
  vaccine_param_original <- vaccine_param[1,]
  
  # overal prop_protected_voc
  prop_vaccine_array     <- array(0,dim=c(num_days,num_age,num_vaccine))
  prop_booster_array     <- array(0,dim=c(num_days,num_age,num_vaccine))
  prop_protected_total   <- matrix(0,num_days,num_age)
  prop_protected_booster <- matrix(0,num_days,num_age)
  
  # vaccine_uptake <- vaccine_uptake %>%
  #   group_by(country, age_group, vaccine, date, dose) %>%
  #   slice(1) %>%
  #   ungroup()
  
  # for vaccine
  # Compute the number of inhabitants per age group with exactly 1, 2 or 3 doses on each date
  active_vaccination <- vaccine_uptake %>%
    group_by(age_group, vaccine, dose) %>%
    arrange(date) %>%
    mutate(cum_count = cumsum(count)) %>%
    ungroup() %>%
    group_by(date, age_group, vaccine) %>%
    mutate(cum_second_dose_vaccine = cum_count[dose == 2]) %>%
    group_by(date, age_group) %>%
    mutate(cum_second_dose = sum(cum_count[dose == 2]),
           cum_booster = sum(cum_count[dose == 3])) %>%
    ungroup() %>%
    mutate(active_count = ifelse(dose == 1,
                                 cum_count - cum_second_dose_vaccine,
                                 ifelse(dose == 2,
                                        (cum_second_dose - cum_booster) / cum_second_dose * cum_count,
                                        cum_count))) %>%
    mutate(active_count = replace_na(active_count, 0)) %>%
    select(age_group, vaccine, date, dose, active_count)
  
  protection <- data.frame()
  date_range <- sort(unique(vaccine_uptake$date))
  
  
  i_vaccine <- 1
  i_dose <- 1
  sel_vaccine <- opt_vaccine[1]
  for(i_vaccine in 1:num_vaccine) {
    
    # define vaccine type
    sel_vaccine = opt_vaccine[i_vaccine]
    
    for(i_dose in 1:3) {
      
      prop_protected    <- get_protected_proportion(country_data       = country_data,
                                                    vaccine_param      = vaccine_param %>% filter(vaccine == sel_vaccine, dose == i_dose),
                                                    vaccine_uptake     = vaccine_uptake %>% filter(vaccine == sel_vaccine, dose == i_dose),
                                                    variant_all        = variant_all)
      
      protection <- rbind(
        protection,
        data.frame(date = date_range[as.numeric(row(prop_protected))],
                   age_group = opt_age[as.numeric(col(prop_protected))],
                   protection = as.numeric(prop_protected),
                   vaccine = sel_vaccine,
                   dose = i_dose)
      )
      
      
    }
  }
  
  # Zero uptake --> divide by zero = NA
  protection$protection[is.na(protection$protection)] <- 0
  active_protection <- left_join(
    active_vaccination,
    protection,
    by = c('age_group', 'vaccine', 'date', 'dose')) %>%
    left_join(country_data, by = 'age_group')
  
  prop_protected <- active_protection %>%
    group_by(date, age_group, vaccine) %>%
    summarise(protected_vaccine = sum(protection * active_count / population)) %>%
    ungroup() %>%
    group_by(date, age_group) %>%
    mutate(protected_total = sum(protected_vaccine)) %>%
    ungroup()
  
  burden_age <- left_join(reported_burden,
                          reported_age_distribution, by = 'country') %>%
    mutate(count = count * frequency) %>%
    left_join(prop_protected, by = c('date', 'age_group')) %>%
    mutate(exp_burden_without_vaccination = count / (1-protected_total),
           exp_burden_prevented = exp_burden_without_vaccination * protected_vaccine)
    
  
  prevented_daily <- burden_age %>%
    select(date, country, age_group, vaccine, exp_burden_prevented) %>%
    rename(!!paste0('prevented_',vaccine_param_original$against) := exp_burden_prevented) %>%
    mutate(scenario = vaccine_param$scenario[1])
  
  doses <- vaccine_uptake %>%
    group_by(age_group, vaccine) %>%
    summarise(count = sum(count)) 
  
  prevented_total <- burden_age %>%
    select(date, country, age_group, vaccine, exp_burden_prevented) %>%
    group_by(country, age_group, vaccine) %>%
    summarise(prevented = sum(exp_burden_prevented)) %>%
    rename(!!paste0('prevented_',vaccine_param_original$against) := prevented) %>%
    ungroup() %>%
    left_join(doses, by = c('age_group', 'vaccine')) %>%
    mutate(scenario = vaccine_param$scenario[1])
  
  return(list(benefits_daily = prevented_daily,
              benefits_total = prevented_total))
}

# Helper functions  ----
#----------------- -
logistic_growth = function(t,K,k0,x0){
  return(1/(1+exp(-k0*(t-x0))))
}

vaccine_effectiveness = function(d, K, k0, x0, w, D, ve){
  value = logistic_growth(d, K, k0, x0)*ve*exp(-w*as.numeric((d - D) > 0)*(d-D))
  return(value)
}


############################################################## #
#  Predicted prevented cases using a probabilistic 
#  implementation     
#                                                              
#  Methods:                                                    
#    *Method 1: protection against infection after the second  
#               vaccine dose                                   
############################################################## #

get_protected_proportion = function(country_data,
                                    vaccine_param,
                                    vaccine_uptake,
                                    variant_all){
  
  if(nrow(vaccine_param) == 0 || nrow(vaccine_uptake) == 0){
    return(0)
  }
  
  num_day_uptake          <- length(unique(vaccine_uptake$date))
  opt_age                 <- levels(country_data$age_group)
  num_ages                <- nlevels(country_data$age_group)
  
  # set parametes
  vaccine_param_original <- vaccine_param %>% filter(variant == "Original") 
  ve_burden              <- vaccine_param_original$efficacy
  delay_protection       <- vaccine_param_original$protection_delay
  waning_vaccination     <- vaccine_param_original$waning_delay
  
  logistic_growth_rate    <- uniroot(f = function(s){logistic_growth(t = delay_protection, 
                                                                     K = 1, 
                                                                     k0 = s, 
                                                                     x0 = (delay_protection/2)) - 0.999},
                                     lower = 0, upper = 3)$root
  
  ve_estim       <- vaccine_effectiveness(d  = seq(0,(num_day_uptake-1), 1), 
                                          K  = 1, 
                                          k0 = logistic_growth_rate, 
                                          x0 = (delay_protection/2), 
                                          w  = 1/waning_vaccination, 
                                          D  = delay_protection, 
                                          ve = ve_burden)
  
  prop_protected <- matrix(0,num_day_uptake,num_ages)
  
  agegp_id <- 5
  for (agegp_id in 1:length(opt_age)){ 
    vaccine_uptake_relative <- vaccine_uptake$count[vaccine_uptake$age_group == opt_age[agegp_id]] / country_data$population[agegp_id]
    for (time_id in 1:num_day_uptake){
      prop_protected[time_id,agegp_id] = sum(vaccine_uptake_relative[1:time_id]*ve_estim[(time_id - (1:time_id))+1]) / sum(vaccine_uptake_relative[1:time_id])
    }
  }
  
  # set VOC parametes
  rel_ve_alpha <- (vaccine_param %>% filter(variant == "Alpha") %>% 
                     pull(efficacy)) / vaccine_param_original$efficacy
  
  rel_ve_delta <- (vaccine_param %>% filter(variant == "Delta") %>% 
                     pull(efficacy)) / vaccine_param_original$efficacy
  
  p_alpha <- unlist(variant_all %>% 
                      filter(variant == "Alpha") %>%
                      pull(proportion))
  p_delta <- variant_all %>% 
    filter(variant == "Delta") %>%
    pull(proportion)
  p_original <- (1 - p_alpha - p_delta)
  
  weights_voc <- (p_original + p_alpha*unlist(rel_ve_alpha) + p_delta*rel_ve_delta)
  
  # account for prevalence of VOC
  prop_protected <- prop_protected * as.numeric(weights_voc)
  
  # return result
  return(prop_protected)
}
