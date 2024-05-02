######################################################################## #
# This file is part of the benefit-risk analysis of COVID-19 vaccines
# in the EU.
# 
# This R file includes:
#     - helper functions
#     - run_static function
# 
# Copyright: DSI, UHasselt, Belgium, 2022.
######################################################################## #

# placeholder function
compute_benefits <- function(age_specific_covid_data,
                             aggregated_covid_data,
                             age_specific_vaccine_data,
                             variant_data,
                             population_data,
                             start_date,
                             end_date,
                             parameters,
                             bool_by_country = FALSE) {
  
  # initialise final tables
  prevented_benefits_age_vaccine <- c()
  prevented_benefits_age_vaccine_dose_100k <- c()
  
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
        country_benefits_absolute <- country_output$benefits_absolute
        country_benefits_dose     <- country_output$benefits_dose
      } else{
        country_benefits_absolute <- merge(country_benefits_absolute,
                                           country_output$benefits_absolute)
        country_benefits_dose     <- merge(country_benefits_dose,
                                           country_output$benefits_dose)
      }
    }
   
    prevented_benefits_age_vaccine <- rbind(prevented_benefits_age_vaccine,
                                            country_benefits_absolute)
    
    prevented_benefits_age_vaccine_dose_100k <- rbind(prevented_benefits_age_vaccine_dose_100k,
                                                      country_benefits_dose)
  } # end for-loop
  
  # option to aggregate over all countries
  if(!bool_by_country){
    
    prevented_benefits_age_vaccine <- prevented_benefits_age_vaccine %>%
                                        group_by(scenario,age_group,vaccine,date) %>%
                                        summarise(prevented_cases = sum(prevented_cases,na.rm=T),
                                                  sprevented_hospitalization = sum(prevented_hospitalization,na.rm=T),
                                                  prevented_mortality = sum(prevented_mortality,na.rm=T))
    
    prevented_benefits_age_vaccine_dose_100k <- prevented_benefits_age_vaccine_dose_100k %>%
                                                  group_by(scenario,age_group,vaccine,dose) %>%
                                                  summarise(prevented_cases = sum(prevented_cases,na.rm=T),
                                                            prevented_hospitalization = sum(prevented_hospitalization,na.rm=T),
                                                            prevented_mortality = sum(prevented_mortality,na.rm=T))
  }
  
  # return
  return(output_benefits = list(prevented_benefits_age_vaccine = prevented_benefits_age_vaccine,
                                prevented_benefits_age_vaccine_dose_100k = prevented_benefits_age_vaccine_dose_100k))
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
  num_vaccine             <- length(opt_vaccine)
  
  # get vaccine paramete subset
  #vaccine_param_original <- vaccine_param %>% filter(variant == "Original")
  vaccine_param_original <- vaccine_param[1,]
  
  # overal prop_protected_voc
  prop_vaccine_array     <- array(0,dim=c(num_days,num_age,num_vaccine))
  prop_booster_array     <- array(0,dim=c(num_days,num_age,num_vaccine))
  prop_protected_total   <- matrix(0,num_days,num_age)
  prop_protected_booster <- matrix(0,num_days,num_age)
  
  
  # for vaccine
  i_vaccine <- 1
  sel_vaccine <- opt_vaccine[1]
  for(i_vaccine in 1:num_vaccine) {
    
    # define vaccine type
    sel_vaccine = opt_vaccine[i_vaccine]
    
    # calculate the proportion protected by dose 1
    prop_protected_dose1    <- get_protected_proportion(country_data       = country_data,
                                               vaccine_param      = vaccine_param %>% filter(vaccine == sel_vaccine, dose == 1),
                                               vaccine_uptake     = vaccine_uptake %>% filter(vaccine == sel_vaccine, dose == 1),
                                               variant_all        = variant_all)
    
    # calculate the proportion protected by dose 2
    prop_protected_dose2    <- get_protected_proportion(country_data       = country_data,
                                                        vaccine_param      = vaccine_param %>% filter(vaccine == sel_vaccine, dose == 2),
                                                        vaccine_uptake     = vaccine_uptake %>% filter(vaccine == sel_vaccine, dose == 2),
                                                        variant_all        = variant_all)

    # calculate the proportion protected by dose 3
    prop_protected_dose3    <- get_protected_proportion(country_data       = country_data,
                                                        vaccine_param      = vaccine_param %>% filter(vaccine == sel_vaccine, dose == 3),
                                                        vaccine_uptake     = vaccine_uptake %>% filter(vaccine == sel_vaccine, dose == 3),
                                                        variant_all        = variant_all)

    # account for dose 1 and dose 2
    prop_protected_vaccine <- prop_protected_dose1 + prop_protected_dose2 - (prop_protected_dose1*prop_protected_dose2)
    
    # save total protected proportion by dose (1+2) and by booster dose
    prop_protected_total   <- prop_protected_total + prop_protected_vaccine
    prop_protected_booster <- prop_protected_booster + prop_protected_dose3
    
    # save vaccine specific proportions
    prop_vaccine_array[,,i_vaccine] <- prop_protected_vaccine
    prop_booster_array[,,i_vaccine] <- prop_protected_dose3
  }
  
  # account for overlapping protection by booster doses
  prop_protected_booster <- prop_protected_booster - (prop_protected_total*prop_protected_booster)
  
  # include additional protection by booster doses
  prop_protected_total <- prop_protected_total + prop_protected_booster
  
  # init output variables
  exp_burden_without_vaccination <- prop_protected_total * 0
  exp_burden_prevented           <- exp_burden_without_vaccination
  
  agegp_id <- 1
  for (agegp_id in 1:8){ 
    reported_burden_age <- (reported_burden %>% pull(count)) * (reported_age_distribution %>% filter(age_group == opt_age[agegp_id]) %>% pull(frequency))
    expected_burden_age <-  reported_burden_age / (1-prop_protected_total[,agegp_id])

    exp_burden_without_vaccination[,agegp_id] <- (expected_burden_age)
    exp_burden_prevented[,agegp_id] <- (expected_burden_age - reported_burden_age)
  }
  

  # get total protection by booster (irrespective of 1st and 2nd dose)
  prop_booster_sum = apply(prop_booster_array,2,rowSums)

  # initialise vaccine-specific matrices (in a combined array)  
  protected_vaccine_array <- prop_vaccine_array * 0
  protected_booster_array <- prop_booster_array * 0
  
  # vaccine specific prevented cases
  i_vaccine <- 1
  for(i_vaccine in 1:num_vaccine) {
    prop_vaccine <-  prop_vaccine_array[,,i_vaccine] / prop_protected_total
    prop_vaccine[is.na(prop_vaccine)] <- 0
    protected_vaccine_array[,,i_vaccine] <- exp_burden_prevented * prop_vaccine
    
    prop_booster <- (prop_protected_booster / prop_protected_total) * (prop_booster_array[,,i_vaccine] / prop_booster_sum)
    prop_booster[is.na(prop_booster)] <- 0
    protected_booster_array[,,i_vaccine] <- exp_burden_prevented * prop_booster
  }
  
  # A. absolute number of prevented cases
  benefits_vaccine <- data.frame(scenario = vaccine_param_original$scenario,
                                  date = rep(reported_burden$date,num_age*num_vaccine),
                                  age_group = rep(opt_age,num_vaccine,each=num_days),
                                  country = unique(country_data$country),
                                  vaccine = rep(opt_vaccine,each=num_age*length(reported_burden$date)),
                                  dose = 2,
                                  prevented = as.numeric(protected_vaccine_array)
  )
  
  benefits_booster <-  data.frame(scenario = vaccine_param_original$scenario,
                                  date = rep(reported_burden$date,num_age*num_vaccine),
                                  age_group = rep(opt_age,num_vaccine,each=num_days),
                                  country = unique(country_data$country),
                                  vaccine = rep(opt_vaccine,each=num_age*length(reported_burden$date)),
                                  dose = 3,
                                  prevented = as.numeric(protected_booster_array)
  )
  
  # merge doses
  benefits_absolute <- rbind(benefits_vaccine,
                             benefits_booster)
  
  
  # # B. prevented cases per dose per 100k population   
  # timewindow <- 25*7
  # exp_total_cases_window     <- exp_burden_without_vaccination[(nrow(exp_burden_without_vaccination) - timewindow+1):nrow(exp_burden_without_vaccination), ]
  # pred_prev_cases_timewindow <- exp_total_cases_window * sim_results$ve_estim[1:timewindow]
  # pred_prev_cases_100k       <- colSums(pred_prev_cases_timewindow) / country_data$population * 100000
  # pred_prev_cases_100k       <- as.matrix(pred_prev_cases_100k,ncol=1)

  suppressMessages(vaccine_doses_by_age <- vaccine_uptake %>%
                    mutate(dose = ifelse(dose == 3, 3, 2)) %>%
                    group_by(age_group,vaccine,dose) %>%
                    summarise(count = sum(count)))

  suppressMessages(benefits_dose <- benefits_absolute %>%
                  group_by(age_group,vaccine,dose) %>%
                  summarise(prevented = sum(prevented)) %>%
                  left_join(country_data) %>%
                  left_join(vaccine_doses_by_age) %>%
                  mutate(prevented_100k = prevented / population * 100000,
                         prevented_100k_dose = prevented_100k / count,
                         scenario = vaccine_param_original$scenario))
  
  benefits_dose$prevented_100k_dose[benefits_dose$count == 0] <- 0
 
  # names
  names(benefits_absolute) <- gsub('prevented',paste0('prevented_',vaccine_param_original$against),names(benefits_absolute))
  names(benefits_dose)     <- gsub('prevented',paste0('prevented_',vaccine_param_original$against),names(benefits_dose))
  
  return(list(benefits_absolute = benefits_absolute,
              benefits_dose = benefits_dose))
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
  ve_burden              <- vaccine_param_original$efficiency
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
      prop_protected[time_id,agegp_id] = sum(vaccine_uptake_relative[1:time_id]*ve_estim[(time_id - (1:time_id))+1])
    }
  }
  
  # set VOC parametes
  rel_ve_alpha <- (vaccine_param %>% filter(variant == "Alpha") %>% 
                     pull(efficiency)) / vaccine_param_original$efficiency
  
  rel_ve_delta <- (vaccine_param %>% filter(variant == "Delta") %>% 
                     pull(efficiency)) / vaccine_param_original$efficiency
  
  p_alpha <- unlist(variant_all %>% 
                      filter(variant == "Alpha") %>%
                      pull(proportion))
  p_delta <- variant_all %>% 
    filter(variant == "Delta") %>%
    pull(proportion)
  p_original <- (1 - p_alpha - p_delta)
  
  weights_voc <- (p_original + p_alpha*unlist(rel_ve_alpha) + p_delta*rel_ve_delta)
  
  # account for prevalence of VOC
  prop_protected <- prop_protected * weights_voc
  
  # return result
  return(prop_protected)
}



