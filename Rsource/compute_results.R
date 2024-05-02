compute_results <- function(population, age_specific_covid, aggregated_covid, age_specific_vaccine, variant, observed_risks,
                            bgr, aggregated_vaccine, param_risk, param_ve, timewindow, progress, interruptor) {
  
  results_benefit <- Compute_benefits(age_specific_covid_data = age_specific_covid,
                                           aggregated_covid_data = aggregated_covid,
                                           age_specific_vaccine_data = age_specific_vaccine,
                                           variant_data = variant,
                                           population_data = population,
                                           start_date = timewindow[1],
                                           end_date = timewindow[2],
                                           parameters = param_ve,
                                           bool_by_country = TRUE,
                                           progress = progress,
                                           interruptor = interruptor)


  results_benefit$observed_burden <- Observed_burden(age_specific_covid_data = age_specific_covid,
                                                     aggregated_covid_data = aggregated_covid,
                                                     start_date = timewindow[1],
                                                     end_date = timewindow[2])
  
  param_risk_weight <- param_risk %>%
    filter(str_detect(param, 'Weight background rate source')) %>%
    mutate(source = substr(param, str_length('Weight background rate source')+2,
                           str_length(param))) %>%
    select(-param) %>%
    pivot_longer(cols = -source,
                 names_to = 'scenario',
                 values_to = 'weight')

  param_risk_notification_period <- param_risk %>%
    filter(str_detect(param, 'notification period')) %>%
    select(-param) %>%
    pivot_longer(cols = everything(),
                 names_to = 'scenario',
                 values_to = 'delay')

  results_risk <- Compute_risks(observed_risks,
                                bgr,
                                aggregated_vaccine,
                                param_risk_weight,
                                param_risk_notification_period)

  progress$close()

  return(list(
    results_benefit = results_benefit,
    results_risk = results_risk,
    params = list(param_risk = param_risk, param_ve = param_ve, timewindow = timewindow),
    random = runif(1)
  ))
}

observeEvent(input$btn_finish_parameter_input, {
  
  if(async) {
    hide("btn_finish_parameter_input")
    show("btn_interrupt_computation")
  }
  
  population <- population_data()
  age_specific_covid <- age_specific_covid_data()
  aggregated_covid <- aggregated_covid_data()
  age_specific_vaccine <- age_specific_vaccine_data()
  variant <- variant_data()
  # population <- readRDS('data/population_data_default.rds')
  # age_specific_covid <- readRDS('data/age_specific_covid_data_1updated.rds')
  # aggregated_covid <- readRDS('data/aggregated_covid_data_1updated.rds')
  # age_specific_vaccine <- readRDS('data/age_specific_vaccine_data_1exclude_janssen.rds')
  # variant <- readRDS('data/variant_data_default.rds')
  
  observed_risks <- observed_risks_data()
  bgr <- bgr_data()
  aggregated_vaccine <- aggregated_vaccine_data()
  #observed_risks <- readRDS('data/observed_risks_data_1myocarditis.rds')
  #bgr <- readRDS('data/backgroundrate_risks_data_1myocarditis.rds')
  #aggregated_vaccine <- readRDS('data/vaccine_aggregated_data_1myocarditis.rds')
  
  param_risk <- parameter_risk()
  #param_risk <- read.table('data/default_param_risk.csv', header = TRUE, sep = ';')
  if(ncol(param_risk) > 2) {
    for(i in 2:ncol(param_risk)) {
      param_risk[is.na(param_risk[, i]) | param_risk[, i] == "", i] <- param_risk[is.na(param_risk[, i]) | param_risk[, i] == "", 2]
      param_risk[, i] <- as.numeric(param_risk[, i])
    }
  }
  
  
  param_ve <- parameter_ve()
  #param_ve <- read.table('data/default_param_ve.csv', header = TRUE, sep = ';')
  
  if(ncol(param_ve) > 6) {
    for(i in 7:ncol(param_ve)) {
      param_ve[is.na(param_ve[, i]) | param_ve[, i] == "", i] <- param_ve[is.na(param_ve[, i]) | param_ve[, i] == "", 6]
      param_ve[, i] <- as.numeric(param_ve[, i])
    }
  }
  
  param_ve[, 6] <- as.numeric(param_ve[, 6])
  
  timewindow <- input$input_time_window_analyses
  #timewindow <- as.Date(c('2020-01-01', '2022-02-05'))
  
  cat('Start computation\n')
  
  if(async) {
    progress <- AsyncProgress$new(session, min=0, max=1, message = 'Loading results')
    result_interrupt <- AsyncInterruptor$new()
     
    future({
      compute_results(population, age_specific_covid, aggregated_covid, age_specific_vaccine, variant, observed_risks,
                      bgr, aggregated_vaccine, param_risk, param_ve, timewindow, progress, result_interrupt)
    }) %...>% result_computation
  } else {
    progress <- shiny::Progress$new(min = 0, max = 1)
    progress$set(value = 0, message = paste0('Loading results 0%'))
    on.exit(progress$close())
    
    session$sendCustomMessage("set_active_tab", c(2,6))
    
    compute_results(population, age_specific_covid, aggregated_covid, age_specific_vaccine, variant, observed_risks,
                                         bgr, aggregated_vaccine, param_risk, param_ve, timewindow, progress, NULL) %>%
      result_computation
    
    
  }
  
  NULL
  
})

observeEvent(input$btn_interrupt_computation, {
  if(async) {
    q$producer$fire('interrupt', TRUE)

    show("btn_finish_parameter_input")
    hide("btn_interrupt_computation")
  }
})

obs <- observe({

  res <- result_computation()

  if(!is.null(res$results_benefit)) {
    if(res$results_benefit$interrupted) {
      cat('Interrupted')
    } else {

      outputs$results_benefit <- res$results_benefit
      outputs$results_risk <- res$results_risk
      outputs$params <- res$params

      Show_results()
    }
  }

  return(NULL)
})

Show_results <- function() {
  if(async) {
    session$sendCustomMessage("set_active_tab", c(2,6))
  }
  
  removeCssClass(selector = "a[href$='shiny-tab-parameter_input']", class = "inactiveLink")
  removeCssClass(selector = "a[href$='shiny-tab-results']", class = "inactiveLink")
  
  #### populate select options ####
  vaccines <- unique(outputs$results_risk$vaccine)
  conditions <- outputs$results_risk %>% filter(vaccine == vaccines[1]) %>% pull(condition) %>% as.character %>% unique
  period <- unique(outputs$results_risk$period)
  age_group <- outputs$results_risk %>% filter(vaccine == vaccines[1], condition == conditions[1]) %>% pull(age_group) %>% as.character %>% unique
  imputation <- unique(outputs$results_risk$imputation_method)
  scenario_risk <- unique(outputs$results_risk$scenario)
  scenario_benefit <- unique(outputs$results_benefit$prevented_benefits_daily$scenario)

  updateSelectInput(session, 'option_vaccine', choices = vaccines, selected = vaccines[1])
  updateSelectInput(session, 'option_condition', choices = conditions, selected = conditions[1])
  updateSelectInput(session, 'option_bgr', choices = period, selected = period[1])
  updateSelectInput(session, 'option_age_benefit_risk1', choices = age_group, selected = age_group[1])
  updateSelectInput(session, 'option_imputation', choices = imputation, selected = imputation[1])
  updateSelectInput(session, 'option_scenario_risks', choices = scenario_risk, selected = scenario_risk[1])
  updateSelectInput(session, 'option_scenario_benefits', choices = scenario_benefit, selected = scenario_benefit[1])


  hide("save_unavailable")
  show("save_available")

  hide("save_desc_unavailable")
  show("save_desc_available")
}
# 
# 
# 
# 
#   population <- population_data()
#   age_specific_covid <- age_specific_covid_data()
#   aggregated_covid <- aggregated_covid_data()
#   age_specific_vaccine <- age_specific_vaccine_data()
#   variant <- variant_data()
#   # population <- readRDS('data/population_data_default.rds')
#   # age_specific_covid <- readRDS('data/age_specific_covid_data_1updated.rds')
#   # aggregated_covid <- readRDS('data/aggregated_covid_data_1updated.rds')
#   # age_specific_vaccine <- readRDS('data/age_specific_vaccine_data_1exclude_janssen.rds')
#   # variant <- readRDS('data/variant_data_default.rds')
# 
#   observed_risks <- observed_risks_data()
#   bgr <- bgr_data()
#   aggregated_vaccine <- aggregated_vaccine_data()
#   #observed_risks <- readRDS('data/observed_risks_data_1myocarditis.rds')
#   #bgr <- readRDS('data/backgroundrate_risks_data_1myocarditis.rds')
#   #aggregated_vaccine <- readRDS('data/vaccine_aggregated_data_1myocarditis.rds')
# 
#   param_risk <- parameter_risk()
#   #param_risk <- read.table('data/default_param_risk.csv', header = TRUE, sep = ';')
# 
# 
# 
#   param_ve <- parameter_ve()
#   #param_ve <- read.table('data/default_param_ve.csv', header = TRUE, sep = ';')
# 
#   if(ncol(param_ve) > 6) {
#     for(i in 7:ncol(param_ve)) {
#       param_ve[is.na(param_ve[, i]) | param_ve[, i] == "", i] <- param_ve[is.na(param_ve[, i]) | param_ve[, i] == "", 6]
#       param_ve[, i] <- as.numeric(param_ve[, i])
#     }
#   }
# 
#   param_ve[, 6] <- as.numeric(param_ve[, 6])
# 
#   timewindow <- input$input_time_window_analyses
#   #timewindow <- as.Date(c('2020-01-01', '2022-02-05'))
# 
# 
# 
#   withProgress(message = 'Computing benefits', value = 0, {
# 
#     results_benefit <- Compute_benefits(age_specific_covid_data = age_specific_covid,
#                                         aggregated_covid_data = aggregated_covid,
#                                         age_specific_vaccine_data = age_specific_vaccine,
#                                         variant_data = variant,
#                                         population_data = population,
#                                         start_date = timewindow[1],
#                                         end_date = timewindow[2],
#                                         parameters = param_ve)
# 
#   })
# 
#   results_benefit$observed_burden <- Observed_burden(age_specific_covid_data = age_specific_covid,
#                                                      aggregated_covid_data = aggregated_covid,
#                                                      start_date = timewindow[1],
#                                                      end_date = timewindow[2])
# 
#   outputs$results_benefit <- results_benefit
# 
# 
# 
# 
# 
# 
#   if(ncol(param_risk) > 2) {
#     for(i in 3:ncol(param_risk)) {
#       param_risk[is.na(param_risk[, i]) | param_risk[, i] == "", i] <- param_risk[is.na(param_risk[, i]) | param_risk[, i] == "", 2]
#       param_risk[, i] <- as.numeric(param_risk[, i])
#     }
#   }
# 
#   param_risk[, 2] <- as.numeric(param_risk[, 2])
# 
#   param_risk_weight <- param_risk %>%
#     filter(str_detect(param, 'Weight background rate source')) %>%
#     mutate(source = substr(param, str_length('Weight background rate source')+2,
#                            str_length(param))) %>%
#     select(-param) %>%
#     pivot_longer(cols = -source,
#                  names_to = 'scenario',
#                  values_to = 'weight')
# 
#   param_risk_notification_period <- param_risk %>%
#     filter(str_detect(param, 'notification period')) %>%
#     select(-param) %>%
#     pivot_longer(cols = everything(),
#                  names_to = 'scenario',
#                  values_to = 'delay')
# 
#   results_risk <- Compute_risks(observed_risks,
#                                 bgr,
#                                 aggregated_vaccine,
#                                 param_risk_weight,
#                                 param_risk_notification_period)
# 
#   outputs$results_risk <- results_risk
# 
# 
# 
# 
#   #### populate select options ####
#   vaccines <- unique(aggregated_vaccine$vaccine)
#   conditions <- observed_risks %>% filter(vaccine == vaccines[1]) %>% pull(condition) %>% as.character %>% unique
#   period <- unique(bgr$period)
#   age_group <- observed_risks %>% filter(vaccine == vaccines[1], condition == conditions[1]) %>% pull(age_group) %>% as.character %>% unique
#   imputation <- unique(aggregated_vaccine$imputation_method)
#   scenario_risk <- unique(results_risk$scenario)
#   scenario_benefit <- unique(results_benefit$prevented_benefits_daily$scenario)
# 
#   updateSelectInput(session, 'option_vaccine', choices = as.character(unique(aggregated_vaccine$vaccine)), selected = vaccines[1])
#   updateSelectInput(session, 'option_condition', choices = conditions, selected = conditions[1])
#   updateSelectInput(session, 'option_bgr', choices = period, selected = period[1])
#   updateSelectInput(session, 'option_age_benefit_risk1', choices = age_group, selected = age_group[1])
#   updateSelectInput(session, 'option_imputation', choices = imputation, selected = imputation[1])
#   updateSelectInput(session, 'option_scenario_risks', choices = scenario_risk, selected = scenario_risk[1])
#   updateSelectInput(session, 'option_scenario_benefits', choices = scenario_benefit, selected = scenario_benefit[1])
# })

observeEvent(input$option_vaccine, {
  if(input$option_vaccine == '') return();

  conditions <- outputs$results_risk %>% filter(vaccine == input$option_vaccine) %>% pull(condition) %>% as.character %>% unique
  updateSelectInput(session, 'option_condition', choices = conditions)
})

observeEvent(input$option_condition, {
  if(input$option_vaccine == '' | input$option_condition == '') return();

  age_group <- outputs$results_risk %>% filter(vaccine == input$option_vaccine, condition == input$option_condition) %>% pull(age_group) %>% as.character %>% unique
  updateSelectInput(session, 'option_age_benefit_risk1', choices = age_group)
})

observeEvent(input$risk1_plot_grouping, {
  if(input$risk1_plot_grouping == 'sex') {
    hide("sex_risk1plot_container")
  } else {
    show("sex_risk1plot_container")
  }
})