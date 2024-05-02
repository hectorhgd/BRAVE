observeEvent(input$btn_finish_data_upload, {
  session$sendCustomMessage("set_active_tab", c(1,3))
  
  addCssClass(selector = "a[href$='#shiny-tab-data_upload']", class = "inactiveLink")
  removeCssClass(selector = "a[href$='shiny-tab-parameter_input']", class = "inactiveLink")
  
  ## risk
  param_risk_template <- data.frame(param = c('Risk notification period (in days)',
                                              paste0('Weight background rate source ', unique(bgr_data()$source)))) %>%
    mutate(param = as.character(param))
  
  default_param_risk <- read.table('data/default_param_risk.csv', sep = ';', header = TRUE)
  
  param_risk <- param_risk_template %>%
    left_join(default_param_risk) %>%
    group_by(param) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(param)
  
  Render_parameter_input(session,
                         param_risk,
                         1,
                         'parameter_risk',
                         'parameter_risk_table_container',
                         'parameter_risk_textarea')
  
  ## benefit
  vaccine_dose <- age_specific_vaccine_data() %>% select(vaccine, dose) %>% distinct %>%
    mutate(vaccine = as.character(vaccine))
  variant <- unique(variant_data()$variant)
  
  param_ve_template <- expand.grid(list(
    vacc_dose_id = 1:nrow(vaccine_dose),
    variant = variant,
    against = c('cases', 'hospitalization', 'icu', 'mortality'),
    parameter = c('efficacy', 'protection_delay', 'waning_delay')
  )) %>%
    mutate(vaccine = vaccine_dose$vaccine[vacc_dose_id],
           dose = vaccine_dose$dose[vacc_dose_id]) %>%
    select(vaccine, dose, variant, against, parameter) %>%
    arrange(parameter, vaccine, dose, against)
  
  default_param_ve <- read.table('data/default_param_ve.csv', sep = ';', header = TRUE, stringsAsFactors = FALSE)

  param_ve <- param_ve_template %>%
    left_join(default_param_ve) %>%
    group_by(vaccine, dose, variant, against, parameter) %>%
    slice(1) %>%
    arrange(parameter, vaccine, dose, against)
  
  Render_parameter_input(session,
                         param_ve,
                         5,
                         'parameter_ve',
                         'parameter_ve_table_container',
                         'parameter_ve_textarea')
})


