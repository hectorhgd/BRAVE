parameter_risk <- reactive({
  
  Text_to_parameter(input$parameter_risk_textarea, c(1, 3, 4, 5))
  
})

output$btn_download_template_risk <- downloadHandler(
  filename = function() {
    'param_risk.csv'
  },
  content = function(con) {
    write.table(parameter_risk(), con, sep = ';', row.names = FALSE)
  }
)

observeEvent(input$btn_add_scenario_risk, {
  param <- parameter_risk()
  num_scenario <- ncol(param)-1
  
  param[[paste0('scenario_', num_scenario + 1)]] <- NA
  
  Render_parameter_input(session,
                         param,
                         1,
                         'parameter_risk',
                         'parameter_risk_table_container',
                         'parameter_risk_textarea')
  
})

observeEvent(input$input_upload_parameter_risk, {
  
  param <- Load_data('Use uploaded dataset', input$input_upload_parameter_risk)
  
  param <- param_risk_template %>% 
    left_join(param) %>%
    group_by(param) %>%
    slice(1) %>%
    arrange(param)
  
  Render_parameter_input(session,
                         param,
                         1,
                         'parameter_risk',
                         'parameter_risk_table_container',
                         'parameter_risk_textarea')
})

initialize_parameter_status <- TRUE
observeEvent(input$parameter_ve_textarea, {
  if(initialize_parameter_status) {
    Validate_parameters_input()
  }
})

parameter_ve <- reactive({

  if(is.null(input$parameter_ve_textarea)) {
    return(NULL)
  }
  
  Text_to_parameter(input$parameter_ve_textarea, c(1, 3, 4, 5))

})

output$btn_download_template_ve <- downloadHandler(
  filename = function() {
    'vaccine_efficacy.csv'
  },
  content = function(con) {
    cat('test');
    write.table(parameter_ve(), con, sep = ';', row.names = FALSE)
  }
)

observeEvent(input$btn_add_scenario_ve, {
  param <- parameter_ve()
  num_scenario <- ncol(param)-5
  
  param[[paste0('scenario_', num_scenario + 1)]] <- NA
  
  Render_parameter_input(session,
                         param,
                         5,
                         'parameter_ve',
                         'parameter_ve_table_container',
                         'parameter_ve_textarea')
  
})

observeEvent(input$input_upload_parameter_ve, {
  
  param <- Load_data('Use uploaded dataset', input$input_upload_parameter_ve)
  
  param <- param_ve_template %>% 
    left_join(param) %>%
    group_by(vaccine, dose, variant, against, parameter) %>%
    slice(1) %>%
    arrange(parameter, vaccine, dose, against)
  
  Render_parameter_input(session,
                         param,
                         5,
                         'parameter_ve',
                         'parameter_ve_table_container',
                         'parameter_ve_textarea')
})

observeEvent(input$menu, {
  
  if(input$menu == 'parameters_finish') {
    
    Validate_parameters_input()
    
  }
})

Validate_parameters_input <- function() {
  risk <- parameter_risk()
  risk_valid <- FALSE
  if(ncol(risk) >= 2) {
    if(any(is.na(risk[, 2]))) {
      html('status_parameter_risks', "Missing required parameters")
      removeClass('status_parameter_risks', "msgvalid")
      addClass('status_parameter_risks', "msginvalid")
    } else {
      
      if(any(colnames(risk) == '')) {
        html('status_parameter_risks', "Missing scenario name")
        removeClass('status_parameter_risks', "msgvalid")
        addClass('status_parameter_risks', "msginvalid")
      } else {
        html('status_parameter_risks', "Ready")
        removeClass('status_parameter_risks', "msginvalid")
        addClass('status_parameter_risks', "msgvalid")
        risk_valid <- TRUE
      }
    }
  } else {
    html('status_parameter_risks', "Missing required parameters")
    removeClass('status_parameter_risks', "msgvalid")
    addClass('status_parameter_risks', "msginvalid")
  }
  
  initialize_parameter_status <- FALSE
  
  benefit <- parameter_ve()
  benefit_valid <- FALSE
  if(is.null(benefit)) {
    html('status_parameter_benefit', "Benefit parameters not found")
    removeClass('status_parameter_benefit', "msgvalid")
    addClass('status_parameter_benefit', "msginvalid")
    
    initialize_parameter_status <- TRUE
  } 
  else if(ncol(benefit) >= 6) {
    if(any(is.na(benefit[, 6]))) {
      html('status_parameter_benefit', "Missing required parameters")
      removeClass('status_parameter_benefit', "msgvalid")
      addClass('status_parameter_benefit', "msginvalid")
    } else {
      
      if(any(colnames(benefit) == '')) {
        html('status_parameter_benefit', "Missing scenario name")
        removeClass('status_parameter_benefit', "msgvalid")
        addClass('status_parameter_benefit', "msginvalid")
      } else {
        html('status_parameter_benefit', "Ready")
        removeClass('status_parameter_benefit', "msginvalid")
        addClass('status_parameter_benefit', "msgvalid")
        benefit_valid <- TRUE
      }
    }
  } else {
    html('status_parameter_benefit', "Missing required parameters")
    removeClass('status_parameter_benefit', "msgvalid")
    addClass('status_parameter_benefit', "msginvalid")
  }
  
  
  if(!benefit_valid | !risk_valid) {
    shinyjs::disable("btn_finish_parameter_input")
  } else {
    shinyjs::enable("btn_finish_parameter_input")
    
    if(auto_proceed_to_results) {
      click("btn_finish_parameter_input")
    }
    
  }
}
