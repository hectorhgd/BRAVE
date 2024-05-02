#### Load risks data ####

#### Observed risk after vaccination ####

observed_risks_datasets <- list.files("data/", pattern = "^observed_risks_data_.*.rds$")
if(!is.null(observed_risks_datasets)) {
  name_clean <- str_replace(observed_risks_datasets, 'observed_risks_data_', '') %>%
    str_replace('.rds$', '')

  names(observed_risks_datasets) <- name_clean

  updateSelectInput(session,
                    'select_upload_risks_observed',
                    choices = observed_risks_datasets,
                    selected = observed_risks_datasets[1])
}

observeEvent(input$input_upload_risks_observed, {
  options <- c('Use uploaded dataset', observed_risks_datasets)
  names(options)[1] = 'Use uploaded dataset'

  updateSelectInput(session,
                    'select_upload_risks_observed',
                    choices = options,
                    selected = 'Use uploaded dataset')
})

observed_risks_data <- reactive({

  selected <- input$select_upload_risks_observed

  return(Load_data(selected, input$input_upload_risks_observed))

})



output$preview_upload_risks_observed <- DT::renderDataTable(DT::datatable({

  observed_risks_data()


}, options = list(searching = FALSE,
                  paging = TRUE,
                  pageLength =  5,
                  lengthChange = FALSE)))


#### Background risk risks ####

bgr_datasets <- list.files("data/", pattern = "^backgroundrate_risks_data_.*.rds$")
if(!is.null(bgr_datasets)) {
  name_clean <- str_replace(bgr_datasets, 'backgroundrate_risks_data_', '') %>%
    str_replace('.rds$', '')

  names(bgr_datasets) <- name_clean

  updateSelectInput(session,
                    'select_upload_risks_bgr',
                    choices = bgr_datasets,
                    selected = bgr_datasets[1])
}


observeEvent(input$input_upload_risks_bgr, {
  options <- c('Use uploaded dataset', bgr_datasets)
  names(options)[1] = 'Use uploaded dataset'

  updateSelectInput(session,
                    'select_upload_risks_bgr',
                    choices = options,
                    selected = 'Use uploaded dataset')
})

bgr_data <- reactive({

  selected <- input$select_upload_risks_bgr

  return(Load_data(selected, input$input_upload_risks_bgr))

})

output$preview_upload_risks_bgr <- DT::renderDataTable(DT::datatable({

  bgr_data()


}, options = list(searching = FALSE,
                  paging = TRUE,
                  pageLength =  5,
                  lengthChange = FALSE)))

#### Aggregated vaccine risks ####

aggregated_vaccine_datasets <- list.files("data/", pattern = "^vaccine_aggregated_data_.*.rds$")
if(!is.null(aggregated_vaccine_datasets)) {
  name_clean <- str_replace(aggregated_vaccine_datasets, 'vaccine_aggregated_data_', '') %>%
    str_replace('.rds$', '')

  names(aggregated_vaccine_datasets) <- name_clean

  updateSelectInput(session,
                    'select_upload_risks_aggregated_vaccine',
                    choices = aggregated_vaccine_datasets,
                    selected = aggregated_vaccine_datasets[1])
}


observeEvent(input$input_upload_risks_aggregated_vaccine, {
  options <- c('Use uploaded dataset', aggregated_vaccine_datasets)
  names(options)[1] = 'Use uploaded dataset'

  updateSelectInput(session,
                    'select_upload_risks_aggregated_vaccine',
                    choices = options,
                    selected = 'Use uploaded dataset')
})

aggregated_vaccine_data <- reactive({

  selected <- input$select_upload_risks_aggregated_vaccine

  return(Load_data(selected, input$input_upload_risks_aggregated_vaccine))

})

output$preview_upload_risks_aggregated_vaccine <- DT::renderDataTable(DT::datatable({

  aggregated_vaccine_data()


}, options = list(searching = FALSE,
                  paging = TRUE,
                  pageLength =  5,
                  lengthChange = FALSE)))

output$btn_download_risk_observed <- downloadHandler(
  filename = "risk_observed.csv",
  content = function(file) {
    write.table(observed_risks_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

output$btn_download_risk_observed <- downloadHandler(
  filename = "risk_observed.csv",
  content = function(file) {
    write.table(observed_risks_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

output$btn_download_risk_bgr <- downloadHandler(
  filename = "risk_background_rate.csv",
  content = function(file) {
    write.table(bgr_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

output$btn_download_risk_aggregated_vaccine <- downloadHandler(
  filename = "risk_aggregated_vaccine.csv",
  content = function(file) {
    write.table(aggregated_vaccine_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)