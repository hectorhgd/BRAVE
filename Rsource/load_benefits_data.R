#### Load benefits data ####

#### Population data ####

population_datasets <- list.files("data/", pattern = "^population_data_.*.rds$")
if(!is.null(population_datasets)) {
  name_clean <- str_replace(population_datasets, 'population_data_', '') %>%
    str_replace('.rds$', '')
  
  names(population_datasets) <- name_clean
  
  updateSelectInput(session, 
                    'select_upload_benefits_population', 
                    choices = population_datasets, 
                    selected = population_datasets[1])
}

observeEvent(input$input_upload_benefits_population, {
  options <- c('Use uploaded dataset', population_datasets)
  names(options)[1] = 'Use uploaded dataset'
  
  updateSelectInput(session, 
                    'select_upload_benefits_population', 
                    choices = options, 
                    selected = 'Use uploaded dataset')
})

population_data <- reactive({
  
  selected <- input$select_upload_benefits_population
  
  return(Load_data(selected, input$input_upload_benefits_population))
  
})

output$preview_upload_benefits_population <- DT::renderDataTable(DT::datatable({
  
  population_data()
  
  
}, options = list(searching = FALSE,
                  paging = TRUE,
                  pageLength =  5,
                  lengthChange = FALSE)))

#### Granular COVID data ####

age_specific_covid_datasets <- list.files("data/", pattern = "^age_specific_covid_data_.*.rds$")
if(!is.null(age_specific_covid_datasets)) {
  name_clean <- str_replace(age_specific_covid_datasets, 'age_specific_covid_data_', '') %>%
    str_replace('.rds$', '')
  
  names(age_specific_covid_datasets) <- name_clean
  
  updateSelectInput(session, 
                    'select_upload_benefits_age_specific_covid_data', 
                    choices = age_specific_covid_datasets, 
                    selected = age_specific_covid_datasets[1])
}

observeEvent(input$input_upload_benefits_age_specific_covid_data, {
  cat(age_specific_covid_datasets)
  
  print(input$input_upload_benefits_age_specific_covid_data)
  
  options <- c('Use uploaded dataset', age_specific_covid_datasets)
  names(options)[1] = 'Use uploaded dataset'
  
  updateSelectInput(session, 
                    'select_upload_benefits_age_specific_covid_data', 
                    choices = options, 
                    selected = 'Use uploaded dataset')
})

age_specific_covid_data <- reactive({
  
  selected <- input$select_upload_benefits_age_specific_covid_data
  
  return(Load_data(selected, input$input_upload_benefits_age_specific_covid_data))
  
})

output$preview_upload_benefits_age_specific_covid_data <- DT::renderDataTable(DT::datatable({
  
  age_specific_covid_data()
  
  
}, options = list(searching = FALSE,
                  paging = TRUE,
                  pageLength =  5,
                  lengthChange = FALSE)))

#### Aggregated COVID data ####

aggregated_covid_datasets <- list.files("data/", pattern = "^aggregated_covid_data_.*.rds$")
if(!is.null(aggregated_covid_datasets)) {
  name_clean <- str_replace(aggregated_covid_datasets, 'aggregated_covid_data_', '') %>%
    str_replace('.rds$', '')
  
  names(aggregated_covid_datasets) <- name_clean
  
  updateSelectInput(session, 
                    'select_upload_benefits_aggregated_covid_data', 
                    choices = aggregated_covid_datasets, 
                    selected = aggregated_covid_datasets[1])
}

observeEvent(input$input_upload_benefits_aggregated_covid_data, {
  cat(aggregated_covid_datasets)
  
  print(input$input_upload_benefits_aggregated_covid_data)
  
  options <- c('Use uploaded dataset', aggregated_covid_datasets)
  names(options)[1] = 'Use uploaded dataset'
  
  updateSelectInput(session, 
                    'select_upload_benefits_aggregated_covid_data', 
                    choices = options, 
                    selected = 'Use uploaded dataset')
})

aggregated_covid_data <- reactive({
  
  selected <- input$select_upload_benefits_aggregated_covid_data
  
  return(Load_data(selected, input$input_upload_benefits_aggregated_covid_data))
  
})

output$preview_upload_benefits_aggregated_covid_data <- DT::renderDataTable(DT::datatable({
  
  aggregated_covid_data()
  
  
}, options = list(searching = FALSE,
                  paging = TRUE,
                  pageLength =  5,
                  lengthChange = FALSE)))

#### Variant data ####

variant_datasets <- list.files("data/", pattern = "^variant_data_.*.rds$")
if(!is.null(variant_datasets)) {
  name_clean <- str_replace(variant_datasets, 'variant_data_', '') %>%
    str_replace('.rds$', '')
  
  names(variant_datasets) <- name_clean
  
  updateSelectInput(session, 
                    'select_upload_benefits_variant_data', 
                    choices = variant_datasets, 
                    selected = variant_datasets[1])
}

observeEvent(input$input_upload_benefits_variant_data, {
  cat(variant_datasets)
  
  print(input$input_upload_benefits_variant_data)
  
  options <- c('Use uploaded dataset', variant_datasets)
  names(options)[1] = 'Use uploaded dataset'
  
  updateSelectInput(session, 
                    'select_upload_benefits_variant_data', 
                    choices = options, 
                    selected = 'Use uploaded dataset')
})

variant_data <- reactive({
  
  selected <- input$select_upload_benefits_variant_data
  
  return(Load_data(selected, input$input_upload_benefits_variant_data))
  
})

output$preview_upload_benefits_variant_data <- DT::renderDataTable(DT::datatable({
  
  variant_data()
  
  
}, options = list(searching = FALSE,
                  paging = TRUE,
                  pageLength =  5,
                  lengthChange = FALSE)))

#### Age specific vaccine data ####

age_specific_vaccine_datasets <- list.files("data/", pattern = "^age_specific_vaccine_data_.*.rds$")
if(!is.null(age_specific_vaccine_datasets)) {
  name_clean <- str_replace(age_specific_vaccine_datasets, 'age_specific_vaccine_data_', '') %>%
    str_replace('.rds$', '')
  
  names(age_specific_vaccine_datasets) <- name_clean
  
  updateSelectInput(session, 
                    'select_upload_benefits_age_specific_vaccine_data', 
                    choices = age_specific_vaccine_datasets, 
                    selected = age_specific_vaccine_datasets[1])
}

observeEvent(input$input_upload_benefits_age_specific_vaccine_data, {
  cat(age_specific_vaccine_datasets)
  
  print(input$input_upload_benefits_age_specific_vaccine_data)
  
  options <- c('Use uploaded dataset', age_specific_vaccine_datasets)
  names(options)[1] = 'Use uploaded dataset'
  
  updateSelectInput(session, 
                    'select_upload_benefits_age_specific_vaccine_data', 
                    choices = options, 
                    selected = 'Use uploaded dataset')
})

age_specific_vaccine_data <- reactive({
  
  selected <- input$select_upload_benefits_age_specific_vaccine_data
  
  return(Load_data(selected, input$input_upload_benefits_age_specific_vaccine_data))
  
})

output$preview_upload_benefits_age_specific_vaccine_data <- DT::renderDataTable(DT::datatable({
  
  age_specific_vaccine_data()
  
  
}, options = list(searching = FALSE,
                  paging = TRUE,
                  pageLength =  5,
                  lengthChange = FALSE)))


output$btn_download_benefits_population <- downloadHandler(
  filename = "benefits_population.csv",
  content = function(file) {
    write.table(population_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

output$btn_download_benefits_age_specific_covid <- downloadHandler(
  filename = "benefits_granular_covid_data.csv",
  content = function(file) {
    write.table(age_specific_covid_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

output$btn_download_benefits_aggregated_covid <- downloadHandler(
  filename = "benefits_aggregated_covid_data.csv",
  content = function(file) {
    write.table(aggregated_covid_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

output$btn_download_benefits_variant <- downloadHandler(
  filename = "benefits_variants_of_concern.csv",
  content = function(file) {
    write.table(variant_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

output$btn_download_benefits_granular_vaccine <- downloadHandler(
  filename = "benefits_granular_vaccine.csv",
  content = function(file) {
    write.table(age_specific_vaccine_data(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)





