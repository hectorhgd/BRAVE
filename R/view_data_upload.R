View_data_upload_risks <- function() {
  tagList(
    h2("Observed risks after vaccination"),
    tags$table(tags$tr(
      tags$td(h4("Use saved dataset:")),
      tags$td(width = "10px"),
      tags$td(selectInput(inputId = "select_upload_risks_observed", label = NULL,choices = c('')))),
      tags$tr(
        tags$td(h4("Upload custom dataset:")),
        tags$td(width = "10px"),
        tags$td(fileInput(inputId = "input_upload_risks_observed", label = NULL, accept = c('.csv', '.rds')))
      ),
      tags$tr(
        tags$td(downloadButton('btn_download_risk_observed', 'Download data')),
        tags$td(),
        tags$td()
      )),
    tags$table(tags$tr(
      tags$td(h4("Status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_risks_observed", "Ready"))
    )),
    p(id = 'error_msg_upload_risks_observed',
      style = 'color: red'),
    DT::dataTableOutput('preview_upload_risks_observed'),
    tags$hr(style="border-color: black;"),
    
    h2("Background rate risks"),
    tags$table(tags$tr(
      tags$td(h4("Use saved dataset:")),
      tags$td(width = "10px"),
      tags$td(selectInput(inputId = "select_upload_risks_bgr", label = NULL,choices = c('')))),
      tags$tr(
        tags$td(h4("Upload custom dataset:")),
        tags$td(width = "10px"),
        tags$td(fileInput(inputId = "input_upload_risks_bgr", label = NULL, accept = c('.csv', '.rds')))
      ),
      tags$tr(
        tags$td(downloadButton('btn_download_risk_bgr', 'Download data')),
        tags$td(),
        tags$td()
      )),
    tags$table(tags$tr(
      tags$td(h4("Status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_risks_bgr", "Ready"))
    )),
    p(id = 'error_msg_upload_risks_bgr',
      style = 'color: red'),
    DT::dataTableOutput('preview_upload_risks_bgr'),
    tags$hr(style="border-color: black;"),
    
    h2("Aggregated vaccine data (imputed)"),
    tags$table(tags$tr(
      tags$td(h4("Use saved dataset:")),
      tags$td(width = "10px"),
      tags$td(selectInput(inputId = "select_upload_risks_aggregated_vaccine", label = NULL,choices = c('')))),
      tags$tr(
        tags$td(h4("Upload custom dataset:")),
        tags$td(width = "10px"),
        tags$td(fileInput(inputId = "input_upload_risks_aggregated_vaccine", label = NULL, accept = c('.csv', '.rds')))
      ),
      tags$tr(
        tags$td(downloadButton('btn_download_risk_aggregated_vaccine', 'Download data')),
        tags$td(),
        tags$td()
      )),
    tags$table(tags$tr(
      tags$td(h4("Status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_risks_aggregated_vaccine", "Ready"))
    )),
    p(id = 'error_msg_upload_risks_aggregated_vaccine',
      style = 'color: red'),
    DT::dataTableOutput('preview_upload_risks_aggregated_vaccine'),
    tags$hr(style="border-color: black;")
  )
}

View_data_upload_benefits <- function() {
  tagList(
    h2("Country demographics data"),
    tags$table(tags$tr(
      tags$td(h4("Use saved dataset:")),
      tags$td(width = "10px"),
      tags$td(selectInput(inputId = "select_upload_benefits_population", label = NULL,choices = c('')))),
      tags$tr(
        tags$td(h4("Upload custom dataset:")),
        tags$td(width = "10px"),
        tags$td(fileInput(inputId = "input_upload_benefits_population", label = NULL, accept = c('.csv', '.rds')))
      )),
    tags$table(tags$tr(
      tags$td(h4("Status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_benefits_population", "Ready"))
    ),
    tags$tr(
      tags$td(downloadButton('btn_download_benefits_population', 'Download data')),
      tags$td(),
      tags$td()
    )),
    p(id = 'error_msg_upload_benefits_population',
      style = 'color: red'),
    DT::dataTableOutput('preview_upload_benefits_population'),
    tags$hr(style="border-color: black;"),
    
    h2("Granular COVID19 data"),
    tags$table(tags$tr(
      tags$td(h4("Use saved dataset:")),
      tags$td(width = "10px"),
      tags$td(selectInput(inputId = "select_upload_benefits_age_specific_covid_data", label = NULL,choices = c('')))),
      tags$tr(
      tags$td(h4("Upload custom dataset:")),
      tags$td(width = "10px"),
      tags$td(fileInput(inputId = "input_upload_benefits_age_specific_covid_data", label = NULL, accept = c('.csv', '.rds')))
    )),
    tags$table(tags$tr(
      tags$td(h4("Status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_benefits_age_specific_covid", "Ready"))
      ),
      tags$tr(
        tags$td(downloadButton('btn_download_benefits_age_specific_covid', 'Download data')),
        tags$td(),
        tags$td()
      )),
    p(id = 'error_msg_upload_benefits_age_specific_covid',
      style = 'color: red'),
    DT::dataTableOutput('preview_upload_benefits_age_specific_covid_data'),
    tags$hr(style="border-color: black;"),
    
    h2("Aggregated COVID19 data"),
    tags$table(tags$tr(
      tags$td(h4("Use saved dataset:")),
      tags$td(width = "10px"),
      tags$td(selectInput(inputId = "select_upload_benefits_aggregated_covid_data", label = NULL,choices = c('')))),
      tags$tr(
        tags$td(h4("Upload custom dataset:")),
        tags$td(width = "10px"),
        tags$td(fileInput(inputId = "input_upload_benefits_aggregated_covid_data", label = NULL, accept = c('.csv', '.rds')))
      ),
      tags$tr(
        tags$td(downloadButton('btn_download_benefits_aggregated_covid', 'Download data')),
        tags$td(),
        tags$td()
      )),
    tags$table(tags$tr(
      tags$td(h4("Status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_benefits_aggregated_covid", "Ready"))
    )),
    p(id = 'error_msg_upload_benefits_aggregated_covid',
      style = 'color: red'),
    DT::dataTableOutput('preview_upload_benefits_aggregated_covid_data'),
    tags$hr(style="border-color: black;"),
  
    h2("COVID19 variants of concern data"),
    tags$table(tags$tr(
      tags$td(h4("Use saved dataset:")),
      tags$td(width = "10px"),
      tags$td(selectInput(inputId = "select_upload_benefits_variant_data", label = NULL,choices = c('')))),
      tags$tr(
        tags$td(h4("Upload custom dataset:")),
        tags$td(width = "10px"),
        tags$td(fileInput(inputId = "input_upload_benefits_variant_data", label = NULL, accept = c('.csv', '.rds')))
      ),
      tags$tr(
        tags$td(downloadButton('btn_download_benefits_variant', 'Download data')),
        tags$td(),
        tags$td()
      )),
    tags$table(tags$tr(
      tags$td(h4("Status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_benefits_variant", "Ready"))
    )),
    p(id = 'error_msg_upload_benefits_variant',
      style = 'color: red'),
    DT::dataTableOutput('preview_upload_benefits_variant_data'),
    tags$hr(style="border-color: black;"),
    
    
    h2("Granular vaccine data"),
    tags$table(tags$tr(
      tags$td(h4("Use saved dataset:")),
      tags$td(width = "10px"),
      tags$td(selectInput(inputId = "select_upload_benefits_age_specific_vaccine_data", label = NULL,choices = c('')))),
      tags$tr(
        tags$td(h4("Upload custom dataset:")),
        tags$td(width = "10px"),
        tags$td(fileInput(inputId = "input_upload_benefits_age_specific_vaccine_data", label = NULL, accept = c('.csv', '.rds')))
      )),
    tags$table(tags$tr(
      tags$td(h4("Status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_benefits_age_specific_vaccine", "Ready"))
    ),
    tags$tr(
      tags$td(downloadButton('btn_download_benefits_granular_vaccine', 'Download data')),
      tags$td(),
      tags$td()
    )),
    p(id = 'error_msg_upload_benefits_age_specific_vaccine',
      style = 'color: red'),
    DT::dataTableOutput('preview_upload_benefits_age_specific_vaccine_data'),
    tags$hr(style="border-color: black;")
  )
}

View_data_upload_finish <- function() {
  tagList(
    h3("Finish data upload"),
    tags$table(tags$tr(
      tags$td(h4("Risk status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_risks", "Ready"))
    ),
    tags$tr(
      tags$td(h4("Benefit status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_upload_benefits", "Ready"))
    )),
    tags$br(),
    h4("Timewindow:"),
    p("Move the slider to indicate the time window in which the vaccination and risk cases were recorded in the risk data sets"),
    sliderInput("input_time_window_analyses",
                NULL,
                min = -Inf,
                max = Inf,
                value = c(-Inf, Inf),
                timeFormat = "%Y-%m-%d"),
    actionButton("btn_finish_data_upload", "Proceed to selecting parameters")
  )
}