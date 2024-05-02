View_parameters_risks <- function() {
  tagList(
    h2("Parameters risk"),
    tags$table(tags$tr(
      tags$td(h4("Upload parameter file:")),
      tags$td(width = "10px"),
      tags$td(fileInput(inputId = "input_upload_parameter_risk", label = NULL, accept = c('.csv', '.rds')))
    ),
    tags$tr(
      tags$td(downloadButton('btn_download_template_risk', 'Download template')),
      tags$td(width = "10px"),
      tags$td(actionButton('btn_add_scenario_risk', 'Add scenario')),
    )),
    tags$br(),
    tags$div(id = 'parameter_risk_table_container', '')
  )
}

View_parameters_benefits <- function() {
  tagList(
    h2("Parameters vaccine efficacy"),
    tags$table(tags$tr(
      tags$td(h4("Upload parameter file:")),
      tags$td(width = "10px"),
        tags$td(fileInput(inputId = "input_upload_parameter_ve", label = NULL, accept = c('.csv', '.rds')))
      ),
      tags$tr(
        tags$td(downloadButton('btn_download_template_ve', 'Download template')),
        tags$td(width = "10px"),
        tags$td(actionButton('btn_add_scenario_ve', 'Add scenario')),
      )),
    tags$br(),
    tags$div(id = 'parameter_ve_table_container', '')
  )
}

View_parameters_finish <- function() {
  tagList(
    h3("Finish parameter input"),
    tags$table(tags$tr(
      tags$td(h4("Risk status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_parameter_risks", "Loading"))
    ),
    tags$tr(
      tags$td(h4("Benefit status:")),
      tags$td(width = "10px"),
      tags$td(h4(id = "status_parameter_benefit", "Loading"))
    )),
    tags$br(),
    actionButton("btn_finish_parameter_input", "Generate results"),
    actionButton("btn_interrupt_computation", "Cancel computation")
  )
}