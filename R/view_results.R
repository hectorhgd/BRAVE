View_results <- function() {
  tagList(
    ### Risks
    div(id = 'tab_risks',
        fluidRow(column(width = 6, 
                        tabBox(
                          id = "risks1",
                          title = "Risks O/E",
                          height = "390px",
                          width = '100%',
                          tabPanel("Figure", 
                                   icon = icon('chart-bar'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     downloadButton("download_risk1plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download"),
                                     dropdown(
                                       radioGroupButtons(
                                         inputId = "risk1_plot_grouping",
                                         label = "Group by", 
                                         choiceNames = c("not grouped", "Vaccine", "Sex", "Condition", "Imputation method", "Background rate", "Scenario"),
                                         choiceValues = c("", "vaccine", "sex", "condition", "imputation_method", "period", "scenario"), 
                                         selected = "sex", 
                                         direction = "vertical"
                                       ),
                                       size = "xs",
                                       icon = icon("gear", class = "opt"), 
                                       up = TRUE,
                                       inputId = 'dropdown_option_risk1plot'),
                                     div(
                                       id = 'sex_risk1plot_container',
                                       class = 'settings_row',
                                       selectInput('option_sex_risk1plot', 'Sex:', choices = c('Male', 'Female'), selected = 'Male')
                                     )
                                     ),
                                   plotOutput('risks1_plot', height = 300)
                          ),
                          tabPanel("Data", 
                                   icon = icon('table'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     class = "settings_row",
                                     downloadButton("download_risks1_data", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                                   ),
                                   DT::dataTableOutput('risks1_data'),
                                   style = "height:300px; overflow-y: auto;overflow-x: auto;",
                                   width = '100%')
                        )
        ),
        column(width = 6, 
               tabBox(
                 id = "risks2",
                 title = "Number of risk cases",
                 height = "390px",
                 width = '100%',
                 tabPanel("Figure", 
                          icon = icon('chart-bar'),
                          div(
                            style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                            class = "settings_row",
                            downloadButton("download_risk2plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download"),
                            selectInput('option_sex_risk2plot', 'Sex:', choices = c('Male', 'Female'), selected = 'Male')
                          ),
                          plotOutput('risks2_plot', height = 300)
                 ),
                 tabPanel("Data", 
                          icon = icon('table'),
                          div(
                            style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                            class = "settings_row",
                            downloadButton("download_risks2_data", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                          ),
                          DT::dataTableOutput('risks2_data'),
                          style = "height:300px; overflow-y: auto;overflow-x: auto;",
                          width = '100%')
               )
        ))
    ),
    
    ### Benefits
    div(id = 'tab_benefits',
        fluidRow(column(width = 6, 
                        tabBox(
                          id = "benefits1",
                          title = "Prevented total",
                          height = "420px",
                          width = '100%',
                          tabPanel("Confirmed cases",
                                   icon = icon('plus-circle'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     class = "settings_row",
                                     downloadButton("download_benefits1_case_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                                   ),
                                   plotOutput('benefits1_case_plot', height = 300)
                          ),
                          tabPanel("Hospitalizations",
                                   icon = icon('hospital'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     class = "settings_row",
                                     downloadButton("download_benefits1_hospital_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                                   ),
                                   plotOutput('benefits1_hospital_plot', height = 300)
                          ),
                          tabPanel("ICU",
                                   icon = icon('procedures'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     class = "settings_row",
                                     downloadButton("download_benefits1_icu_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                                   ),
                                   plotOutput('benefits1_icu_plot', height = 300)
                          ),
                          tabPanel("Mortality",
                                   icon = icon('cross'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     class = "settings_row",
                                     downloadButton("download_benefits1_mortality_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                                   ),
                                   plotOutput('benefits1_mortality_plot', height = 300)
                          ),
                          tabPanel("Data",
                                   icon = icon('table'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     class = "settings_row",
                                     downloadButton("download_benefits1_data", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                                   ),
                                   DT::dataTableOutput('benefits1_data'),
                                   style = "height:300px; overflow-y: auto;overflow-x: auto;",
                                   width = '100%')
                        )
        ),
        column(width = 6, 
               div(
                 style = "position: absolute; left:1.5em; bottom: 2em;",
                 dropdown(
                   radioGroupButtons(
                     inputId = "benefit2_plot_grouping",
                     label = "Compare", 
                     choiceNames = c("no comparison", "Vaccine", "Scenario"),
                     choiceValues = c("", "vaccine", "scenario"), 
                     selected = "", 
                     direction = "vertical"
                   ),
                   size = "xs",
                   icon = icon("gear", class = "opt"), 
                   up = TRUE
                 )),
               tabBox(
                 id = "benefits2",
                 title = "Prevented per 100 000",
                 height = "420px",
                 width = '100%',
                 tabPanel("Confirmed cases",
                          icon = icon('plus-circle'),
                          div(
                            style = "position: absolute; left:5em; bottom: 2em; width:90%",
                            class = "settings_row",
                            downloadButton("download_benefits2_case_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                          ),
                          plotOutput('benefits2_case_plot', height = 300)
                 ),
                 tabPanel("Hospitalizations",
                          icon = icon('hospital'),
                          div(
                            style = "position: absolute; left:5em; bottom: 2em; width:90%",
                            class = "settings_row",
                            downloadButton("download_benefits2_hospital_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                          ),
                          plotOutput('benefits2_hospital_plot', height = 300)
                 ),
                 tabPanel("ICU",
                          icon = icon('procedures'),
                          div(
                            style = "position: absolute; left:5em; bottom: 2em; width:90%",
                            class = "settings_row",
                            downloadButton("download_benefits2_icu_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                          ),
                          plotOutput('benefits2_icu_plot', height = 300)
                 ),
                 tabPanel("Mortality",
                          icon = icon('cross'),
                          div(
                            style = "position: absolute; left:5em; bottom: 2em; width:90%",
                            class = "settings_row",
                            downloadButton("download_benefits2_mortality_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                          ),
                          plotOutput('benefits2_mortality_plot', height = 300)
                 ),
                 tabPanel("Data",
                          icon = icon('table'),
                          div(
                            style = "position: absolute; left:5em; bottom: 2em; width:90%",
                            class = "settings_row",
                            downloadButton("download_benefits2_data", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                          ),
                          DT::dataTableOutput('benefits2_data'),
                          style = "height:300px; overflow-y: auto;overflow-x: auto;",
                          width = '100%')
               )
        )
        )
    ),
    
    ### Risk benefits
    div(id = 'tab_benefit_risk',
        fluidRow(column(width = 6, 
                        tabBox(
                          id = "benefit_risk1",
                          title = "Total benefit/risk",
                          height = "490px",
                          width = '100%',
                          tabPanel("Figure",
                                   icon = icon('chart-bar'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     class = "settings_row",
                                     downloadButton("download_benefit_risk1_plot", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download"),
                                     selectInput('option_age_benefit_risk1', 'Age group:', choices = c(''))
                                   ),
                                   plotOutput('benefit_risk1_plot', height = 400)
                          ),
                          tabPanel("Data",
                                   icon = icon('table'),
                                   div(
                                     style = "position: absolute; left:1.5em; bottom: 2em; width:90%",
                                     class = "settings_row",
                                     downloadButton("download_benefit_risk1_data", icon = icon('download'), label = "", class = "btn-default btn-xs btn-download")
                                   ),
                                   DT::dataTableOutput('benefit_risk1_data'),
                                   style = "height:400px; overflow-y: auto;overflow-x: auto;",
                                   width = '100%')
                        )
        )
        )
    )
  )
}

Menu_result_options <- function() {
  tagList(
    selectInput(
      inputId="option_vaccine", 
      label='Vaccine:', 
      choices=c('')),
    selectInput(
      inputId="option_condition", 
      label="Condition:",
      c('')),
    selectInput(
      inputId = "option_bgr",
      label = div("Background rate:", div(id='info_background_rate', class="tooltips",
                                          icon("question-circle", class = 'fa-solid'),
                                          span(class="tooltiptext", style='width:500px;', "The background rate defines the comparison period used to compute the expected number of risk conditions in the absence of COVID19 vaccination."))),
      choices = c('')
    ),
    selectInput(
      inputId = "option_imputation",
      label = div("Imputation method:", div(id='info_imputation_rate', class="tooltips",
                                       icon("question-circle", class = 'fa-solid'),
                                       div(class="tooltiptext", style='width:500px;', 
                                            "Two imputation methods are supported.",
                                            br(),
                                            "The ", tags$strong('fixed proportion'), " imputation method assumes that the age/gender proportions of the vaccine coverage is exactly the same between countries with missing and without missing age/gender coverage information.",
                                            br(),
                                            "The ", tags$strong('multiple imputation'), " method assumes that the age/gender proportion of the vaccine coverage varies between countries with missing and without missing age/gender coverage information."))),
      choices = c('multiple imputation', 'fixed proportion'),
      selected = c('multiple imputation')
    ),
    selectInput(
      inputId = "option_scenario_risks",
      label = "Scenario risks:",
      choices = c('')
    ),
    selectInput(
      inputId = "option_scenario_benefits",
      label = "Scenario benefits:",
      choices = c('')
    )
  )
}