require(shiny)
require(shinydashboard)
require(shinydashboardPlus)
require(shinyWidgets)
require(htmlwidgets)
require(shinycssloaders)
require(shinyjs)
require(ggplot2)
require(tidyverse)
require(RColorBrewer)
require(grDevices)
require(shinyalert)
require(ggforce)

async <- FALSE

if(async) {
  library(promises)
  library(future)
  library(ipc)
  
  plan(multisession)
}

options(dplyr.summarise.inform = FALSE)
options(shiny.maxRequestSize = 100 * 1024^2)
       
source('R/load_data.R')
source('R/render_parameter_input.R')
source('R/view_data_upload.R')
source('R/view_parameters.R')
source('R/view_results.R')
source('R/risks.R')
source('R/benefits.R')
source('R/custom_file_input.R')

icon <- function (name, class = NULL, lib = "font-awesome"){
  if(lib=="local"){
    if(is.null(name$src))
      stop("If lib='local', 'name' must be a named list with a 'src' element
           and optionally 'width' (defaults to 100%).")
    if(is.null(name$width)) name$width <- "100%"
    return(tags$i(tags$img(class="img img-local", src=name$src, width=name$width),
                  style = 'margin-right:15px;'))
  }
  
  prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon")
  prefix <- prefixes[[lib]]
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ", 
         paste0("\"", names(prefixes), "\"", collapse = ", "))
  }
  iconClass <- ""
  if (!is.null(name)) 
    iconClass <- paste0(prefix, " ", prefix, "-", name)
  if (!is.null(class)) 
    iconClass <- paste(iconClass, class)
  iconTag <- tags$i(class = iconClass)
  if (lib == "font-awesome") {
    htmltools::htmlDependencies(iconTag) <- htmltools::htmlDependency("font-awesome", 
                                                                      "4.6.3", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css")
  }
  iconTag
}


ui_sidebar <- dashboardSidebar(  
  tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
  tags$style(".fa-question-circle {color:#003399}"),
  tags$style(HTML("
      .main-sidebar{
        width: 200px;
      }
    ")), 
  
  sidebarMenu(id = 'menu', style = 'margin-top:15px',
    menuItem("Home", icon = icon(list(src = "Assets/Icon_home.png", width="12px"), lib = 'local'), tabName = 'landing_page', selected = TRUE),
    menuItem("Input data", tabName = 'data_upload', icon = icon(list(src = "Assets/Icon_Input data.png", width="12px"), lib = 'local'), startExpanded = TRUE,
             menuItem("Risk data", tabName = "data_upload_risks"),
             menuItem("Benefit data", tabName = "data_upload_benefits"),
             menuItem("Finish data upload", tabName = "data_upload_finish")),
    menuItem("Input parameters", tabName = "parameter_input", icon = icon(list(src = "Assets/Icon_input parameteres.png", width="12px"), lib = 'local'),
             menuItem("Risk parameters", tabName = "parameters_risks"),
             menuItem("Benefit parameters", tabName = "parameters_benefits"),
             menuItem("Finish parameter input", tabName = "parameters_finish")),
    menuItem("Results", tabName = 'results', icon = icon(list(src = "Assets/Icon_results.png", width="12px"), lib = 'local'),
             menuItem("Parameters:", tabName = "results"),
             Menu_result_options()),
    hr(style = 'margin-top:4px;margin-bottom:4px'),
    menuItem("Guidance", tabName = 'guidance', icon = icon(list(src = "Assets/Icon_guidance.png", width="12px"), lib = 'local'), startExpanded = TRUE,
             menuItem("User manual", href = "user manual.pdf")),
    menuItem("About", tabName = 'about', icon = icon(list(src = "Assets/Icon_About.png", width="12px"), lib = 'local'), startExpanded = TRUE,
             menuItem("App version 1.1"))
  )
)

ui_controlbar <- dashboardControlbar(
  h4("Save/restore session", style = "margin-left:15px;"),
  tags$ul(
    tags$li(id = 'save_unavailable', actionLink("save_session_unavailable", "save session")),
    tags$li(id = 'save_available', downloadLink("save_session", "save session")),
    tags$li(Custom_file_input("restore_session", "Restore session"))
  ),
  h4("Documentation", style = "margin-left:15px;"),
  tags$ul(
    tags$li(a("user manual", target="_blank", href="user manual.pdf")),
    tags$li(id = "save_desc_unavailable", actionLink("save_data_description_unavailable", "save data description")),
    tags$li(id = "save_desc_available", downloadLink("save_data_description", "save data description"))
  ),
  br(),
  h4("About", style = "margin-left:15px;"),
  tags$ul(
    tags$li("App version 1.1")
  )
)

ui_header_logo <- function() {
  
  div(id='logos',
      style = "position: absolute; left: 0px; top: 0px; z-index:1050; display:inline-block; text-align:center;",
      img(src='Assets/BRAVE Benefit Risk Assessment of VaccinEs.png', height='50px', style='margin:4px; margin-left:25px')
  )
}

ui_header_save_restore <- function() {
  link_style <- "line-height:50px; vertical-align:middle; color: white; font-size: 14px; margin:10px;"
  
  div(style = "position: absolute; height:50px; right: 15px; top: 0px; z-index:1050; display:inline-block;",
    tags$span(id = "save_desc_unavailable", actionLink("save_data_description_unavailable", icon = icon('download'), "Save data description", style = link_style)),
    tags$span(id = "save_desc_available", downloadLink("save_data_description", icon = icon('download'), "Save data description", style = link_style)),
    tags$span(id = 'save_unavailable', actionLink("save_session_unavailable", "Save session", icon = icon('download'), style = link_style)),
    tags$span(id = 'save_available', downloadLink("save_session", "Save session", icon = icon('download'), style = link_style)),
    tags$span(Custom_file_input("restore_session", "Restore session"))
  )
}

ui_body <- dashboardBody(
  tags$head(
    tags$link(
      rel = "stylesheet", 
      type = "text/css", 
      href = "stylesheet.css")
  ),
  tags$script(src = "javascript.js"),
  useShinyjs(),
  ui_header_logo(),
  ui_header_save_restore(),
  tabItems(
    tabItem(tabName = "landing_page", View_landing_page()),
    tabItem(tabName = "data_upload_risks", View_data_upload_risks()),
    tabItem(tabName = "data_upload_benefits", View_data_upload_benefits()),
    tabItem(tabName = "data_upload_finish", View_data_upload_finish()),
    tabItem(tabName = "parameters_risks", View_parameters_risks()),
    tabItem(tabName = "parameters_benefits", View_parameters_benefits()),
    tabItem(tabName = "parameters_finish", View_parameters_finish()),
    tabItem(tabName = "results", View_results())
  )
)

ui <- dashboardPage(header = dashboardHeader(title = "", controlbarIcon = icon('bars')),
                    sidebar = ui_sidebar,
                    body = ui_body)

server <- function(session, input, output) {
  
  auto_proceed_to_results <- FALSE
  
  result_computation <- reactiveVal(NULL)
  result_interrupt <- NULL

  if(async) {
    q <- queue()
    q$consumer$addHandler(function(sig, msg, env){
      return(msg)
    }, 'interrupt')
  }
  
  source("Rsource/load_benefits_data.R", local = TRUE)
  source("Rsource/load_risks_data.R", local = TRUE)
  source("Rsource/finish_data_upload.R", local = TRUE)
  source("Rsource/validate_loaded_data.R", local = TRUE)
  source("Rsource/input_parameters.R", local = TRUE)
  source("Rsource/output_figures.R", local = TRUE)
  source('Rsource/compute_results.R', local = TRUE)
  source('Rsource/save_restore_session.R', local = TRUE)
  
  #addCssClass(selector = "a[href$='#']", class = "inactiveLink")
  addCssClass(selector = "a[href$='shiny-tab-parameter_input']", class = "inactiveLink")
  addCssClass(selector = "a[href$='shiny-tab-results']", class = "inactiveLink")

  param_risks_template <- NULL
  param_ve_template <- NULL
  
  hide("btn_interrupt_computation")
  hide("save_available")
  hide("save_desc_available")
  
  outputs = reactiveValues(results_risk = NULL, results_benefit = NULL, params = NULL)
  
  results_risk <- NULL
  results_benefit <- NULL
  
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  # 
  #   print(query)
  #   if(!is.null(query$tab)) {
  #     if(query$tab == "shiny-tab-data_upload_risks") {
  #       session$sendCustomMessage("set_active_tab", c(-1,0))
  #     }
  #   }
  # })
}

# rsconnect::deployApp(appTitle='COVID vaccine risks and benefits develop', account = 'dsi-uhasselt', server='shinyapps.io', forceUpdate = TRUE)

# rsconnect::deployApp(appTitle='COVID vaccine risks and benefits', account = 'dsi-uhasselt', server='shinyapps.io')
shinyApp(ui = ui, server = server)
