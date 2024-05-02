observeEvent(input$save_session_unavailable, {

  shinyalert("session not saved", "saving a session is only possible after generating results", type = "warning")

})

output$save_session <- downloadHandler(
  filename = function() {
    paste0("vaccine_benefit_risk_", Sys.Date(), ".session")
  },
  content = function(file) {
    showNotification("Preparing data...", duration = 5)

    out <- list(
      params = outputs$params,
      results_benefit = outputs$results_benefit,
      results_risk = outputs$results_risk
    )

    saveRDS(out, file = file)
  }
)

observeEvent(input$restore_session, {

  loaded <- readRDS(input$restore_session$datapath)

  outputs$results_risk <- loaded$results_risk
  outputs$results_benefit <- loaded$results_benefit

  Show_results()

  addCssClass(selector = "a[href$='shiny-tab-data_upload']", class = "inactiveLink")

})


observeEvent(input$save_data_description_unavailable, {

  shinyalert("report not generated", "saving an overview of the data becomes available after generating the results", type = "warning")

})

output$save_data_description <- downloadHandler(
  filename = function() {
    paste0("vaccine_benefit_risk_data_descritpion", Sys.Date(), ".html")
  },
  content = function(file) {
    showNotification("Preparing report", duration = 5)

    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("www/template.Rmd", tempReport, overwrite = TRUE)

    params <- list(params = outputs$params,
                   countries = unique(outputs$results_benefit$prevented_benefits_total$country),
                   age_groups_benefits = unique(outputs$results_benefit$prevented_benefits_total$age_group),
                   vaccines = unique(outputs$results_benefit$prevented_benefits_total$vaccine),
                   variants = unique(outputs$params$param_ve$variant),
                   conditions_per_vaccine = outputs$results_risk %>% select(vaccine, condition) %>% distinct,
                   age_groups_per_condition = outputs$results_risk %>% select(condition, age_group) %>% distinct)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }

)



