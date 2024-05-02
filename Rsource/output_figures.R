#### plot style ####

wrap_sentence <- function(string, width) {
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

ggblue <- "#003399";#gg_co
ggorange <- "#E98300"
ggred <- scales::hue_pal()(1)
ggpurple <- "#796BD6"
gggreen <- "#175E54"

colors <- c(ggblue, ggorange, gggreen, ggpurple, '#f5ce42', '#34ebbd', '#a19d9a', '#cf15e8', '#b4eb34', '#e89415', '#1f2121', '#0033ff', '#ff0000', '#4d6339', '#00b6e8', '#00e859', '#e800a2', '#e4e800')

#### outputs ####

subset_risk_data <- reactive({
  
  vaccine <- input$option_vaccine
  condition <- input$option_condition
  period <- input$option_bgr
  imputation <- input$option_imputation
  sex <- input$option_sex_risk1plot
  scenario <- input$option_scenario_risks
  
  group <- input$risk1_plot_grouping
  
  if(sex == 'Male') {
    sex = c('Male', 'M')
  } else if(sex == 'Female') {
    sex = c('Female', 'F')
  }
  
  if(is.null(outputs$results_risk)) {
    return(NULL)
  }

  df <- outputs$results_risk
  
  title <- 'Observed risk'
  
  if(group != 'condition') {
    df <- df %>% filter(condition == !!condition)
    title <- paste0(title, ' of ', condition)
  }
  title <- paste0(title, ' following the vaccination')
  
  if(group != 'sex') {
    df <- df %>% filter(sex %in% !!sex)
    title <- paste0(title, ' of ', ifelse('F' %in% sex, 'females', 'males'))
  }
  
  if(group != 'vaccine') {
    df <- df %>% filter(vaccine == !!vaccine)
    title <- paste0(title, ' with ', vaccine)
  }
  
  if(group != 'period') {
    df <- df %>% filter(period == !!period)
    title <- paste0(title, ' relative to the ', period, ' background rate')
  }
  
  if(group != 'imputation_method') {
    df <- df %>% filter(imputation_method == !!imputation)
  }
  
  if(group != 'scenario') {
    df <- df %>% filter(scenario == !!scenario)
  }
  
  return(list(data = df,
              title = title,
              group = group))
})

subset_risk2_data <- reactive({
  
  vaccine <- input$option_vaccine
  condition <- input$option_condition
  period <- input$option_bgr
  imputation <- input$option_imputation
  sex <- input$option_sex_risk2plot
  scenario <- input$option_scenario_risks
  
  if(sex == 'Male') {
    sex = c('Male', 'M')
  } else if(sex == 'Female') {
    sex = c('Female', 'F')
  }
  
  if(is.null(outputs$results_risk)) {
    return(NULL)
  }
  
  df <- outputs$results_risk
  
  df <- df %>%
    filter(condition == !!condition,
           sex %in% !!sex,
           vaccine == !!vaccine,
           imputation_method == !!imputation,
           period == !!period,
           scenario == !!scenario)
  
  title <- paste0('Observed number of ', condition, ' cases per 100 000 ', vaccine, ' vaccines in ', ifelse('F' %in% sex, 'females', 'males'), ' following vaccination.')
  
  return(list(data = df,
              title = title))
})

risk1_plot_f <- function() {
  obj <- subset_risk_data()
  
  input$option_vaccine
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  group <- obj$group
  title <- obj$title
  df <- obj$data
  
  if(all(df$expected_total == 0)) {
    return(ggplot() +
      theme_void() +
      geom_circle(aes(x0 = 0, y0 = 0, r = 1), size = 2, color = ggorange) + 
      geom_line(aes(x = c(0, 0), y = c(.8, -0.4)), size = 2, color = ggorange) +
      geom_point(aes(x = 0, y = -.6), size = 2, color = ggorange) +
      geom_text(aes(x = 0, y =-1.3, label = paste0('Zero or missing background rate for ', df$condition[1],'\nObserved/expected ratio unavailable') )) +
      coord_fixed(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1)))
  }
  
  
  
  if(group != '') {
    df <- df %>%
      rename(group = !!group)
    
    group_label <- c('Vaccine', 'Condition', 'Sex', 'Background rate', 'Imputation method', 'Scenario')[match(group, c('vaccine', 'condition', 'sex', 'period', 'imputation_method', 'scenario'))]
    
    return(ggplot(data = df, 
                  aes(y = age_group, x = Rmean, xmin = LL, xmax = UL, color = group, group = group)) +
             geom_point(position = position_dodge(width = .2)) + 
             geom_errorbarh(height=.1, 
                            position = position_dodge(width = .2)) +
             geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
             theme_classic() +
             scale_color_manual(group_label, values = colors) +
             ggtitle(wrap_sentence(title, 80)) +
             xlab('O/E ratio') +
             ylab(''))
    
  } else {
    return(ggplot(data = df, 
                  aes(y = age_group, x = Rmean, xmin = LL, xmax = UL)) +
             geom_point(position = position_dodge(width = .2), color = ggblue) + 
             geom_errorbarh(height=.1, 
                            position = position_dodge(width = .2), color = ggblue) +
             geom_vline(xintercept=1, color='black', linetype='dashed', alpha=.5) +
             theme_classic() +
             ggtitle(wrap_sentence(title, 80)) +
             xlab('O/E ratio') +
             ylab(''))
  }
}

output$download_risk1plot <- downloadHandler(
  filename = "risk_ratio_observed_expected.png",
  content = function(file) {
    ggsave(file, plot = risk1_plot_f(), width = 8, height = 4, dpi = 600)
  }
)

output$risks1_plot <- renderPlot({
  
  risk1_plot_f()
  
})

risks1_data_d <- function() {
  obj <- subset_risk_data()
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  obj$data %>%
    rename(ratio = Rmean,
           lower = LL,
           upper = UL)
}

output$risks1_data <- DT::renderDataTable(DT::datatable({
  
  risks1_data_d()
  
  
}, options = list(searching = FALSE,
                  paging = FALSE)) %>%
  DT::formatRound(c('ratio', 'lower', 'upper', 'expected_100k', 'observed_100k', 'expected_total', 'observed_total'), 3))


output$download_risks1_data <- downloadHandler(
  filename = "risk_ratio_observed_expected.csv",
  content = function(file) {
    write.table(risks1_data_d(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

risk2_plot_f <- function() {
  obj <- subset_risk2_data()
  
  if(is.null(obj$data)) {
    return(NULL)
  }
  
  title <- obj$title
  df <- obj$data
  
  ggplot(data = df %>% select(age_group, expected_100k, observed_100k) %>%
           pivot_longer(c(expected_100k, observed_100k))) +
    theme_classic() +
    geom_bar(aes(x = age_group, y = value, fill = name), stat = 'identity', position = position_dodge(width=.8)) +
    ggtitle(wrap_sentence(title, 80)) +
    xlab('') +
    ylab('Number of cases') +
    scale_fill_manual('', 
                      values = c('gray', ggorange), 
                      label = c(paste0('Expected based on the\n', obj$data$period[1], ' background rate'), 'Observed'))
}

output$download_risk2plot <- downloadHandler(
  filename = "risk_comparison_observed_expected.png",
  content = function(file) {
    ggsave(file, plot = risk2_plot_f(), width = 8, height = 4, dpi = 600)
  }
)

output$risks2_plot <- renderPlot({
  
  risk2_plot_f()
  
})

risk2_data_d <- function() {
  obj <- subset_risk2_data()
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  obj$data %>%
    rename(ratio = Rmean,
           lower = LL,
           upper = UL)
}

output$risks2_data <- DT::renderDataTable(DT::datatable({
  
  risk2_data_d()
  
}, options = list(searching = FALSE,
                  paging = FALSE)) %>%
  DT::formatRound(c('ratio', 'lower', 'upper', 'expected_100k', 'observed_100k', 'expected_total', 'observed_total'), 3))

output$download_risks2_data <- downloadHandler(
  filename = "risk_comparison_observed_expected.csv",
  content = function(file) {
    write.table(risk2_data_d(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

#### benefits ####

aggregated_benefits_data <- reactive({
  
  scenario <- input$option_scenario_benefits
  
  if(is.null(outputs$results_benefit)) {
    return(NULL)
  }
  
  df_prevented <- outputs$results_benefit$prevented_benefits_daily
  df_observed <- outputs$results_benefit$observed_burden
  
  prevented <- df_prevented %>%
    filter(scenario == !!scenario) %>%
    pivot_longer(starts_with('prevented_'),
                 names_to = 'burden',
                 names_prefix = 'prevented_',
                 values_to = 'prevented') %>%
    group_by(date, burden) %>%
    summarise(prevented = sum(prevented)) %>%
    ungroup()
  
  observed <- df_observed %>%
    pivot_longer(c(cases, mortality, hospitalization, icu),
                 names_to = 'burden',
                 values_to = 'observed') %>%
    group_by(date, burden) %>%
    summarise(observed = sum(observed)) %>%
    ungroup()
  
  df <- left_join(prevented,
                  observed,
                  by = c('date', 'burden'))
  
  return(list(data = df))
})

benefits_100k_data <- reactive({
  
  scenario <- input$option_scenario_benefits
  option_group <- input$benefit2_plot_grouping
  vaccine <- input$option_vaccine
  
  if(is.null(outputs$results_benefit) | vaccine == '') {
    return(NULL)
  }
  
  df <- outputs$results_benefit$prevented_benefits_total
  
  if(option_group != 'vaccine') {
    df <- df %>% filter(vaccine == !!vaccine)
  }
  
  if(option_group != 'scenario') {
    df <- df %>% filter(scenario == !!scenario)
  }
  
  if(option_group != "") {
    df <- df %>%
      select(age_group, count, !!option_group, starts_with('prevented_')) %>%
      pivot_longer(starts_with('prevented_'),
                   names_to = 'against',
                   values_to = 'prevented',
                   names_prefix = "prevented_") %>%
      mutate(count = replace_na(count, 0),
             prevented = replace_na(prevented, 0)) %>%
      group_by(age_group, against, .dots = option_group) %>%
      summarise(prevented_100k = sum(prevented) / sum(count) * 100000) %>%
      ungroup()
  } else {
    df <- df %>%
      select(age_group, count, starts_with('prevented_')) %>%
      pivot_longer(starts_with('prevented_'),
                   names_to = 'against',
                   values_to = 'prevented',
                   names_prefix = "prevented_") %>%
      mutate(count = replace_na(count, 0),
             prevented = replace_na(prevented, 0)) %>%
      group_by(age_group, against) %>%
      summarise(prevented_100k = sum(prevented) / sum(count) * 100000) %>%
      ungroup()
  }
  
  return(list(data = df,
              group = option_group))
})

plot_prevented_100k <- function(burden, burden_name) {

  obj <- benefits_100k_data()
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  if(obj$group == "") {
    return(ggplot(obj$data %>%
                    filter(against == !!burden)) +
             theme_classic() +
             geom_bar(aes(x = age_group, y = prevented_100k), fill = ggblue, stat = 'identity') +
             ggtitle(wrap_sentence(paste0('Prevented number of ', burden_name, ' per 100 000 ', input$option_vaccine, ' vaccines'), 80)) +
             xlab('') +
             scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
             scale_alpha_continuous(guide = 'none') +
             ylab(paste0('Prevented ', burden_name)) )
  } else {
    ggplot(obj$data %>%
             filter(against == !!burden)) +
      theme_classic() +
      geom_bar(aes(x = age_group, y = prevented_100k, fill = get(obj$group)), 
               stat = 'identity', position = position_dodge(width = .8)) +
      ggtitle(wrap_sentence(paste0('Prevented number of ', burden_name, ' per 100 000 vaccines'), 80)) +
      xlab('') +
      scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
      scale_fill_manual(obj$group, values = colors) +
      ylab(paste0('Prevented ', burden_name))
  }
}

plot_prevented <- function(burden, burden_name) {
  
  obj <- aggregated_benefits_data()
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  df <- obj$data
  
  res <- df %>%
    filter(burden == !!burden) %>%
    mutate(observed = zoo::rollmean(observed, 7, na.pad = TRUE),
           prevented = zoo::rollmean(prevented, 7, na.pad = TRUE)) %>%
    pivot_longer(cols = c(observed, prevented)) 
  
  return(ggplot(res) +
           theme_classic() +
           geom_bar(aes(x = date, y = value, fill = factor(name, c('prevented', 'observed'))), stat = 'identity', position = position_stack(reverse = FALSE), width = 1) +
           ggtitle(wrap_sentence(paste0('Expected total number of observed COVID-19 ', burden_name, ' aggregated over all age groups in the absense of COVID-19 vaccination. Daily counts are split into observed and prevented ', burden_name), 80)) +
           xlab('') +
           scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
           scale_alpha_continuous(guide = 'none') +
           scale_x_date(expand = c(0, 0)) +
           ylab(paste0('Daily number of ', burden_name)) +
           scale_fill_manual('', values = c('gray', ggblue), label = c('Prevented', 'Observed')))
}

output$benefits1_case_plot <- renderPlot({
  plot_prevented('cases', 'cases')
})

output$benefits1_hospital_plot <- renderPlot({
  plot_prevented('hospitalization', 'hospital admissions')
})

output$benefits1_mortality_plot <- renderPlot({
  plot_prevented('mortality', 'deaths')
})

output$benefits1_icu_plot <- renderPlot({
  plot_prevented('icu', 'icu admissions')
})

output$download_benefits1_case_plot <- downloadHandler(
  filename = "benefits_cases_observed_prevented.png",
  content = function(file) {
    ggsave(file, plot = plot_prevented('cases', 'cases'), width = 8, height = 4, dpi = 600)
  }
)

output$download_benefits1_hospital_plot <- downloadHandler(
  filename = "benefits_hospitalization_observed_prevented.png",
  content = function(file) {
    ggsave(file, plot = plot_prevented('hospitalization', 'hospital admissions'), width = 8, height = 4, dpi = 600)
  }
)

output$download_benefits1_icu_plot <- downloadHandler(
  filename = "benefits_icu_observed_prevented.png",
  content = function(file) {
    ggsave(file, plot = plot_prevented('icu', 'icu admissions'), width = 8, height = 4, dpi = 600)
  }
)

output$download_benefits1_mortality_plot <- downloadHandler(
  filename = "benefits_deaths_observed_prevented.png",
  content = function(file) {
    ggsave(file, plot = plot_prevented('mortality', 'deaths'), width = 8, height = 4, dpi = 600)
  }
)

output$benefits2_case_plot <- renderPlot({
  plot_prevented_100k('cases', 'cases')
})

output$benefits2_hospital_plot <- renderPlot({
  plot_prevented_100k('hospitalization', 'hospital admissions')
})

output$benefits2_mortality_plot <- renderPlot({
  plot_prevented_100k('mortality', 'deaths')
})

output$benefits2_icu_plot <- renderPlot({
  plot_prevented_100k('icu', 'icu admissions')
})

output$download_benefits2_case_plot <- downloadHandler(
  filename = "benefits_cases_prevented_100k.png",
  content = function(file) {
    ggsave(file, plot = plot_prevented_100k('cases', 'cases'), width = 8, height = 4, dpi = 600)
  }
)

output$download_benefits2_hospital_plot <- downloadHandler(
  filename = "benefits_hospitalization_prevented_100k.png",
  content = function(file) {
    ggsave(file, plot = plot_prevented_100k('hospitalization', 'hospital admissions'), width = 8, height = 4, dpi = 600)
  }
)

output$download_benefits2_icu_plot <- downloadHandler(
  filename = "benefits_icu_prevented_100k.png",
  content = function(file) {
    ggsave(file, plot = plot_prevented_100k('icu', 'icu admissions'), width = 8, height = 4, dpi = 600)
  }
)

output$download_benefits2_mortality_plot <- downloadHandler(
  filename = "benefits_deaths_prevented_100k.png",
  content = function(file) {
    ggsave(file, plot = plot_prevented_100k('mortality', 'deaths'), width = 8, height = 4, dpi = 600)
  }
)

benefits1_data_d <- function() {
  obj <- aggregated_benefits_data()
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  df <- obj$data
}

output$benefits1_data <- DT::renderDataTable(DT::datatable({

  benefits1_data_d()

}, options = list(searching = FALSE,
                  paging = FALSE)) %>%
  DT::formatRound(c('prevented', 'observed'), 1))


output$download_benefits1_data <- downloadHandler(
  filename = "benefits_observed_prevented.csv",
  content = function(file) {
    write.table(benefits1_data_d(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

benefits2_data_d <- function() {
  obj <- benefits_100k_data()
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  df <- obj$data
}

output$benefits2_data <- DT::renderDataTable(DT::datatable({
  
  benefits2_data_d()
  
}, options = list(searching = FALSE,
                  paging = FALSE)) %>%
  DT::formatRound(c('prevented_100k'), 1))

output$download_benefits2_data <- downloadHandler(
  filename = "benefits_prevented_100k.csv",
  content = function(file) {
    write.table(benefits2_data_d(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)

#### benefit-risk comparison ####

Age_group_to_vector <- function(group) {
  ages <- 0:100
  age_group <- rep('', 100)
  
  for(i in 1:length(group)) {
    start <- str_extract(group[i], '^(\\d+)') %>% as.numeric
    end <- str_extract(group[i], '(\\d+)$') %>% as.numeric
    
    if(is.na(start) | is.na(end)) {
      num <- ifelse(is.na(start), end, start) %>% as.numeric
      if(any(str_detect(group[i], fixed(c('>', '+'))))){
        age_group[ages >= num] <- group[i]
      } else{
        age_group[ages <= num] <- group[i]
      }
    } else {
      age_group[ages >= start & ages <= end] <- group[i]
    }
    
  }
  
  return(age_group)
}

risk_benefit_data <- reactive({
  
  vaccine <- input$option_vaccine
  condition <- input$option_condition
  period <- input$option_bgr
  imputation <- input$option_imputation
  scenario_risk <- input$option_scenario_risks
  scenario_benefit <- input$option_scenario_benefits
  age_group <- input$option_age_benefit_risk1
  
  if(is.null(outputs$results_risk)) {
    return(NULL)
  }
  
  risk <- outputs$results_risk
  benefit <- outputs$results_benefit$prevented_benefits_daily
  
  additional_risk <- risk %>%
    filter(condition == !!condition,
           vaccine == !!vaccine,
           imputation_method == !!imputation,
           period == !!period,
           scenario == !!scenario_risk,
           age_group == !!age_group) %>%
    summarise(additional = sum(observed_total) - sum(expected_total))
  
  if(nrow(additional_risk) == 0) {
    return(NULL)
  }
  
  out <- data.frame(outcome =  paste0(condition, '*'),
             additional = additional_risk$additional) 
  
  convert_age <- data.frame(age_risk = Age_group_to_vector(risk %>%
                                                             filter(condition == !!condition,
                                                                    vaccine == !!vaccine) %>%
                                                             pull(age_group) %>% as.character()),
                            age_benefit = Age_group_to_vector(unique(benefit$age_group) %>% as.character())) %>%
    group_by(age_benefit) %>%
    mutate(proportion = 1 / n()) %>%
    ungroup() %>%
    filter(age_risk == !!age_group) %>%
    group_by(age_benefit) %>%
    summarise(proportion = sum(proportion)) %>%
    ungroup() %>%
    rename(age_group = age_benefit)
  
  prevented_benefit <- benefit %>%
    filter(vaccine == !!vaccine,
           scenario == !!scenario_benefit) %>%
    pivot_longer(starts_with('prevented_'),
                 names_prefix = 'prevented_',
                 values_to = 'prevented',
                 names_to = 'outcome') %>%
    group_by(age_group, outcome) %>%
    summarise(prevented = sum(prevented)) %>%
    inner_join(convert_age) %>%
    mutate(prevented = prevented * proportion) %>%
    group_by(outcome) %>%
    summarise(prevented = sum(prevented)) %>%
    filter(outcome != 'cases') %>%
    arrange(outcome)
  
  out <- rbind(out,
               data.frame(outcome = c('hospital admission', 'ICU admission', 'mortality'),
                          additional = -prevented_benefit$prevented))
  
  
  title <- paste0('Expected additional and prevented burden following ', vaccine, ' vaccination in age group ', age_group)
  caption <- paste0('*Expected additional cases compared to the ', period, ' background rate')
  
  return(list(data = out,
              title = title,
              caption = caption))
  
})

benefit_risk1_f <- function() {
  obj <- risk_benefit_data()
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  df <- obj$data
  
  col <- c(ggorange, gggreen)
  if(all(obj$data$additional < 0)){
    col <- gggreen
  }
  
  lim <- max(abs(obj$data$additional)) *1.1
  tick <- ceiling(lim/100)*c(-100, -50, 0, 50, 100)
  label <- paste0(paste0(tick, '\n'),c('', 'prevented burden', '', 'additional burden', ''))
  
  ggplot(df) +
    theme_classic() +
    geom_bar(aes(x = fct_rev(outcome), y = additional, fill = factor(additional > 0, c(TRUE, FALSE), c('red', 'green'))), stat = 'identity') +
    coord_flip() +
    labs(title = wrap_sentence(obj$title, 80),
         caption = obj$caption) +
    scale_y_continuous(limits = c(min(tick), max(tick))*1.1,
                       breaks = tick,
                       labels = label) +
    xlab('') +
    ylab('') +
    geom_hline(yintercept = 0) +
    scale_fill_manual('', values = col, guide = 'none')
}

output$benefit_risk1_plot <- renderPlot({
  benefit_risk1_f()
})

output$download_benefit_risk1_plot <- downloadHandler(
  filename = "benefits_risk_comparison.png",
  content = function(file) {
    ggsave(file, plot = benefit_risk1_f(), width = 8, height = 4, dpi = 600)
  }
)

benefit_risk1_data_d <- function() {
  obj <- risk_benefit_data()
  
  if(is.null(obj)) {
    return(NULL)
  }
  
  obj$data %>% rename(additional_burden = additional)
}

output$benefit_risk1_data <- DT::renderDataTable(DT::datatable({
  
  benefit_risk1_data_d()
  
}, options = list(searching = FALSE,
                  paging = FALSE)) %>%
  DT::formatRound(c('additional_burden'), 1))

output$download_benefit_risk1_data <- downloadHandler(
  filename = "benefits_risk_comparison.csv",
  content = function(file) {
    write.table(benefit_risk1_data_d(), file = file, row.names = FALSE, col.names = TRUE, sep = ';')
  }
)