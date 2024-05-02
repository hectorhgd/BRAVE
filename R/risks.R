Compute_risks <- function(observed_risks,
                          bgr,
                          aggregated_vaccine,
                          param_risk_weight,
                          param_risk_notification_period) {
  
  bgr_pool <- bgr %>%
    left_join(param_risk_weight, by = 'source') %>%
    mutate(sd = sqrt(cases) / person_years * 100000,
           incidence = cases / person_years * 100000) %>%
    mutate(sd = ifelse(sd == 0, NA, sd^2)) %>%
    group_by(sex, age_group, condition, period, scenario) %>% 
    summarise(PoolMean = (sum(weight * incidence / sd, na.rm = TRUE) / sum(weight / sd, na.rm = TRUE)),
              PoolSd = (sqrt(sum(weight^2)) / sum(weight / sd, na.rm = TRUE)),
              .groups = 'drop') 
  
  OE_before_MP <- inner_join(bgr_pool,
                             observed_risks, 
                             by=c("sex", "age_group", "condition")) 
  
  df2 <- aggregated_vaccine %>% 
    full_join(OE_before_MP, by = c('vaccine', 'sex', 'age_group')) %>%
    left_join(param_risk_notification_period) %>%
    mutate(delay = delay / 365) %>%
    mutate(observed = as.numeric(cases) * 100000 / cov,
           expected = PoolMean * delay) %>%
    group_by(vaccine, sex, age_group, condition, imputation_method, period, scenario) %>%
    summarise(observed = mean(observed),
              expected = mean(expected)) %>%
    ungroup() %>%
    mutate(expected = replace_na(expected, 0))
  
  df <- aggregated_vaccine %>%
    inner_join(OE_before_MP, by = c('vaccine', 'sex', 'age_group')) %>%
    left_join(param_risk_notification_period) %>%
    mutate(delay = delay / 365) %>%
    mutate(R = as.numeric(cases) * 100000 / cov / PoolMean / delay,
           VR = (((as.numeric(cases) * 100000 / delay)^2) * ((var*PoolSd^2)+(var*PoolMean^2)+(PoolSd^2*cov^2))/(cov*PoolMean)^4 )) %>%
    group_by(vaccine, sex, age_group, condition, period, imputation_method, scenario) %>%
    summarise(Rmean = mean(R),
              VarW = mean(VR),
              VarB = var(R),
              mi = max(imputation_id),
              cases = mean(as.numeric(cases)),
              cov = cov[1],
              PoolMean = PoolMean[1],
              delay = delay[1]) %>%
    ungroup() %>%
    mutate(expected = cov * PoolMean * delay / 100000,
           Var = VarW + (mi+1)/mi * VarB,
           DF = (mi-1)*(1+(VarW/((1+1/mi)*VarB)))^2,
           LL = ifelse(imputation_method == 'multiple imputation',
                       Rmean - qt(0.975, df=DF)*sqrt(Var),
                       ((1-(1/(9*cases)) - (1.96/(3*(sqrt(cases)))))^3)*cases/expected),
           UL = ifelse(imputation_method == 'multiple imputation',
                       Rmean + qt(0.975, df=DF)*sqrt(Var),
                       ((1-(1/(9*(cases+1))) + (1.96/(3*sqrt(cases+1))))^3)*(cases+1)/expected),
           expected_100k = expected / cov * 100000,
           observed_100k = cases / cov * 100000) %>%
    select(vaccine, sex, age_group, condition, period, imputation_method, scenario,
           Rmean, LL, UL, expected_100k, observed_100k, expected_total = expected, observed_total = cases) %>%
    mutate(expected_100k = replace_na(expected_100k, 0),
           expected_total = replace_na(expected_total, 0))
  
  return(df)
}
