population <- readRDS('data/population_data_default.rds')
age_specific_vaccine <- readRDS('data/age_specific_vaccine_data_default.rds')

correction_factor <- population %>% 
  left_join(age_specific_vaccine,
            by = c('country', 'age_group')) %>%
  group_by(country, age_group, dose) %>%
  summarise(count = sum(count),
            pop = population[1]) %>%
  ungroup() %>%
  mutate(correction_factor = ifelse(count > pop, pop/count * 0.999, 1)) %>%
  select(country, age_group, dose, correction_factor)

age_specific_vaccine_corrected <- age_specific_vaccine %>%
  left_join(correction_factor) %>%
  mutate(count = count * correction_factor) %>%
  select(-correction_factor)

saveRDS(age_specific_vaccine_corrected, 'data/age_specific_vaccine_data_default.rds')
