# TODO: Add try catch for loading files

Load_data <- function(path, uploaded) {
  if(is.null(path)) {
    return(NULL) # not yet initialized or no dataset uploaded
  } else if(path != '' & path != 'Use uploaded dataset') {
    path <- paste0('data/', path)
  } else if(!is.null(uploaded)) {
    path <- uploaded$datapath
  } else {
    return(NULL)
  }
  
  type <- tolower(Get_extension(path))
  
  # only rds and csv files allowed
  # csv files use ; as delimiter
  if(type == 'rds') {
    return(readRDS(path))
  } else {
    return(read.table(path, sep = ';', fileEncoding = 'UTF-8-BOM', header = TRUE))
  }
}

Get_extension <- function(file){ 
  ex <- strsplit(basename(file), split="\\.")[[1]]
  return(ex[-1])
} 

STATUS_ERROR <- 2
STATUS_WARNING <- 1
STATUS_VALID <- 0

Set_valid_status <- function(id, id_error, validation_status) {
  error_txt <- ''
  if(length(validation_status$error) > 0) {
    error_txt <- paste0('Errors:<br>', 
                        paste0('-', validation_status$error, '<br>', collapse = ''))
  }
  if(length(validation_status$warning) > 0) {
    if(error_txt != '') {
      error_txt <- paste0(error_txt, '<br>')
    } 
    
    error_txt <- paste0(error_txt,
                        'Warnings:<br>', 
                        paste0('-', validation_status$warning, '<br>', collapse = ''))
  }
  
  if(validation_status$status != STATUS_ERROR) {
    html(id, "Ready")
    removeClass(id, "msginvalid")
    addClass(id, "msgvalid")
    html(id_error, error_txt)
  } else {
    html(id, "Fix error to proceed")
    removeClass(id, "msgvalid")
    addClass(id, "msginvalid")
    html(id_error, error_txt)
  }
}

Init_validation_status <- function() {
  return(list(
    status = STATUS_VALID,
    error = c(),
    warning = c()
  ))
}

Validate_data_loaded <- function(data, validation_status, required = TRUE) {
  if(is.null(data)) {
    if(required) {
      validation_status$error <- c(validation_status$error, 'Dataset missing')
      validation_status$status <- STATUS_ERROR
    } else {
      validation_status$warning <- c(validation_status$warning, 'Dataset missing')
      validation_status$status <- max(validation_status$status, STATUS_WARNING)
    }
  }
  
  return(validation_status)
}

Validate_column <- function(data, validation_status, required_columns = c(), optional_columns = c()) {
  missing_required_columns <- required_columns[!(required_columns %in% colnames(data))]
  missing_optional_columns <- optional_columns[!(optional_columns %in% colnames(data))]
  
  if(length(missing_required_columns) > 0) {
    validation_status$error <- c(validation_status$error, 
                                 paste0('Missing required column ', missing_required_columns))
    
    validation_status$status <- STATUS_ERROR
  }
  
  if(length(missing_optional_columns) > 0) {
    validation_status$warning <- c(validation_status$warning, 
                                 paste0('Missing optional column ', missing_optional_columns))
    
    validation_status$status <- max(validation_status$status, STATUS_WARNING)
  }
  
  return(validation_status)
}

Validate_levels <- function(var, validation_status, required_levels, varname) {
  levels <- unique(var)
  
  missing_required_levels <- required_levels[!(required_levels %in% levels)]
  additional_levels <- levels[!(levels %in% required_levels)]
  
  if(length(missing_required_levels) > 0) {
    validation_status$error <- c(validation_status$error, 
                                 paste0('Missing ', varname, ' ', missing_required_levels))
    
    validation_status$status <- STATUS_ERROR
  }
  
  if(length(additional_levels) > 0) {
    validation_status$warning <- c(validation_status$warning, 
                                 paste0('Additional ', varname, ' ', additional_levels))
    
    validation_status$status <- max(validation_status$status, STATUS_WARNING)
  }
  
  return(validation_status)
}

Validate_age_condition <- function(validation_status, observed_levels, required_levels) {
  required_levels <- required_levels %>% select(age_group, condition) %>% distinct
  
  missing <- required_levels %>%
    anti_join(observed_levels)
  additional <- observed_levels %>%
    anti_join(required_levels)
  
  if(nrow(missing) > 0) {
    validation_status$error <- c(validation_status$error, 
                                   paste0('Missing level: ', missing$condition, ' ', missing$age_group))
    
    validation_status$status <- max(validation_status$status, STATUS_ERROR)
  } 
  
  if(nrow(additional) > 0) {
    validation_status$warning <- c(validation_status$warning, 
                                 paste0('Additional level: ', additional$condition, ' ', additional$age_group))
    
    validation_status$status <- max(validation_status$status, STATUS_WARNING)
  } 
  
  return(validation_status)
}

Validate_age_vaccine <- function(validation_status, observed_levels, required_levels) {
  required_levels <- required_levels %>% select(age_group, vaccine) %>% distinct
  
  missing <- required_levels %>%
    anti_join(observed_levels)
  additional <- observed_levels %>%
    anti_join(required_levels)
  
  if(nrow(missing) > 0) {
    validation_status$error <- c(validation_status$error, 
                                 paste0('Missing level: ', missing$vaccine, ' ', missing$age_group))
    
    validation_status$status <- max(validation_status$status, STATUS_ERROR)
  } 
  
  if(nrow(additional) > 0) {
    validation_status$warning <- c(validation_status$warning, 
                                   paste0('Additional level: ', additional$vaccine, ' ', additional$age_group))
    
    validation_status$status <- max(validation_status$status, STATUS_WARNING)
  } 
  
  return(validation_status)
}

Validate_age_group <- function(validation_status, group) {
  group <- as.character(group)
  for(i in 1:length(group)) {
    start <- str_extract(group[i], '^(\\d+)') %>% as.numeric
    end <- str_extract(group[i], '(\\d+)$') %>% as.numeric
    
    if(is.na(start) & is.na(end)) {
      validation_status$error <- c(validation_status$error, 
                                   paste0('Invalid specification of age_group: ', group[i]))
      
      validation_status$status <- max(validation_status$status, STATUS_ERROR)    
    }
    
    if(!is.na(start) & !is.na(end)) {
      if(start >= end) {
        validation_status$error <- c(validation_status$error, 
                                     paste0('Invalid specification of age_group: ', group[i]))
        
        validation_status$status <- max(validation_status$status, STATUS_ERROR)    
      }
    }
  }
  
  return(validation_status)
}

