Render_parameter_input <- function(session,
                                   parameters, 
                                   fixed,
                                   id,
                                   container,
                                   target) {

  shinyjs::disable("btn_finish_parameter_input")
  
  output_html <- c('<table id="',id,'">')
  
  header_html <- '<tr class="parameter_table_head_tr">'
  for(j in 1:ncol(parameters)) {
    idd <- paste0(id, '_', j)
    if(j <= fixed) {
      header_html <- c(header_html, '<td id="',idd,'" class="parameter_table_head_td">', colnames(parameters)[j],'</td>')
    } else {
      header_html <- c(header_html, '<td class="parameter_table_head_td"><input id="', idd, '" type="text" value="', colnames(parameters)[j],'" onblur=\'required_field("',idd,'"); get_parameters("',id,'", "',target,'")\'/></td>')
    }
  }
  
  output_html <- c(output_html, header_html, '</tr>')
  
  for(i in 1:nrow(parameters)) {
    row_html <- '<tr class="parameter_table_tr">'
    
    for(j in 1:ncol(parameters)) {
      idd <- c(id, '_', i, '_', j)
      
      if(j <= fixed) {
        row_html <- c(row_html, '<td id="',idd,'" class="parameter_table_td">', parameters[i, j],'</td>')
      } else {
        if(j == fixed+1) { # required
          row_html <- c(row_html, '<td class="parameter_table_td"><input id="', idd, '" value="', parameters[i, j],'" type="number" onblur=\'required_field("',idd,'"); get_parameters("',id,'", "',target,'")\'/></td>')
        } else { # optional
          row_html <- c(row_html, '<td class="parameter_table_td"><input id="', idd, '" value="', parameters[i, j],'" type="number" onblur=\'get_parameters("',id,'", "',target,'")\' /></td>')
        }
        
      }
      
    }
    
    row_html <- c(row_html, '</tr>')
    
    output_html <- c(output_html, row_html)
    
  }
  
  output_html <- c(output_html, '</table>')
  
  
  html(container, paste0(output_html, collapse = ''))
  
  session$sendCustomMessage("get_parameter_input", c(id, target))
}

Text_to_parameter <- function(msg, chr = c()) {
  msg_data <- msg %>% 
    str_split(fixed(';+;')) %>% unlist %>%
    str_split(fixed(';=;'))
  
  ret <- data.frame(colid = 1:(length(msg_data)-1))
  ncol <- length(msg_data[[1]])

  for(i in 1:ncol) {
    colname <- msg_data[[1]][i]
    value <- map_chr(msg_data[2:length(msg_data)], function(x){x[i]})
    
    if(!(i %in% chr)) {
      value <- as.numeric(value)
    }
    
    ret[[colname]] <- value
    
  }  
  
  ret <- ret %>% select(-colid)
  
  return(ret)
}

Download_parameter_input <- function(session, id, target) {
  session$sendCustomMessage('get_parameter_input', c(id, target))
  #document.getElementById('table1').rows[0].cells.length
}


