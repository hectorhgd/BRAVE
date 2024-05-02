Custom_file_input <- function(...) {
  
    temp <- fileInput(...)

  # Cut away the label
  temp$children[[1]] <- NULL
  # Cut away the input field (after label is cut, this is position 1 now)
  temp$children[[1]]$children[[2]] <- NULL
  # Remove input group classes (makes button flat on one side)
  temp$children[[1]]$attribs$class <- NULL
  temp$children[[1]]$children[[1]]$attribs$class <- NULL

  temp$children[[1]]$children[[1]]$children[[1]]$name <- 'a'
  temp$children[[1]]$children[[1]]$children[[1]]$attribs$class <- NULL
  temp$children[[1]]$children[[1]]$children[[1]]$attribs$style <- "cursor:pointer; font-weight:400;"

  temp$children[[1]]$children[[1]]$children[[1]]$children[[1]] <- 'restore session'
  
  # return(HTML('<label>
  #     <a style="cursor:pointer; font-weight:400;">
  #       restore session
  #       <input id="restore_session" name="restore_session" type="file" style="display: none;"/>
  #     </a>
  #   </label>
  # </div>'))
  
  return(HTML('<label><a style="cursor:pointer; font-weight:400;line-height:50px; vertical-align:middle; color: white; font-size: 14px; margin:10px;">
        <i class="fas fa-redo"></i> Restore session 
        <input id="restore_session" name="restore_session" type="file" style="display: none;"/>
      </a></label>'))
}
