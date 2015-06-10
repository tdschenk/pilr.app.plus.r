## Weekly graph of percent reported, percent taken, and 80% cutoff line
## Weeks are monday to sunday
# REQUIRES XTS
#' @export
reported_taken_bar <- function(data, params, ...) {
  dosage <- data$dosages
  survey <- data$survey
  # Add weekday to dataframe
  survey$local_time <- as.POSIXlt(survey$local_time, format = "%Y-%m-%dT%H:%M:%SZ")
  survey$week <- format(survey$local_time, format = "%W")
  # Unlist columns
  dosage_schedule <- as.data.frame(t(apply(dosage_schedule, 1, unlist)))
  # Remove empty dosage_id rows
  dosage_schedule <- dosage_schedule[dosage_schedule$dosage_id != "", ]
  # Find total expected dosages
  dosage_final <- data.frame(dosage_id = character())
  for (i in 1:length(unique(dosage_schedule$dosage_id))) {
    dosage_subset <- dosage_schedule[dosage_schedule$dosage_id == 
                                       as.character(unique(dosage_schedule$dosage_id))[i], ]
    if (as.character(dosage_subset$action[nrow(dosage_subset)]) == "EDIT" |
        as.character(dosage_subset$action[nrow(dosage_subset)]) == "CREATE")
      dosage_final <- rbind(dosage_final, 
                            data.frame(dosage_id = as.character(unique(dosage_schedule$dosage_id))[i]))
  }
  # Expected doses per week
  expected_doses <- 7*nrow(dosage_final)
  # Calculate percent reported and percent taken
  summary <- data.frame(week_start = character(),
                        percent  = numeric(),
                        category = numeric())
  for (i in 1:length(unique(survey$week))) {
    survey_reported <- survey[survey$week == unique(survey$week)[i], ]
    prevmonday <- 7 * floor(as.numeric(as.Date(survey_reported$local_time[1])-1+4) / 7) + as.Date(1-4, origin = "1970-01-01") 
    rep_percent <- nrow(survey_reported)/expected_doses
    survey_taken <- survey_reported[survey_reported$response_value == 1, ]
    taken_percent <- nrow(survey_taken)/expected_doses
    summary <- rbind(summary, data.frame(week_start = prevmonday,
                                         percent = rep_percent,
                                         category = "reported"))
    summary <- rbind(summary, data.frame(week_start = prevmonday,
                                         percent = taken_percent,
                                         category = "taken"))
  }
  summary$week_start <- substr(as.character(summary$week_start), 6, 
                               nchar(as.character(summary$week_start)))
  
  # 80% compliance line
  compliance <- data.frame(x = c(summary$week_start[1],
                                 summary$week_start[nrow(summary)]),
                           y = c(.8, .8))
  
  
  # Bar chart 

  summary %>%
    #start with barchart
    mutate(week_category = factor(paste(week_start, category))) %>%
    ggvis(x = ~week_category, y = ~percent, fill = ~category) %>% 
    layer_bars(stack = FALSE) %>%
    
    #add the initial x axis in order to set x labes to blank
    add_axis('x', title='Week Start', title_offset = 75,
             properties = axis_props(labels=list(fill='blank', angle = 45, align = "left")))# %>%
    
    
    #details for right axis i.e. the bars
    #add_axis("y", orient = "left", title = "Percent") %>% 
     
    #details for left axis i.e. the lines + plotting of lines 
    #add_axis("y", "ylines", orient = "left", title= "Percent", grid=F ) %>%
    #layer_lines(stroke := "red", prop('y', ~compliance)) %>%
    
    #add new axis which will be for our categorical x axis
    #add_axis('x', 'myx2', orient='bottom', title='') %>%
    
    #add categorical data and make lines invisible (we only need the categories anyway)
    #layer_lines(prop("x", ~week_start, scale = "myx2"), stroke := 'blank')
}