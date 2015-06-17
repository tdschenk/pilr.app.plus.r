## Weekly graph of percent reported, percent taken, and 80% cutoff line
## Weeks are monday to sunday
## Bars are NOT stacked
# REQUIRES XTS
#' @export
reported_taken_bar <- function(data, params, ...) {
  stop(paste0("Names: ", names(data), "  Length: ", length(data), "  Class: ", class(data)))
  # Add weekday to dataframe
  data$survey.metadata.local_time <- as.POSIXlt(data$survey.metadata.local_time, format = "%Y-%m-%dT%H:%M:%SZ")
  data$survey.data.week <- format(data$survey.metadata.local_time, format = "%W")
  # Unlist columns
  ##dosage <- as.data.frame(t(apply(dosage, 1, unlist)))
  # Remove empty dosage_id rows
  data <- data[data$dosage.data.id != "", ]
  # Find total expected dosages
  dosage_final <- data.frame(dosage_id = character())
  for (i in 1:length(unique(data$dosage.data.dosage_id))) {
    dosage_subset <- data[data$dosage.data.dosage_id == 
                              as.character(unique(data$dosage.data.dosage_id))[i], ]
    if (as.character(dosage_subset$action[nrow(dosage_subset)]) == "EDIT" |
        as.character(dosage_subset$action[nrow(dosage_subset)]) == "CREATE")
      dosage_final <- rbind(dosage_final, 
                            data.frame(dosage_id = as.character(unique(data$dosage.data.dosage_id))[i]))
  }
  # Expected doses per week
  expected_doses <- 7*nrow(dosage_final)
  # Calculate percent reported and percent taken
  summary <- data.frame(week_start = character(),
                        percent  = numeric(),
                        category = numeric())
  for (i in 1:length(unique(data$survey.data.week))) {
    survey_reported <- data[survey$week == unique(survey$week)[i], ]
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

  # Bar chart 
  summary %>%
    ggvis(x = ~week_start, y = ~percent, fill = ~category) %>%
    layer_bars(stack = FALSE) %>%
    add_axis('x', title = 'Week Start') %>%
    add_axis('y', title = 'Percent') %>%
    add_axis('y', title = 'Percent', orient = 'right')
}

## A table with columns: pt, week #, total doses, doses reported on, doses taken
#' @export
app_plus_table <- function(data, params, ...) {
  dosage <- data$dosage
  survey <- data$survey
  # Add weekday to dataframe
  survey$local_time <- as.POSIXlt(survey$local_time, format = "%Y-%m-%dT%H:%M:%SZ")
  survey$week <- format(survey$local_time, format = "%W")
  # Unlist columns
  dosage <- as.data.frame(t(apply(dosage, 1, unlist)))
  # Remove empty dosage_id rows
  dosage <- dosage[dosage$dosage_id != "", ]
  # Find total expected dosages
  dosage_final <- data.frame(dosage_id = character())
  for (i in 1:length(unique(dosage$dosage_id))) {
    dosage_subset <- dosage[dosage$dosage_id == 
                              as.character(unique(dosage$dosage_id))[i], ]
    if (as.character(dosage_subset$action[nrow(dosage_subset)]) == "EDIT" |
        as.character(dosage_subset$action[nrow(dosage_subset)]) == "CREATE")
      dosage_final <- rbind(dosage_final, 
                            data.frame(dosage_id = as.character(unique(dosage$dosage_id))[i]))
  }
  # Expected doses per week
  expected_doses <- 7*nrow(dosage_final)
  # Calculate percent reported and percent taken
  mx <- matrix(nrow = length(unique(survey$week)), ncol = 5)
  rownames(mx) <- c(paste0(data$pt[1]))
  colnames(mx) <- c("Week Start", "Reported", "Taken", "Expected", "% Taken")
  ## Stopped here
  for (i in 1:length(unique(survey$week))) {
    survey_reported <- survey[survey$week == unique(survey$week)[i], ]
    prevmonday <- 7 * floor(as.numeric(as.Date(survey_reported$local_time[1])-1+4) / 7) + as.Date(1-4, origin = "1970-01-01") 
    total_rep <- nrow(survey_reported)
    total_taken <- nrow(survey_reported[survey_reported$response_value == 1, ])
    mx[i, 1] <- prevmonday %>% as.POSIXlt(format = "%Y-%m-%dT%H:%M:%S") %>% as.character()
    mx[i, 2] <- total_rep
    mx[i, 3] <- total_taken
    mx[i, 4] <- expected_doses
    mx[i, 5] <- round(total_taken/expected_doses, 4)
  }
  print(xtable(mx), type = "html")
}