# Set options
options(pilr_server_default = "http://app_plus.pilrhealth.com")
options(pilr_project_default = "horvath_testing_3")
options(pilr_default_access_code = "be1eceb7-353e-44bf-9a16-4c08d44eced8")

# Grab data
data <- list(survey = read_pilr(data_set = "pilrhealth:mobile:survey_data", schema = "1",
                                query_params = list(participant = "904")),
             dosage = read_pilr(data_set = "pilrhealth:app_plus:dosage_schedule", schema = "1",
                                query_params = list(participant = "904")))
params <- ""

# Bar chart reported/taken/80% compliance
dash1 <- reported_taken_bar(data, params)
dash1

# Table
dash2 <- app_plus_table(data, params)
dash2

#- on the backend, they would like a simple way to get some measures of #
#     compliance, instead of having to manually tally all their info.
#- could we make some dashboard panels for that?
#- metrics per person
#- total taken and expected counts, with percent
#- tank levels for each week in the past, week by week
#- number of weeks they reported on 80% of the dosages
#- number of weeks they took 80% of their dosages
#- maybe a bar chart, one bar set for each week on x, and y of 1) percent 
#     reported on, 2) percent taken, and a vertical line at 80%
#- a table with columns: pt, week #, total doses, doses reported on, doses taken
