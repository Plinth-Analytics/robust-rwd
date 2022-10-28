# Started working on this "just one more thing" script to show it all at once
# but not sure if it's worth it

# Could also be implemented in a markdown document

engine_demo <- function() {

data_delivered <- receive_delivery_02()

data_etld <- data_delivered %>%
  etl_02()

# Interrogate person -----------------------------------------------------------

cli::cli_h1("Interrogating .$patients data...")

patients_interrogation_tidy <- data_etld$patients   %>%
  create_agent(
    tbl_name = "Persons",
    label = "Persons data (Post ETL)"
  ) %>%
  expectations_patients() %>%
  interrogate()  %>%
  tidy_interrogation()

if (!any(patients_interrogation_tidy$any_fail)) {

  cli::cli_alert_success("All tests pass for .$patients data")

} else {

  cli::cli_alert_danger("All tests did NOT pass for .$patients data")

  stop()

}


}
