
#' Read all of the CSV Zip files in a folder
#'
#' @param A folder containing files (with the extension .csv.zip) to read
#'
read_folder_csv_zips <- function(folder) {
  list.files(folder, pattern = ".*csv.zip") %>%
    {
      set_names(., gsub(".csv.zip$", "", .))
    } %>%
    map(~ suppressWarnings({
      read_csv(file.path(folder, .x), show_col_types = FALSE)
    }))
}


receive_delivery_01 <- function() {
  if (is_noisy()) {
    cli::cli_h1("Receiving a delivery labelled as 01 from provider")
  }

  list.files("data", pattern = "csv$") %>%
    stringr::str_subset(pattern = "01") %>%
    {
      set_names(
        .,
        stringr::str_remove_all(., ".csv") %>%
          stringr::str_remove_all("01")
      )
    } %>%
    map(~ suppressWarnings({
      if (is_noisy()) {
        cli::cli_alert_info("Received {crayon::bold(crayon::magenta(.x))}")
      }

      read_csv(file.path("data", .x), show_col_types = FALSE)
    }))
}

receive_delivery_02 <- function() {
  if (is_noisy()) {
    cli::cli_h1("Receiving a delivery labelled as 02 from provider")
  }

  list.files("data", pattern = "csv$") %>%
    stringr::str_subset(pattern = "02") %>%
    {
      set_names(
        .,
        stringr::str_remove_all(., ".csv") %>%
          stringr::str_remove_all("02")
      )
    } %>%
    map(~ suppressWarnings({
      if (is_noisy()) {
        cli::cli_alert_info("Received {crayon::bold(crayon::magenta(.x))}")
      }
      read_csv(file.path("data", .x), show_col_types = FALSE)
    }))
}
