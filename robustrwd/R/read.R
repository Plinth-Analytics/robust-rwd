
#' Read all of the CSV Zip files in a folder
#'
#' @param A folder containing files (with the extension .csv.zip) to read
#'
read_folder_csv_zips <- function(folder) {
  list.files(folder, pattern = ".*csv.zip") %>%
    {
      set_names(., gsub(".csv.zip$", "", .))
    } %>%
    map(~ suppressWarnings({read_csv(file.path(folder, .x), show_col_types = FALSE)}))
}


read_data_delivery_01 <- function() {

  list.files("data", pattern = "csv$") %>%
    stringr::str_subset(pattern = "01") %>%
    {
      set_names(.,
                stringr::str_remove_all(., ".csv") %>%
                stringr::str_remove_all("01"))
    } %>%
    map(~ suppressWarnings({read_csv(file.path("data", .x), show_col_types = FALSE)}))

}

read_data_delivery_02 <- function() {

  list.files("data", pattern = "csv$") %>%
    stringr::str_subset(pattern = "02") %>%
    {
      set_names(.,
                stringr::str_remove_all(., ".csv") %>%
                  stringr::str_remove_all("02"))
    } %>%
    map(~ suppressWarnings({read_csv(file.path("data", .x), show_col_types = FALSE)}))

}
