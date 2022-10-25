
#' Read all of the CSV Zip files in a folder
#'
#' @param A folder containing files (with the extension .csv.zip) to read
#'
read_folder_csv_zips <- function(folder) {
  list.files(folder, pattern = ".*csv.zip") %>%
    {
      set_names(., gsub(".csv.zip$", "", .))
    } %>%
    map(~ read_csv(file.path(folder, .x), show_col_types = FALSE))
}
