
read_folder_csv_zips <- function(folder) {
  list.files(folder) %>% 
  { set_names(., gsub(".csv.zip$", "", .)) } %>%
  map(~ read_csv(file.path(folder, .x), show_col_types = FALSE))
}

