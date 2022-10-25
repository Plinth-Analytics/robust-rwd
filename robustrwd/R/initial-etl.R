
factor_as_string <- compose(as.character, factor)

#' Do initial ETL on the beneficiaries table
#'
#' This function will:
#'   * transform conditions to TRUE/FALSE instead of 1/2
#'   * transform sex/race/state codes to strings
#'   * calculate age in years
#'
#' Ideally, we would specify levels/labels elsewhere so they're more easily
#' modified
#' 
#' @param bene_df A Beneficiary table (one row per patient) provided by CMS. 
#'
#' @importFrom lubridate ymd 
#' @importFrom dplyr rename_all mutate mutate_at
#' 
#' 
initial_etl_bene <- function(bene_df) {
  bene_df %>%
    rename_all(tolower) %>%
    rename_all(~ gsub("(bene|sp)_", "", .x)) %>%
    mutate_at(
      c("alzhdmta", "chf", "chrnkidn", "cncr",
        "copd", "depressn", "diabetes", "ischmcht",
        "osteoprs", "ra_oa", "strketia"),
      ~ .x == 1
    ) %>%
    mutate_at(
      "sex_ident_cd",
      ~ factor_as_string(.x, levels = 1:2, labels = c("Male", "Female"))
      ) %>%
    mutate_at(
      "race_cd",
      ~ factor_as_string(
        .x,
        levels = c(1:2, 3, 5),
        labels = c("White", "Black", "Other", "Hispanic")
      )
    ) %>%
    mutate_at(
      "state_code",
      ~ factor_as_string(
        .x,
        levels =
          c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10",
          "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21",
          "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32",
          "33", "34", "35", "36", "37", "38", "39", "41", "42", "43", "44",
          "45", "46", "47", "49", "50", "51", "52", "53", "54"),
        labels =
          c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
          "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
          "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
          "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
          "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "Others")
      )
    ) %>%
    # need to supply a time zone for posix dates
    mutate_at(c("birth_dt", "death_dt"), ymd, tz = "UTC") %>%
    mutate(
      years_until_death = as.numeric(difftime(death_dt, birth_dt, units = "days")) / 365.25,
      coverage_end_month = ymd("2008-01-01") + months(hi_cvrage_tot_mons),
      years_alive_so_far = as.numeric(difftime(coverage_end_month, birth_dt, units = "days")) / 365.25,
      survival_years = coalesce(years_until_death, years_alive_so_far),
      death_observed = !is.na(death_dt)
    )
}


#' ETL for inpatient tables
#' 
#' @param An inpatient claims table provided by CMS
#' 
#' 
initial_etl_inpatient <- function(inpatient_df) {
  # maybe do some ETL on the inpatient data.frame
  inpatient_df %>%
    rename_all(tolower)
}

#' Initial ETL over available tables
#' 
#' Given a set of tables (with the strings "bene" and "inpatient" in their names)
#' 
#' @param tables A named list of tables
#' 
# could make a function factory out of this if needed
initial_etl <- function(tables) {
  # could:
  #  * map over named list of functions
  #  * make sure expected tables are present
  # but this may be easier to review
  which_tables <-
    map(set_names(c("bene", "inpatient")), ~ grep(.x, names(tables), value = TRUE))

  tables[which_tables$bene] <- map(tables[which_tables$bene], initial_etl_bene)
  tables[which_tables$inpatient] <- map(tables[which_tables$inpatient], initial_etl_inpatient)

  tables
}

