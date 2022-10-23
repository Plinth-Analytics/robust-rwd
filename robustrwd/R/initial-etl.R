
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
#' @importFrom lubridate year ymd years
#' @importFrom dplyr rename_all mutate mutate_at
initial_etl_bene <- function(bene_df) {
  bene_df %>%
    rename_all(tolower) %>%
    mutate_at(
      c("sp_alzhdmta", "sp_chf", "sp_chrnkidn", "sp_cncr",
        "sp_copd", "sp_depressn", "sp_diabetes", "sp_ischmcht",
        "sp_osteoprs", "sp_ra_oa", "sp_strketia"),
      ~ .x == 1
    ) %>%
    mutate_at(
      "bene_sex_ident_cd",
      ~ factor_as_string(.x, levels = 1:2, labels = c("Male", "Female"))
      ) %>%
    mutate_at(
      "bene_race_cd",
      ~ factor_as_string(
        .x,
        levels = c(1:2, 3, 5),
        labels = c("White", "Black", "Other", "Hispanic")
      )
    ) %>%
    mutate_at(
      "sp_state_code",
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
    mutate_at(c("bene_birth_dt", "bene_death_dt"), ymd) %>%
    mutate(
      years_until_death = as.numeric(difftime(bene_death_dt, bene_birth_dt, units = "days")) / 365.25,
      coverage_end_month = ymd("2008-01-01") + months(bene_hi_cvrage_tot_mons),
      years_alive_so_far = as.numeric(difftime(coverage_end_month, bene_birth_dt, units = "days")) / 365.25,
      survival_years = coalesce(years_until_death, years_alive_so_far),
      death_observed = !is.na(bene_death_dt)
    ) %>%
    rename_all(~ gsub("(bene|sp)_", "", .x))
}

initial_etl_inpatient <- function(inpatient_df) {
  # maybe do some ETL on the inpatient data.frame
  inpatient_df %>%
    rename_all(tolower)
}

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

