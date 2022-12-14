#' @importFrom purrr compose
factor_as_string <- compose(as.character, factor) #' Initial ETL over available tables
#'
#' Given a set of tables (with the strings "bene" and "inpatient" in their names)
#'
#' @param tables A named list of tables
#'
# could make a function factory out of this if needed
etl_02 <- function(tables) {
  if (is_noisy()) {
    cli::cli_h1("Applying ETL v01 to data")
  }

  which_tables <-
    map(set_names(c("patients", "inpatient", "prescription", "outpatient")), ~ grep(.x, names(tables), value = TRUE))

  tables[which_tables$patients] <- map(tables[which_tables$patients], etl_patients_02)

  tables[which_tables$inpatient] <- map(tables[which_tables$inpatient], etl_inpatient_02)

  tables[which_tables$prescription] <- map(tables[which_tables$prescription], etl_prescription_02)

  tables[which_tables$outpatient] <- map(tables[which_tables$outpatient], etl_outpatient_02)

  tables
}

#' ETL for a patients table tables
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
etl_patients_02 <- function(bene_df) {
  if (is_noisy()) {
    cli::cli_alert_info("Applying ETL (v02) to {crayon::bold(crayon::magenta('patients'))} table")
  }

  bene_df %>%
    rename_all(tolower) %>%
    rename_all(~ gsub("(bene|sp)_", "", .x)) %>%
    mutate_at(
      c(
        "alzhdmta", "chf", "chrnkidn", "cncr",
        "copd", "depressn", "diabetes", "ischmcht",
        "osteoprs", "ra_oa", "strketia"
      ),
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
          c(
            "02", "02", "03", "04", "05", "06", "07", "08", "09", "10",
            "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21",
            "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32",
            "33", "34", "35", "36", "37", "38", "39", "41", "42", "43", "44",
            "45", "46", "47", "49", "50", "51", "52", "53", "54"
          ),
        labels =
          c(
            "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
            "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM",
            "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
            "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "Others"
          )
      )
    ) %>%
    # need to supply a time zone for posix dates
    mutate_at(c("birth_dt", "death_dt"), ymd) %>%
    mutate(
      years_until_death = as.numeric(difftime(death_dt, birth_dt, units = "days")) / 365.25,
      coverage_end_month = ymd("2008-02-02") + months(hi_cvrage_tot_mons),
      years_alive_so_far = as.numeric(difftime(coverage_end_month, birth_dt, units = "days")) / 365.25,
      survival_years = coalesce(years_until_death, years_alive_so_far),
      death_observed = !is.na(death_dt),
      pppymt_ip = case_when(
        rbinom(length(pppymt_ip), 1, 0.02) == 1 &
          esrd_ind == "Y" ~ round(rnorm(length(pppymt_ip), -100000, sd = 100)),
        TRUE ~ pppymt_ip
      )
    ) %>%
    select(
      desynpuf_id, birth_dt, death_dt, sex_ident_cd, race_cd,
      esrd_ind, hi_cvrage_tot_mons, pppymt_ip, diabetes, cncr,
      years_until_death, coverage_end_month, years_alive_so_far, survival_years,
      death_observed
    )
}

#' ETL for inpatient tables
#'
#' @param inpatient_df An inpatient claims table provided by CMS
#'
#'
etl_inpatient_02 <- function(inpatient_df) {
  if (is_noisy()) {
    cli::cli_alert_info("Applying ETL (v02) to {crayon::bold(crayon::magenta('inpatient'))} table")
  }

  # maybe do some ETL on the inpatient data.frame
  inpatient_df %>%
    rename_all(tolower) %>%
    mutate(across(ends_with("dt"), ~ lubridate::ymd(.x))) %>%
    select(desynpuf_id, clm_id, clm_from_dt, clm_thru_dt, clm_pmt_amt, prvdr_num)
}

#' ETL for outpatient tables
#'
#' @param outpatient_df An outpatient claims table provided by CMS
#'
#'
etl_outpatient_02 <- function(outpatient_df) {
  if (is_noisy()) {
    cli::cli_alert_info("Applying ETL (v02) to {crayon::bold(crayon::magenta('outpatient'))} table")
  }

  # maybe do some ETL on the inpatient data.frame
  outpatient_df %>%
    rename_all(tolower) %>%
    mutate(across(ends_with("dt"), ~ lubridate::ymd(.x))) %>%
    select(desynpuf_id, clm_id, clm_from_dt, clm_thru_dt, clm_pmt_amt, prvdr_num)
}

#' ETL for inpatient tables
#'
#' @param prescription_df A prescription claims table provided by CMS
#'
#'
etl_prescription_02 <- function(prescription_df) {
  if (is_noisy()) {
    cli::cli_alert_info("Applying ETL (v02) to {crayon::bold(crayon::magenta('prescription'))} table")
  }


  # maybe do some ETL on the inpatient data.frame
  prescription_df %>%
    rename_all(tolower) %>%
    mutate(across(ends_with("dt"), ~ lubridate::ymd(.x))) %>%
    select(desynpuf_id, pde_id, srvc_dt, days_suply_num, ptnt_pay_amt, tot_rx_cst_amt)
}
