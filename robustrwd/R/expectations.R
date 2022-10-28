
#' Define expectations for patients data from medicare
#'
#' @param agent. An agent created with pointblank::create_agent()
#'
#' @return ptblank_agent
#'
expectations_patients <- function(agent) {
  agent %>%
    # do the columns desynpuf_id and clm_admsn_dt exist?
    col_exists(
      vars(
        desynpuf_id, birth_dt,
        death_dt, sex_ident_cd,
        race_cd, cncr, diabetes
      ),
      label = "Key columns exist"
    ) %>%
    # Are patient IDs unique?
    rows_distinct(vars(desynpuf_id),
      label = "Dataset has only One Row Per Patient (OORP)"
    ) %>%
    # All dates after 1900
    col_vals_gt(vars(birth_dt, death_dt),
      value = ymd("19000101"),
      label = "Everyone born after 01-01-1900",
      na_pass = TRUE
    ) %>%
    col_vals_make_set(vars(death_observed),
      set = c(TRUE, FALSE),
      label = "Dataset contains alive and dead patients"
    ) %>%
    col_vals_make_set(vars(sex_ident_cd),
      set = c("Male", "Female"),
      label = "Dataset contains both Male and Female patients"
    ) %>%
    col_vals_make_set(vars(cncr),
      set = c(TRUE, FALSE),
      label = "Dataset contains patients with and without cancer"
    )
}

#' Define expectations for inpatient data from medicare
#'
#' @param agent. An agent created with pointblank::create_agent()
#'
#' @return ptblank_agent
#'
expectations_inpatient <- function(agent) {
  agent %>%
    # do the columns desynpuf_id and clm_admsn_dt exist?
    col_exists(vars(desynpuf_id),
      label = "Key columns exist"
    ) %>%
    # Is the claim from date and claim through date column(s) both dates?
    col_is_date(vars(clm_from_dt, clm_thru_dt),
      label = "Claim admin is a date"
    ) %>%
    # Is the claim payment amount greater than 0?
    col_vals_gte(vars(clm_pmt_amt),
      value = 0,
      label = "Claim payment amount is positive"
    )
}


#' Define expectations for a ORPP object derived from medicare tables
#'
#' @param agent
#'
#' @return ptblank_agent
#'
expectations_orpp <- function(agent) {
  agent %>%
    # Are patient IDs unique?
    rows_distinct(vars(desynpuf_id),
      label = "Dataset has only One Row Per Patient (OORP)"
    ) %>%
    # Are patient IDs unique?
    col_vals_gt(vars(survival_years),
      value = 0,
      label = "No patient has negative survival time"
    )
}
