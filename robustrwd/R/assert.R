assert_is_orpp <- function(data,
                           pid = "desynpuf_id") {
  assertthat::assert_that(pid %in% colnames(data))

  pid_sym <- rlang::sym(pid)

  is_orpp <- data %>%
    dplyr::group_by(!!pid_sym) %>%
    dplyr::tally() %>%
    dplyr::ungroup() %>%
    dplyr::summarise(n_max = max(n)) %>%
    dplyr::pull(n_max) %>%
    `==`(1)

  assertthat::assert_that(is_orpp,
    msg = "Data is not one-row-per-patient (ORPP)!"
  )
}

#' Assert that a table is an 'inpatient' table
#'
#' @param data tbl.
#'
#' @return
#' @export
#'
#' @examples
assert_is_inpatient <- function(data) {
  # Minimal expectations of included columns
  cols_expected <- c(
    "desynpuf_id", "clm_id", "segment", "clm_from_dt", "clm_thru_dt",
    "prvdr_num", "clm_pmt_amt", "nch_prmry_pyr_clm_pd_amt", "at_physn_npi"
  )

  cols_missing <- setdiff(cols_expected, colnames(data))

  assertthat::assert_that(length(cols_missing) == 0,
    msg = glue::glue("Does not meet expctations for an inpatient table. The following expected columns are missing {paste(cols_missing, collapse = ', ')}")
  )
}


#' Assert that a table is a 'prescription' table
#'
#' @param data tbl.
#'
#' @return
#' @export
#'
#' @examples
assert_is_prescription <- function(data) {
  # Minimal expectations of included columns
  cols_expected <- c(
    "desynpuf_id", "pde_id", "srvc_dt", "prod_srvc_id", "qty_dspnsd_num",
    "days_suply_num", "ptnt_pay_amt", "tot_rx_cst_amt"
  )

  cols_missing <- setdiff(cols_expected, colnames(data))

  assertthat::assert_that(length(cols_missing) == 0,
    msg = glue::glue("Does not meet expctations for an inpatient table. The following expected columns are missing {paste(cols_missing, collapse = ', ')}")
  )
}
