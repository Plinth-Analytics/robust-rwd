#' Summarise row fail information from an interrogation object
#'
#' @param interrogation interrogation. A object created by pointblank::interrogate()
#'
#' @return numeric. A named vecor
#' @export
#'
#' @examples
summarise_fail <- function(interrogation) {
  out <- interrogation %>%
    pointblank::get_agent_x_list() %>%
    purrr::pluck("n_failed") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(test = interrogation$validation_set$label) %>%
    dplyr::mutate(column = unlist(interrogation$validation_set$column)) %>%
    dplyr::rename(fail_n = value) %>%
    dplyr::arrange(desc(fail_n), column) %>%
    dplyr::mutate(any_fail = fail_n > 0)

  fail_rows_n <- sum(out$any_fail)

  if (all(!out$any_fail)) {
    cli::cli_alert_success("{cli::style_bold(cli::col_green('All Pass'))} No failures across all {nrow(out)} Tests!")
  } else {
    cli::cli_alert_danger("{cli::style_bold(cli::col_red(paste(fail_rows_n, 'failures')))} out of {nrow(out)} tests.")
  }

  out
}


# NP reconsidering whether we want to actually use this or not
check_inpatient <- function(tbl) {
  expectations_inpatient <- function(obj) {
    obj %>%
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

  cli::cli_alert_info("Interrogating {cli::style_bold(cli::col_blue('inpatient'))}")

  inpatient_interroggation <- create_agent(tbl,
    tbl_name = "Inpatient",
    label = "Inpatient data (Post ETL)"
  ) %>%
    expectations_inpatient() %>%
    suppressMessages({
      interrogate()
    })

  fail_summary <- inpatient_interroggation %>%
    summarise_fail()

  for (test_i in 1:nrow(fail_summary)) {
    dat <- fail_summary %>%
      dplyr::slice(test_i)

    browser()

    if (dat$any_fail) {
      cli::cli_alert_danger("Fail | Col = {cli::style_bold(cli::style_underline(cli::col_cyan(dat$column)))} | Why = {cli::style_underline(cli::style_bold(cli::col_green(dat$test)))} | N = {cli::style_underline(cli::style_bold(cli::col_magenta(dat$fail_n)))}")
    } else {
      cli::cli_alert_success("Pass | Col = {cli::style_bold(dat$column)} | Why = {cli::style_bold(dat$test)}")
    }
  }
}
