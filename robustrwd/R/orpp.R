

add_orpp_inpatient_visits <- function(data,
                                 inpatient_tbl,
                                 pid = "desynpuf_id") {
  assertthat::assert_that("desynpuf_id" %in% colnames(data))
  assertthat::assert_that("desynpuf_id" %in% colnames(inpatient_tbl))

  assert_is_orpp(data)

  inpatient_orpp <- inpatient_tbl %>%
    dplyr::group_by(desynpuf_id) %>%
    dplyr::summarise(
      has_inpatient_claims = TRUE,
      inpatient_claims_n = dplyr::n(),
      inpatient_payment_median = median(clm_pmt_amt),
      inpatient_payment_min = min(clm_pmt_amt),
      inpatient_payment_max = max(clm_pmt_amt),
      claims_days_median = median(clm_utlztn_day_cnt)
    )

  definition_tbl <- tibble::tribble(
    ~column, ~definition,
    "has_inpatient_claims", "Flag if patient has any inpatient claims",
    "inpatient_claims_n", "Number of inpatient claims",
    "inpatient_payment_median", "Median dollar amount of inpatient claims",
    "inpatient_payment_min", "Minimum dollar amount of inpatient claims",
    "inpatient_payment_max", "Maximum dollar amount of inpatient claims",
    "claims_days_median", "Median number of number of covered days of care that are chargeable to Medicare facility"
  )

  data_out <- data %>%
    dplyr::left_join(inpatient_orpp,
      by = pid
    ) %>%
    tidyr::replace_na(list("inpatient_claims_n" = 0,
                           "has_inpatient_claims" = FALSE))

  message_new_vars(
    data_original = data,
    data_out = data_out
  )

  data_out
}
