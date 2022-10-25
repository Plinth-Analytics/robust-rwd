

add_inpatient_visits <- function(data,
                                 inpatient_tbl,
                                 pid = "desynpuf_id") {
  assertthat::assert_that("desynpuf_id" %in% colnames(data))
  assertthat::assert_that("desynpuf_id" %in% colnames(inpatient_tbl))

  assert_is_orpp(data)

  inpatient_orpp <- inpatient_tbl %>%
    dplyr::group_by(desynpuf_id) %>%
    dplyr::summarise(
      has_inpatient = TRUE,
      inpatient_claims_n = dplyr::n(),
      inpatient_payment_median = median(clm_pmt_amt),
      inpatient_payment_min = min(clm_pmt_amt),
      inpatient_payment_max = max(clm_pmt_amt)
    )

  definition_tbl <- tibble::tribble(
    ~column, ~definition,
    "inpatient_claims_n", "Number of inpatient claims",
    "inpatient_payment_median", "Median dollar amount of inpatient claims",
    "inpatient_payment_min", "Minimum dollar amount of inpatient claims",
    "inpatient_payment_max", "Maximum dollar amount of inpatient claims"
  )

  data_out <- data %>%
    dplyr::left_join(inpatient_orpp,
      by = pid
    )

  message_new_vars(
    data_original = data,
    data_out = data_out
  )

  data_out
}
