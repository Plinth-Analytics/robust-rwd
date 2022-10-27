#' Add ORPP variables related to inpatient events
#'
#' @param data tbl. A ORPP table
#' @param inpatient_tbl tbl. An MRPP table representing inpatient events
#' @param pid character. The name of the unique patient id column
#'
#' @return tbl. Data with specific derived columns added
#' @export
#'
#' @examples
add_orpp_inpatient <- function(data,
                               inpatient_tbl,
                               pid = "desynpuf_id") {
  assert_is_inpatient(inpatient_tbl)
  assert_is_orpp(data)
  assertthat::assert_that("desynpuf_id" %in% colnames(data))
  assertthat::assert_that("desynpuf_id" %in% colnames(inpatient_tbl))

  inpatient_orpp <- inpatient_tbl %>%
    dplyr::group_by(desynpuf_id) %>%
    dplyr::summarise(
      has_inpatient_claims = TRUE,
      inpatient_claims_n = dplyr::n(),
      inpatient_payment_median = median(clm_pmt_amt),
      inpatient_payment_min = min(clm_pmt_amt),
      inpatient_payment_max = max(clm_pmt_amt)
    )

  data_out <- data %>%
    dplyr::left_join(inpatient_orpp,
      by = pid
    ) %>%
    tidyr::replace_na(list(
      "inpatient_claims_n" = 0,
      "has_inpatient_claims" = FALSE
    ))

  if (is_noisy()) {
    message_new_vars(
      data_original = data,
      data_out = data_out,
      data_new_source = "inpatient"
    )
  }

  data_out
}


#' Add ORPP variables related to prescription events
#'
#' @param data tbl. A ORPP table
#' @param prescription_tbl tbl. An MRPP table representing prescription events
#' @param pid character. The name of the unique patient id column
#'
#' @return tbl. Data with specific derived columns added
#' @export
#'
#' @examples
add_orpp_prescription <- function(data,
                                  prescription_tbl,
                                  pid = "desynpuf_id") {
  assert_is_prescription(prescription_tbl)
  assert_is_orpp(data)

  assertthat::assert_that("desynpuf_id" %in% colnames(data))
  assertthat::assert_that("desynpuf_id" %in% colnames(prescription_tbl))

  prescription_orpp <- prescription_tbl %>%
    dplyr::group_by(desynpuf_id) %>%
    dplyr::summarise(
      has_prescription = TRUE,
      prescription_n = dplyr::n(),
      prescription_patient_payment_median = median(ptnt_pay_amt),
      prescription_rx_cost_median = median(tot_rx_cst_amt),
      prescription_patient_payment_percentage_median = median(ptnt_pay_amt / tot_rx_cst_amt)
    )

  data_out <- data %>%
    dplyr::left_join(prescription_orpp,
      by = pid
    ) %>%
    tidyr::replace_na(list(
      "prescription_n" = 0,
      "has_prescription" = FALSE
    ))


  if (is_noisy()) {
    message_new_vars(
      data_original = data,
      data_out = data_out,
      data_new_source = "prescription"
    )
  }

  data_out
}
