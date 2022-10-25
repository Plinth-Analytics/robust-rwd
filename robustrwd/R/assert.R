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
