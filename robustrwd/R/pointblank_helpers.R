#' Summarise row fail information from an interrogation object
#'
#' @param interrogation interrogation. A object created by pointblank::interrogate()
#'
#' @return numeric. A named vecor
#' @export
#'
#' @examples
summarise_fail <- function(interrogation) {

  interrogation %>%
    pointblank::get_agent_x_list() %>%
    purrr::pluck("n_failed") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(test = interrogation$validation_set$label) %>%
    dplyr::mutate(column = unlist(interrogation$validation_set$column)) %>%
    dplyr::rename(fail_n = value) %>%
    dplyr::arrange(desc(fail_n), column) %>%
    dplyr::mutate(any_fail = fail_n > 0)

}
