
#' Factory to create step counters
#'
#' @importFrom dplyr count filter mutate
#' @importFrom rlang `!!`
#'
create_attrition_factory <- function(.df) {
  function(criterion, name) {
    .df <<- filter(.df, !!criterion)
    mutate(count(.df), description = name)
  }
}

#' Count how many rows are filtered out of a dataframe (sequentially)
#'
#' @param .df A table whose rows will be counted
#' @param ... Criteria (named, if you like) to apply to `.df`
#'
#' @importFrom rlang quo
#' @importFrom purrr reduce2
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#'
#' @examples create_attrition(mtcars, "vs are 1" = vs == 1, "mpg less than 20" = mpg < 20)
#'
create_attrition <- function(.df, ...) {
  # there's something nice about writing "Everyone" = TRUE
  criteria <- append(list("Everyone" = quo(TRUE)), enquos(...))
  counter <- create_attrition_factory(.df)

  reduce2(criteria, names(criteria), function(acc, cr, n_cr) {
    bind_rows(acc, counter(cr, n_cr))
  }, .init = tibble(description = character(0), n = integer(0))) %>%
    dplyr::mutate(n_dropped = n - lag(n, 1))
}

#' @importFrom dplyr filter
#' @importFrom rlang `!!!` enquos
apply_inclusion <- function(.df, ...) {
  list(
    attrition = create_attrition(.df, ...),
    df = filter(.df, !!!unname(enquos(...)))
  )
}
