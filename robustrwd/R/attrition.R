# attrition.R
#  Functions to create and communication cohort attrition

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
#' @importFrom rlang enquos
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

#' Plot an attrition diagram from an attrition table object
#'
#' @param attrition_tbl tbl.
#' @param nudge_label_x numeric. How much to nudge labels
#'
#' @return ggplot2

plot_attrition <- function(attrition_tbl,
                           title = "Cohort Attrition",
                           nudge_label_x = .01) {
  attrition_tbl <- attrition_tbl %>%
    mutate(
      step = 1:nrow(attrition_tbl),
      criteria = factor(description, levels = rev(description)),
      type = "inclusion"
    )

  attrition_gg <- ggplot2::ggplot(
    attrition_tbl,
    ggplot2::aes(
      x = n,
      y = criteria,
      label = scales::comma(n),
      fill = type
    )
  ) +
    ggplot2::geom_bar(stat = "identity", col = "black", fill = rgb(71, 131, 241, maxColorValue = 255)) +
    ggplot2::geom_label(fill = "white", nudge_x = nudge_label_x * max(attrition_tbl$n), hjust = "inward") +
    ggplot2::labs(title = title, y = "") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_x_continuous(labels = scales::comma)

  return(attrition_gg)
}
