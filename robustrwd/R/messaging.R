be_noisy <- function() {
  Sys.setenv("noisy" = TRUE)
}

be_quiet <- function() {
  Sys.setenv("noisy" = FALSE)
}

is_noisy <- function() {
  noisy_status <- Sys.getenv("noisy")

  if (identical(noisy_status, "")) {
    be_noisy()

    return(TRUE)
  } else {
    return(as.logical(noisy_status))
  }
}

message_new_vars <- function(data_original,
                             data_out,
                             data_new_source = "unspecified",
                             use_definitions = TRUE,
                             definition_tbl = NULL) {
  vars_new <- colnames(data_out) %>%
    dplyr::setdiff(colnames(data_original))

  if (!use_definitions) {
    cli::cli_alert_info("ORPP | Added var(s) from {crayon::bold(crayon::yellow(data_new_source))}: {crayon::blue(crayon::bold(paste(vars_new, collapse = ', ')))}")
  } else {
    if (is.null(definition_tbl)) {
      definition_tbl <- getOption("definition_tbl")
    }

    definitions_new <- definition_tbl %>%
      dplyr::filter(column %in% vars_new) %>%
      dplyr::mutate(
        column = crayon::bold(crayon::blue(column)),
        definition = crayon::green(definition)
      ) %>%
      tidyr::unite("val_def", column:definition, sep = " = ") %>%
      dplyr::select(val_def) %>%
      unlist()

    names(definitions_new) <- rep("*", length(definitions_new))

    cli::cli_alert_info("ORPP | Added var(s) from {crayon::bold(crayon::yellow(data_new_source))}:")

    cli::cli_bullets(definitions_new)
  }
}
