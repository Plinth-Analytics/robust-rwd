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
                             definition_tbl = NULL) {
  vars_new <- colnames(data_out) %>%
    dplyr::setdiff(colnames(data_original))

  cli::cli_alert_info("ORPP | Added Variable(s): {crayon::blue(crayon::bold(paste(vars_new, collapse = ', ')))}")

  if (!is.null(definition_tbl)) {

    definitions_new <- definition_tbl %>%
      dplyr::filter(column %in% vars_new)

    definitions_new

    cli::cli_bullets(c(
      " " = "indent",
      "*" = "bullet",
      ">" = "arrow",
      "v" = "success",
      "x" = "danger",
      "!" = "warning",
      "i" = "info"
    ))




  }

}
