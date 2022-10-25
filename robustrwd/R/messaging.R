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
                             data_out) {
  vars_new <- colnames(data_out) %>%
    dplyr::setdiff(colnames(data_original))

  cli::cli_alert_info("Added Variables: {crayon::blue(crayon::bold(paste(vars_new, collapse = ', ')))}")
}
