
#' @importFrom gtsummary tbl_summary
table_one <- function(bene_df) {
  bene_df %>%
    select(-desynpuf_id, -state_code, -county_cd) %>%
    tbl_summary()
}
