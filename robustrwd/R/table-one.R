
#' @importFrom gtsummary tbl_summary
table_one <- function(bene_df) {
  bene_df %>%
    select(-any_of(c("desynpuf_id", "county_cd"))) %>%
    tbl_summary()
}
