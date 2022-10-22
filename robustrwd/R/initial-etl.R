
initial_etl <- etl_factory(initial_etl_bene, initial_etl_inpatient)

factor_as_string <- compose(as.character, factor)

#' Do initial ETL on the beneficiaries table
#' 
#' This function will: 
#'   * transform conditions to TRUE/FALSE instead of 1/2
#'   * transform sex/race/state codes to strings
#'   * calculate age in years
#'   
#' Ideally, we would specify levels/labels elsewhere so they're more easily
#' modified
initial_etl_bene <- function(bene_df) {
  bene_df %>% 
    mutate_at(
      c("SP_ALZHDMTA", "SP_CHF", "SP_CHRNKIDN", "SP_CNCR", 
        "SP_COPD", "SP_DEPRESSN", "SP_DIABETES", "SP_ISCHMCHT", "SP_OSTEOPRS", 
        "SP_RA_OA", "SP_STRKETIA"),
      ~ .x == 1
    ) %>% 
    mutate_at(
      "BENE_SEX_IDENT_CD", 
      ~ factor_as_string(.x, levels = 1:2, labels = c("Male", "Female"))
      ) %>% 
    mutate_at(
      "BENE_RACE_CD", 
      ~ factor_as_string(
        .x, 
        levels = c(1:2, 3, 5), 
        labels = c("White", "Black", "Other", "Hispanic")
      )
    ) %>% 
    mutate_at(
      "SP_STATE_CODE", 
      ~ factor_as_string(
        .x,
        levels = 
          c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
          "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", 
          "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
          "33", "34", "35", "36", "37", "38", "39", "41", "42", "43", "44", 
          "45", "46", "47", "49", "50", "51", "52", "53", "54"),
        labels = 
          c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", 
          "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
          "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
          "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", 
          "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "Others")
      )
    )
}

initial_etl_inpatient <- function(inpatient_df) {
  # maybe do some ETL on the inpatient data.frame
  inpatient_df
}

