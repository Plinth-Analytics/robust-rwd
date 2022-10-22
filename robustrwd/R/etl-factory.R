
# consider: 
#  * taking a named list of arguments `...`  
#     whose names will be used to identify tables to transform
#  * making sure each of `which_tables` exists
etl_factory <- function(bene_etl, inpatient_etl) {
  function(tables) {
    # could map over list of functions but this may be easier to review
    which_tables <- 
      map(set_names(c("bene", "inpatient")), ~ grep(.x, names(tables), value = TRUE))

    tables[which_tables$bene] <- map(tables[which_tables$bene], bene_etl)

    tables[which_tables$inpatient] <- map(tables[which_tables$inpatient], inpatient_etl)

    tables
  }
}
