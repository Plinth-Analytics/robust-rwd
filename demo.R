# 0. Setup ---------------------------------------------------------------------

# Install packages (utilizing renv)
source("setup/packages.R")

# Download data (may not be needed now that files are committed to the repo)
if (!dir.exists("data")) source("setup/download.R")

# Load libraries
library(tidyverse)
library(pointblank)
library(survival)

# Load the robustrwd package
devtools::load_all("robustrwd")

# 0a Params ====================================================================

# Turn on messaging
be_noisy()

# Turn off messaging
# be_quiet()

# 1. Read data -----------------------------------------------------------------

# Use `read_folder_csv_zips()` to read in the raw data as it was delivered from
#  the source

tables_02 <-
  receive_delivery_02()

# Now we'll use etl() to simulate an internal ETL process

tables_02_etl <- tables_02 %>%
  etl_02()

# 2. QC raw data ---------------------------------------------------------------

## 2b patients ===================================================================

expectations_patients <- function(obj) {

  obj %>%

    # do the columns desynpuf_id and clm_admsn_dt exist?
    col_exists(vars(desynpuf_id, birth_dt, death_dt, sex_ident_cd, race_cd, cncr, diabetes),
               label = "Key columns exist") %>%

    # Are claim IDs all unique?
    rows_distinct(vars(desynpuf_id),
                  label = "Table is ORPP") %>%

    # Is the claim from date and claim through date column(s) both dates?
    col_is_date(vars(birth_dt, death_dt),
                label = "Date columns are valid dates") %>%

    # All dates after 1900
    col_vals_gt(vars(birth_dt, death_dt),
                 value = ymd("19000101"),
                 label = "No one born before 01-01-1900",
                 na_pass = TRUE) %>%

    col_vals_make_set(vars(cncr),
                      set = c(TRUE, FALSE),
                      label = "Expect both patients with and without cancer")

}

patients_interrogation <- create_agent(tables_02_etl$patients,
                                       tbl_name = "Patients",
                                       label = "Patient level table") %>%
  expectations_patients() %>%
  interrogate()

patients_interrogation %>%
  summarise_fail()

## 2c Inpatient ================================================================

expectations_inpatient <- function(obj) {

  obj %>%

    # do the columns desynpuf_id and clm_admsn_dt exist?
    col_exists(vars(desynpuf_id),
               label = "Key columns exist") %>%

    # Is the claim from date and claim through date column(s) both dates?
    col_is_date(vars(clm_from_dt, clm_thru_dt),
                label = "Claim admin is a date") %>%

    # Is the claim payment amount greater than 0?
    col_vals_gte(vars(clm_pmt_amt),
                 value = 0,
                 label = "Claim payment amount is positive")

}


inpatient_interroggation <- create_agent(tables_02_etl$inpatient,
                                         tbl_name = "Inpatient",
                                         label = "Inpatient data (Post ETL)") %>%
  expectations_inpatient() %>%
  interrogate()

inpatient_interroggation %>%
  summarise_fail()

# 3. ORPP ----------------------------------------------------------------------

# We'll start from patients, a table that is already one-row-per-patient

orpp_tbl <- tables_02_etl$patients

# Now we'll add some ORPP variables from tables$inpatient and tables$prescription

orpp_tbl <- tables_02_etl$patients %>%
  add_orpp_inpatient(inpatient_tbl = tables_02_etl$inpatient) %>%
  add_orpp_prescription(prescription_tbl = tables_02_etl$prescription)

# 4. QC ORPP -------------------------------------------------------------------

# ...

# 5. Cohort definition and Attrition -------------------------------------------

# 5b. Create attrition table ===================================================

attrition_table <-
  step_counter(
    orpp_tbl,
    "Race is white or black" = race_cd %in% c("White", "Black"),
    "Has Diabetes" = diabetes == TRUE,
    "Has at least 1000 inpatient costs" = inpatient_payment_median >= 10000,
    "Median prescription cost is > 50" = prescription_rx_cost_median > 20,
    "18 years of age or older" = survival_years >= 18
  )

# look at the attrition table to see how many patients were removed:
attrition_table

# 5c. Apply attrition criteria to orpp tble ====================================

cohort_tbl <- orpp_tbl %>%
  filter(
    race_cd %in% c("White", "Black"),
    diabetes == TRUE,
    inpatient_payment_median >= 10000,
    prescription_rx_cost_median > 20,
    survival_years >= 18
  )

# 6. Analyses ------------------------------------------------------------------

## 6a. Table 1 -----------------------------------------------------------------

# Look at overall patient character istics

table_one(cohort_tbl)

## 6b. Survival analysis--------------------------------------------------------

# Let's see how cancer diagnosis may affect survival after age 65. Note that we assume:
#  * patients entered the data at and had conditions diagnosed by age 65
#  * greater inpatient spending suggests more inpatient care was provided
# scoping out some analyses

fit <- survfit(Surv(survival_years, death_observed) ~ race_cd,
               data = cohort_tbl)

ggsurvplot(fit, conf.int = TRUE, surv.median.line = "hv")


