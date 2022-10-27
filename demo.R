# 0. Setup ---------------------------------------------------------------------

# Install packages (utilizing renv)
source("setup/packages.R")

# Load libraries
library(tidyverse)
library(pointblank)
library(survival)
library(gtsummary)

# Load the robustrwd package
devtools::load_all("robustrwd")

# 0a Params ====================================================================

# Turn on messaging
be_noisy()

# Turn off messaging
# be_quiet()

# 1. Read data -----------------------------------------------------------------

# Receive the delivery of data from the provider

tables_01 <-
  receive_delivery_01()

# Now we'll use etl() to simulate an internal ETL process

tables_01_etl <- tables_01 %>%
  etl_01()

# 2. QC raw data ---------------------------------------------------------------

## 2b patients ===================================================================

expectations_patients <- function(obj) {

  obj %>%

    # do the columns desynpuf_id and clm_admsn_dt exist?
    col_exists(vars(desynpuf_id, birth_dt,
                    death_dt, sex_ident_cd,
                    race_cd, cncr, diabetes),
               label = "Key columns exist") %>%

    # Are patient IDs unique?
    rows_distinct(vars(desynpuf_id),
                  label = "Table is ORPP") %>%

    # All dates after 1900
    col_vals_gt(vars(birth_dt, death_dt),
                 value = ymd("19000101"),
                 label = "No one born before 01-01-1900",
                 na_pass = TRUE) %>%

    col_vals_make_set(vars(death_observed),
                      set = c(TRUE, FALSE),
                      label = "Expect both patients with and without death info") %>%

    col_vals_make_set(vars(cncr),
                      set = c(TRUE, FALSE),
                      label = "Expect both patients with and without cancer")

}

patients_interrogation <- create_agent(tables_01_etl$patients,
                                       tbl_name = "Patients",
                                       label = "Patient level table") %>%
  expectations_patients() %>%
  interrogate()

# Print result
patients_interrogation

# See the
patients_interrogation %>%
  summarise_fail()

# Hm what's going wrong? Let's do some digging

#### Birth date --------------------------------

tables_01_etl$patients %>%
  filter(birth_dt <= ymd("19000101"))

#### Cancer --------------------------------

tables_01_etl$patients %>%
  group_by(cncr) %>%
  tally()

#### Death info ------------------------------

tables_01_etl$patients %>%
  group_by(death_observed) %>%
  tally()

# patients data
#   Problem 1: Some patients have birth dates before 1900
#   Problem 2: There are no patients with cancer
#   Problem 3: There are no patients with death data


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

inpatient_interroggation <- create_agent(tables_01_etl$inpatient,
                                         tbl_name = "Inpatient",
                                         label = "Inpatient data (Post ETL)") %>%
  expectations_inpatient() %>%
  interrogate()

inpatient_interroggation %>%
  summarise_fail()

# Hm what's going wrong? Let's do some digging

# Claim Payment Amount --------------------------------

tables_01_etl$inpatient %>%
  filter(clm_pmt_amt < 0)

# inpatient data
#   Problem 1: 204 records indicate have NEGATIVE claims over 10,000


# STOP - Get the new delivery

tables_02_post_etl <-
  receive_delivery_02() %>%
  etl_01()

# Test patients ---------------------------------------------------------------

patients_interrogation <- create_agent(tables_02_post_etl$patients,
                                       tbl_name = "Patients",
                                       label = "Patient level table") %>%
  expectations_patients() %>%
  interrogate()

# Print result
patients_interrogation

# See the
patients_interrogation %>%
  summarise_fail()

## Comment: Looks great!
## While there is much more checking we could implement, let's move on to the next step

# 3. ORPP ----------------------------------------------------------------------

# Now that'
# We'll start from patients, a table that is already one-row-per-patient

orpp_tbl <- tables_02_post_etl$patients

# Now we'll add some ORPP variables from tables$inpatient and tables$prescription
# To do this, we need to summarise MRPP (multiple-row-per-patient) tables to be
# ORPP. We then need to join then based on a unique ID, and potentially replace
# values.

# We could do this with dozens of lines of bespoke dplyr code. But instead, we like to
# take standardized ORPP routines, and embed them in add_orpp() functions.
# You can find the functions in robustrwd/r/orpp.

# Let's create a ORPP table by taking ...
#
#  1. Patients (alredy ORPP)
#  2. inpatient data, summarised with add_orpp_inpatient()
#  3. prescription data, summarised with add_orpp_prescription()

orpp_tbl <- tables_02_post_etl$patients %>%
  add_orpp_inpatient(inpatient_tbl = tables_02_post_etl$inpatient) %>%
  add_orpp_prescription(prescription_tbl = tables_02_post_etl$prescription)

# 4. QC ORPP -------------------------------------------------------------------

expectations_orpp <- function(obj) {

  obj %>%

    # Is the claim payment amount greater than 0?
    col_vals_lte(vars(prescription_n),
                 value = 1000,
                 label = "No patient should have more than 1000 prescriptions") %>%

    # Are patient IDs unique?
    rows_distinct(vars(desynpuf_id),
                  label = "Table is ORPP") %>%

    # Are patient IDs unique?
    col_vals_gt(vars(survival_years), value = 0,
                  label = "No patient has negative survival time")


}


orpp_interrogation <- create_agent(orpp_tbl,
                                   tbl_name = "ORPP cohort",
                                   label = "Patient level table") %>%
  expectations_orpp() %>%
  interrogate()

# Print result
orpp_interrogation

# See the
orpp_interrogation %>%
  summarise_fail()



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

