# 0. Setup ---------------------------------------------------------------------

# Install packages (utilizing renv)
source("setup/packages.R")

# Load libraries
library(tidyverse)
library(pointblank)
library(survival)
library(gtsummary)
library(survminer)

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

tables_01_etl_01 <- tables_01 %>%
  etl_01()

# 2. QC raw data ---------------------------------------------------------------

## 2b patients ===================================================================

patients_interrogation <- create_agent(tables_01_etl_01$patients,
  tbl_name = "Patients",
  label = "Patient level table"
) %>%
  expectations_patients() %>%
  interrogate()

# Print result
patients_interrogation

# See the
patients_interrogation %>%
  summarise_fail()

# Hm what's going wrong? Let's do some digging

#### Birth date --------------------------------

tables_01_etl_01$patients %>%
  filter(birth_dt <= ymd("19000101"))

#### Cancer --------------------------------

tables_01_etl_01$patients %>%
  group_by(cncr) %>%
  tally()

#### Death info ------------------------------

tables_01_etl_01$patients %>%
  group_by(death_observed) %>%
  tally()

#### Sex info ------------------------------

tables_01_etl_01$patients %>%
  group_by(sex_ident_cd) %>%
  tally()

# patients data
#   Problem 1: Some patients have birth dates before 1900
#   Problem 2: There are no patients with cancer
#   Problem 3: There are no patients with death data
#   Problem 4: All patients are Male

## 2c Inpatient ================================================================

inpatient_interroggation <- tables_01_etl_01$inpatient %>%
  create_agent(
    tbl_name = "Inpatient",
    label = "Inpatient data (Post ETL)"
  ) %>%
  expectations_inpatient() %>%
  interrogate()

inpatient_interroggation %>%
  summarise_fail()

# Hm what's going wrong? Let's do some digging

# Claim Payment Amount --------------------------------

tables_01_etl_01$inpatient %>%
  filter(clm_pmt_amt < 0)

# inpatient data
#   Problem 1: 104 records indicate have NEGATIVE claims

# STOP - Get the new delivery

tables_02 <- receive_delivery_02()

tables_02_etl_01 <- tables_02 %>%
  etl_01()

# Test patients ---------------------------------------------------------------

patients_interrogation <- create_agent(tables_02_etl_01$patients,
  tbl_name = "Patients",
  label = "Patient level table"
) %>%
  expectations_patients() %>%
  interrogate()

# Print result
patients_interrogation

# See the
patients_interrogation %>%
  summarise_fail()

## Something is still wrong! We still  don't have both Males and Females
# The provider assures us that the data are correct. Values are sent as 1s and 2s
# and they KNOW both are in our file.

# Wait...1s and 2s? What about the "Male" and "Female"

# Look at 'raw' patients data before ETL. I see both 1s and 2s!
tables_02$patients %>%
  select(BENE_SEX_IDENT_CD)

# Now look after ETL. I only see "Male"!
tables_02_etl_01$patients %>%
  select(sex_ident_cd)

# Oh no our ETL is wrong!
# If you look at the robustrwd/R/etl_01.R script you'll see the offending lines

# Our eng team has created a new ETL process called etl_02. Let's run it on our data

tables_02_etl_02 <- tables_02 %>%
  etl_02()

# Run on the patients data
tables_02_etl_02$patients %>%
  create_agent(
    tbl_name = "Patients",
    label = "Patients data (Post ETL 02)"
  ) %>%
  expectations_patients() %>%
  interrogate() %>%
  summarise_fail()

# Results look good!
# Now let's try on the inpatient data

tables_02_etl_02$inpatient %>%
  create_agent(
    tbl_name = "Inpatient",
    label = "Inpatient data (Post ETL 02)"
  ) %>%
  expectations_inpatient() %>%
  interrogate() %>%
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

orpp_tbl <- tables_02_etl_02$patients %>%
  add_orpp_inpatient(inpatient_tbl = tables_02_etl_02$inpatient) %>%
  add_orpp_prescription(prescription_tbl = tables_02_etl_02$prescription)

# Print to the console
orpp_tbl

# 4. QC ORPP -------------------------------------------------------------------

orpp_interrogation <- create_agent(orpp_tbl,
  tbl_name = "ORPP cohort",
  label = "Patient level table"
) %>%
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

## 6a. Survival analysis--------------------------------------------------------

# Let's see how cancer diagnosis may affect survival after age 65. Note that we assume:
#  * patients entered the data at and had conditions diagnosed by age 65
#  * greater inpatient spending suggests more inpatient care was provided
# scoping out some analyses

fit <- survfit(Surv(survival_years, death_observed) ~ race_cd,
  data = cohort_tbl
)

ggsurvplot(fit, conf.int = TRUE, surv.median.line = "hv")
