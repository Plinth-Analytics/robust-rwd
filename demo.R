# Goal -------------------------------------------------------------------------

# 1. Read in raw data
# 2. Apply QC checks on the raw data
#      Discover issues with delivered data and an internal ETL pipeline
# 3. Generate a one-row-per-patient (ORPP) table from multiple tables
# 4. Conduct QC checks on the ORPP table
# 5. Define a cohort based on multi-dimensional criteria
# 6. Create an attrition table
# 7. Conducted a survival analysis
# 8. Created a survival plot

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

# Turn on messaging
be_noisy()

# Turn off messaging
# be_quiet()

# 1. Read data -----------------------------------------------------------------

# Receive the delivery of data from the provider

tables_01 <-
  receive_delivery_01()

# Now we'll use etl_01() to simulate an internal ETL process

tables_01_etl_01 <- tables_01 %>%
  etl_01()

# 2. QC raw data ---------------------------------------------------------------

## 2b patients ===================================================================

# Let's define some expectations for tha patient table:
edit(expectations_patients)

# And apply them to the patients data
patients_interrogation <- create_agent(tables_01_etl_01$patients,
                                       tbl_name = "Patients",
                                       label = "Patient level table") %>%
  expectations_patients() %>%
  interrogate()

# Print  interrogation summary
patients_interrogation

# See the tidy interrogation results
patients_interrogation %>%
  tidy_interrogation()

# Hm what's going wrong? Let's do some digging

#### Birth date --------------------------------

##### Patients borth before 1900
tables_01_etl_01$patients %>%
  filter(birth_dt <= ymd("19000101"))

#### Cancer --------------------------------

##### No patients have cancer
tables_01_etl_01$patients %>%
  group_by(cncr) %>%
  tally()

#### Death info ------------------------------

##### No patients have recorded deaths
tables_01_etl_01$patients %>%
  group_by(death_observed) %>%
  tally()

#### Sex info ------------------------------

##### All patients are Male
tables_01_etl_01$patients %>%
  group_by(sex_ident_cd) %>%
  tally()

# patients data
#   Problem 1: Some patients have birth dates before 1900
#   Problem 2: There are no patients with cancer
#   Problem 3: There are no patients with death data
#   Problem 4: All patients are Male

## 2c Inpatient ================================================================

edit(expectations_inpatient)

inpatient_interroggation <- tables_01_etl_01$inpatient %>%
  create_agent(
    tbl_name = "Inpatient",
    label = "Inpatient data (Post ETL)"
  ) %>%
  expectations_inpatient() %>%
  interrogate()

inpatient_interroggation %>%
  tidy_interrogation()

# Hm what's going wrong? Let's do some digging

### Claim Payment Amount --------------------------------

tables_01_etl_01$inpatient %>%
  filter(clm_pmt_amt < 0)

# inpatient data
#   Problem 1: 104 records indicate have NEGATIVE claims

# STOP - Get the new delivery from the provider using receive_delivery_02()

tables_02 <- receive_delivery_02()

# Apply our internal etl 01 process
tables_02_etl_01 <- tables_02 %>%
  etl_01()

# Test patients ---------------------------------------------------------------

patients_interrogation <- create_agent(tables_02_etl_01$patients,
  tbl_name = "Patients",
  label = "Patient level table"
) %>%
  expectations_patients() %>%
  interrogate()

# See the
patients_interrogation %>%
  tidy_interrogation()

## The new data seems to have been a big improvement, but there is still something
# wrong! We still seem to only have male patients!
# The provider assures us that the data are correct. Values are sent as 1s and 2s
# and they KNOW both are in our file.
#


# Wait...1s and 2s? What about the "Male" and "Female"

# Look at 'raw' patients data before ETL. I see both 1s and 2s!
tables_02$patients %>%
  select(BENE_SEX_IDENT_CD)

# Now look after ETL. I only see "Male"!
tables_02_etl_01$patients %>%
  select(sex_ident_cd)

# Oh no our ETL is wrong!
# We can see bug in the etl_patients_01() function in the robustrwd/R/etl_01.R script
edit(etl_patients_01)

# Our eng team has created a new ETL process called etl_02.
edit(etl_patients_02)

#Let's run it on our data

tables_02_etl_02 <- tables_02 %>%
  etl_02()

# Run on the patients data
patients_interrogation <- tables_02_etl_02$patients %>%
  create_agent(
    tbl_name = "Patients",
    label = "Patients data (Post ETL 02)"
  ) %>%
  expectations_patients() %>%
  interrogate()

# Look at tidy results
patients_interrogation %>%
  tidy_interrogation()

# Results look good!
# Now let's try on the inpatient data

inpatient_interrogation <- tables_02_etl_02$inpatient %>%
  create_agent(
    tbl_name = "Inpatient",
    label = "Inpatient data (Post ETL 02)"
  ) %>%
  expectations_inpatient() %>%
  interrogate()

# Look at tidy results
inpatient_interrogation %>%
  tidy_interrogation()

## Ok now we're good!
## Data Delivery 02 fixed several issues from the data provider
## ETL 02 fixed a coding issue with Sex from our data engineering team

# 3. ORPP ----------------------------------------------------------------------

# Now that'
# We'll start from patients, a table that is already one-row-per-patient

orpp_tbl <- tables_02_etl_02$patients

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

## Now let's QC the orpp table. We'll use the existing set of
#  expectations expectations_orpp()

edit(expectations_orpp)

orpp_interrogation <- create_agent(orpp_tbl,
  tbl_name = "ORPP cohort",
  label = "Patient level table"
) %>%
  expectations_orpp() %>%
  # expectations_patients() %>%   # Can also add our existing patients expectations!
  interrogate()

# Print result
orpp_interrogation %>%
  tidy_interrogation()

# 5. Cohort definition and Attrition -------------------------------------------

# Define cohort as patients who have...
# Aace of Black or white
# Diabetes
# Median inpatient claim amount of at least 10,000
# Median cost of prescriptions must be at least $20
# At least 18 years of known survival

# 5b. Create attrition table ===================================================

attrition_table <-
  create_attrition(
    orpp_tbl,
    "Race is white or black" = race_cd %in% c("White", "Black"),
    "Has Diabetes" = diabetes == TRUE,
    "Has at least 1000 inpatient costs" = inpatient_payment_median >= 10000,
    "Median prescription cost is > 50" = prescription_rx_cost_median > 20,
    "18 years of age or older" = survival_years >= 18
  )

# Show the attrition table
attrition_table

# Plot an attrition chart
attrition_table %>%
  plot_attrition()

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

# Are there disparities between Black and White patients in our final cohort?
# We will answer this with a survival analysis

fit <- survfit(Surv(survival_years, death_observed) ~ race_cd,
  data = cohort_tbl
)

# Create the final Kaplan-Meier curve
ggsurvplot(fit,
           conf.int = TRUE,
           surv.median.line = "hv") +
  labs(title = "Survival of adult male patients from birth to death",
       subtitle = "Simulated Medicare data")

# Cox
model <- coxph( Surv(survival_years, death_observed) ~ race_cd + sex_ident_cd,
                data = cohort_tbl )
ggforest(model)

# Finished! -------------------------------------------------------------------

# Here's what we did

# Read in raw data
# Applied QC checks on the raw data
#   Discovered issues with delivered data and an internal ETL pipeline
# Generated a one-row-per-patient (ORPP) table from multiple tables
# Conducted QC checks on the ORPP table
# Defined a cohort based on multi-dimensional criteria
# Created an attrition table
# Conducted a survival analysis
# Created a survival plot






# Final thoughts --------------------------------------------------------------

# What could have gone wrong:

bad_orpp_tbl <- tables_02_etl_01$patients %>%
  add_orpp_inpatient(inpatient_tbl = tables_02_etl_01$inpatient) %>%
  add_orpp_prescription(prescription_tbl = tables_02_etl_01$prescription)

bad_cohort_tbl <- bad_orpp_tbl %>%
  filter(
    race_cd %in% c("White", "Black"),
    diabetes == TRUE,
    inpatient_payment_median >= 10000,
    prescription_rx_cost_median > 20,
    survival_years >= 18
  )

bad_fit <- survfit(Surv(survival_years, death_observed) ~ race_cd,
               data = bad_cohort_tbl
)

# Create the final Kaplan-Meier curve
ggsurvplot(bad_fit,
           conf.int = TRUE,
           surv.median.line = "hv", risk.table = TRUE) +
  labs(title = "Survival of adult male patients from birth to death using bad data",
       subtitle = "Simulated Medicare data")


# Cox Model
bad_model <- coxph( Surv(survival_years, death_observed) ~ race_cd,
                data = bad_cohort_tbl)
ggforest(bad_model)
