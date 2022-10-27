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

tables_raw <-
  read_folder_csv_zips("data")

# Now we'll use initial_etl() to simulate an internal ETL process

tables_post_etl <- tables_raw %>%
  initial_etl()

# 2. QC raw data ---------------------------------------------------------------

## 2b Bene08 ===================================================================

expectations_bene <- function(obj) {

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

    # Is the claim from date and claim through date column(s) both dates?
    col_is_date(vars(birth_dt, death_dt),
                label = "Date columns are valid dates") %>%

    # All dates after 1900
    col_vals_gt(vars(birth_dt, death_dt),
                 value = ymd("19000101"),
                 label = "All dates after 01-01",
                 na_pass = TRUE) %>%

    # All dates before 2008
    col_vals_lt(vars(birth_dt, death_dt),
                 value = ymd("20090101"),
                 label = "All dates before 01-01-2009",
                 na_pass = TRUE)

}

bene_interroggation <- create_agent(tables_post_etl$bene08,
                                         tbl_name = "Bene",
                                         label = "Patient level table") %>%
  expectations_bene() %>%
  interrogate()

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

inpatient_interroggation <- create_agent(tables_post_etl$inpatient,
                                         tbl_name = "Inpatient",
                                         label = "Inpatient data (Post ETL)") %>%
  expectations_inpatient() %>%
  interrogate()

# 3. ORPP ----------------------------------------------------------------------

# We'll start from bene08, a table that is already one-row-per-patient

orpp_tbl <- tables_post_etl$bene08

# Now we'll add some ORPP variables from tables$inpatient and tables$prescription

orpp_tbl <- tables_post_etl$bene08 %>%
  add_orpp_inpatient(inpatient_tbl = tables_post_etl$inpatient)

# 4. QC ORPP -------------------------------------------------------------------

# ...

# 5. Cohort definition and Attrition -------------------------------------------

# 5b. Create attrition table ===================================================

attrition_table <-
  step_counter(
    orpp_tbl,
    "Doesn't have ESRD" = esrd_ind == 0,
    "65 years of age or older" = survival_years >= 65
  )

# look at the attrition table to see how many patients were removed:
attrition_table

# 5c. Apply attrition criteria to orpp tble ====================================

age_eligible_beneficiaries <- orpp_tbl %>%
  filter(
    esrd_ind == 0,
    survival_years >= 65
  )

# 6. Analyses ------------------------------------------------------------------

## 6a. Table 1 -----------------------------------------------------------------

# Look at overall patient character istics

table_one(orpp_tbl)

## 6b. Survival analysis--------------------------------------------------------

# Let's see how cancer diagnosis may affect survival after age 65. Note that we assume:
#  * patients entered the data at and had conditions diagnosed by age 65
#  * greater inpatient spending suggests more inpatient care was provided
# scoping out some analyses

coxph(Surv(survival_years, event = death_observed) ~ cncr,
      data = orpp_tbl) %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)


# action taken and reflections on pointblank results =============

# The team that provided you data did a great job, but `pointblank`
# revealed there are some things about the data that still need to be done.
#
# It looks like there are people younger than 65 years in this dataset!
# This is because (1) CMS also Medicare also covers patients who have end-stage disease,
# no matter their age and (2) there's probably something else going on.

# Now that we know these data reflect patients who are not age-eligible for Medicare,
# let's filter to the population who we intend to investigate. ESRD could be one reason for
# Medicare coverage, but there are others so we'll just filter out patients who are under
# 65 years of age at the time these data were taken.
#
# We'll start by making an attrition table to report this filtering:


