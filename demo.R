
# This script will make sure we have the right packages installed.
# You may see a warning if your version of R is different--this will
# probably not make a difference to your findings, but if anything is
# particularly concerning then consider look here to revise
source("setup/packages.R")

# Note:
# * Since they're zips, it's unfortunately necessary to download files
#   before reading them rather than reading from the url--remotely reading
#   isn't supported
# * This script is not lazy--it will download the data regardless of whether
#   any files are present, and will overwrite existing files
# * We only check if the "data" folder exists before deciding whether to
#   download
if (!dir.exists("data")) source("setup/download.R")

library(tidyverse)
library(pointblank)
library(survival)
devtools::load_all("robustrwd")

# read data ==================

# Alternatively, we can make it such that the table is only read in at
# interrogation-time. This is useful in situations where we might deploy an
# agent from a YAML file. (https://rich-iannone.github.io/pointblank/articles/VALID-I.html)
tables <-
  read_folder_csv_zips("data") %>%
  # the team that provides you data will probably have done
  # some ETL on it. We'll call this the "initial" ETL
  # see robustrwd/R/initial-etl.R for this ETL
  initial_etl()

bene <- tables$bene08

# side note:
# do we need more years to provide important insights? start with something like:
# bene <- bind_rows(tables[grep("bene", names(tables))])
# Just make sure your ETL works as-intended!

# Analysis (initial) ======================

# Let's see how cancer diagnosis may affect survival after age 65. Note that we assume:
#  * patients entered the data at and had conditions diagnosed by age 65
#  * greater inpatient spending suggests more inpatient care was provided
# scoping out some analyses

coxph(Surv(survival_years, event = death_observed) ~ cncr, data = bene) %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

# Pointblank - first run =================

# It's great that we have results. Still, let's run pointblank to be sure they come from
# acceptable data...we'll start by making sure the data align with the codebook

# This pointblank agent will address
#   * some expectations from the codebook (https://www.cms.gov/files/document/de-10-codebook.pdf-0)
#   * other expectations we asked the data delivery team to address
# We'll make this a function so we can run it again
assess_expectations <- function(tbl) {
  tbl %>%
    # all the rows should be distinct--this is a patient-level table
    rows_distinct() %>%
    col_is_posix(vars(birth_dt, death_dt)) %>%
    # There should be fewer than 2,326,856 unique values of DESYNPUF_ID
    # NOTE FOR NATHANIEL: Is there a function to be sure of this? or do we need a custom expression?
    col_vals_expr(expr(length(unique(desynpuf_id)) <= 2326856), brief = "Plausible number of unique IDs") %>%
    # Some columns should be logical
    col_is_logical(vars(
      alzhdmta, chf, chrnkidn, cncr, copd, depressn,
      diabetes, ischmcht, osteoprs, ra_oa, strketia
    )) %>%
    #  Since we need patients to be age-eligible for Medicare,
    #  let's make sure everyone survived to 65 or older (but not implausibly old)
    col_vals_between(vars(survival_years), left = 65, right = 120, brief = "Age between 65 and 120") %>%
    # a few variables should be in a particular set of values
    col_vals_in_set("sex_ident_cd", set = c("Male", "Female")) %>%
    col_vals_in_set("race_cd", set = c("White", "Black", "Other", "Hispanic")) %>%
    col_vals_in_set(
      "state_code",
      set = c(
        "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID",
        "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
        "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA",
        "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
        "Others"
      )
    )
}

assess_expectations(bene)

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

attrition_table <-
  step_counter(
    bene,
    "Doesn't have ESRD" = esrd_ind == 0,
    "65 years of age or older" = survival_years >= 65
    )

# look at the attrition table to see how many patients were removed:
attrition_table

age_eligible_beneficiaries <- filter(bene, esrd_ind == 0, survival_years >= 65)

# pointblank on updated data =======================

# let's re-run `pointblank` on the updated data

assess_expectations(age_eligible_beneficiaries)

# Analysis on updated data ==========================

# Now that we've done ETL with pointblank in mind, let's do the same analysis again

coxph(Surv(survival_years, event = death_observed) ~ cncr, data = age_eligible_beneficiaries) %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

# Interesting, our findings are pretty different!

# exercises for the reader

# [Try this on your own] Pretend we were asked to use more data, from the years
# 2009 and 2010. Can you apply `pointblank` to these data? What do you see?
