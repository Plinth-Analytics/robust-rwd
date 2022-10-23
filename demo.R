
# TODO: revise lockfile to only what's needed

# This script will make sure we have the right packages installed.
# You may see a warning if your version of R is different--this will
# probably not make a difference to your findings, but if anything is
# particularly concerning then consider look here to revise
source("setup/packages.R")

# Note:
# * It's unfortunately necessary to download files before reading them
#   rather than reading from the url since they're zips, remotely reading
#   which isn't supported
# * This script is not lazy--it will download the data regardless of whether
#   any files are present, and will overwrite existing files
if (!dir.exists("data")) source("setup/download.R")

library(tidyverse)
library(pointblank)
library(survival)
devtools::load_all("robustrwd")

# read data ==================

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

first_cancer_fit <-
  coxph(Surv(survival_years - 65, event = death_observed) ~ cncr, data = bene) %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

# Pointblank - first run =================

# It's great that we have results. Still, let's run pointblank to be sure they come from
# acceptable data...we'll start by making sure the data align with the codebook

# see codebook at https://www.cms.gov/files/document/de-10-codebook.pdf-0
# These pointblank agent will address some points from the codebook

#  * There are 2,326,856 valid values of DESYNPUF_ID
#  * ...
#  * Also, since we need patients to be age-eligible for Medicare,
#    let's make sure everyone is 65 or older

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
    "Over 65 years of age" = survival_years >= 65
    )

# look at the attrition table to see how many patients were removed:
attrition_table

age_eligible_beneficiaries <- filter(bene, esrd_ind == 0, survival_years >= 65)

# pointblank on updated data =======================

# let's re-run `pointblank` on the updated data




# Analysis on updated data ==========================

# Now that we've done ETL with pointblank in mind, let's do the same analysis again

pointblanked_cancer_fit <-
  coxph(Surv(survival_years - 65, event = death_observed) ~ cncr, data = age_eligible_beneficiaries) %>%
  broom::tidy(exp = TRUE, conf.int = TRUE)

# Interesting, our findings are pretty different!

# exercises for the reader

# [Try this on your own] Pretend we were asked to use more data, from the years
# 2009 and 2010. Can you apply `pointblank` to these data? What do you see?
