
definition_tbl <- tibble::tribble(
  ~sources, ~column, ~definition,

  # Inpatient derivations ------------------------------------------------------

  "inpatient","has_inpatient_claims",      "Flag if patient has any inpatient claims",
  "inpatient","inpatient_claims_n",        "Number of inpatient claims",
  "inpatient", "inpatient_payment_median", "Median dollar amount of inpatient claims",
  "inpatient", "inpatient_payment_min",    "Minimum dollar amount of inpatient claims",
  "inpatient","inpatient_payment_max",     "Maximum dollar amount of inpatient claims",
  "inpatient","claims_days_median",        "Median number of number of covered days of care that are chargeable to Medicare facility",

  # Prescription derivations ------------------------------------------------------

  "prescription", "has_prescription",      "Flag if patient has any prescriptions",
  "prescription", "prescription_n",        "Number of prescription events",
  "prescription","prescription_patient_payment_median", "Median patient prescription payment amount",
  "prescription", "prescription_rx_cost_median", "Median prescription total cost amount",
  "prescription","prescription_patient_payment_percentage_median", "Median percentage of total prescription cost paid by patient"

)

options("definition_tbl" = definition_tbl)
