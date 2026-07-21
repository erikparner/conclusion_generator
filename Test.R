library(scales)
library(dplyr)
source("Conclusion_function.R")

conclusion_statistical(
  estimate=0.79,
  ci_lower=0.61,
  ci_upper=1.02,
  superiority= 0.96,
  noninferiority=1.02,
  measure = "RR",
  direction_beneficial="small",
  decimals=2,
  type="short"
  )

conclusion_statistical(
  estimate=0.79,
  ci_lower=0.61,
  ci_upper=1.02,
  superiority= 0.96,
  noninferiority=1.02,
  measure = "RR",
  direction_beneficial="small",
  decimals=2,
  type="long"
  )

conclusion_clinical(
  estimate=0.79,
  estimate_importance="Important benefit",
  ci_lower=0.61,
  ci_lower_importance="Important benefit",
  ci_upper=1.02,
  ci_upper_importance="Too small to be important harm",
  decimals=2
  )                 
