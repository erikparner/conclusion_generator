# library(scales)

# Format a relative measure as percent increase or decrease. 
# decimals: Number of decimals that is specified for the relative measure. 
# Comments.
# A RR=1 is treated as "0% reduction".
relative_to_percent <- function(relative, decimals=0, interpret_measure="", interpret_direction=TRUE) {
  if(relative<=1) {
    if(interpret_direction) {
      posttext <- "reduction"
    } else {
      posttext <- ""
    }
    text <- paste0(percent(1-relative, accuracy=10^(-(decimals))),
                   " ",
                   interpret_measure, 
                   posttext
    )
  }
  if(relative>1) {
    if(interpret_direction) {
      posttext <- "increase"
    } else {
      posttext <- ""
    }
    text <- paste0(percent(relative-1, accuracy=10^(-(decimals))), 
                   " ",
                   interpret_measure, 
                   posttext
    )
  }
  text
}

# Format a risk difference (RD) to percent difference.
# decimals: Number of decimals that is specified for the risk difference. 
# Comments.
# A RD=0 is treated as "0% reduction".
rd_to_percent <- function(rd, decimals=0, interpret_measure="", interpret_direction=TRUE) {
  if(rd<=0) {
    if(interpret_direction) {
      posttext <- "reduction"
    } else {
      posttext <- ""
    }
    text <- paste0(percent(-rd, accuracy=10^(-(decimals))),
                   " point ",
                   interpret_measure, 
                   posttext
    )
  }
  if(rd>0) {
    if(interpret_direction) {
      posttext <- "increase"
    } else {
      posttext <- ""
    }
    text <- paste0(percent(rd, accuracy=10^(-(decimals))), 
                   " point ",
                   interpret_measure, 
                   posttext
    )
  }
  text
}

# Format a mean difference (MD) as a text description.
md_to_text <- function(md, decimals=2, units="", interpret_measure="", interpret_direction=TRUE) {
  units_str <- if(nchar(units) > 0) paste0(units, " ") else ""
  if(md <= 0) {
    if(interpret_direction) {
      posttext <- "reduction"
    } else {
      posttext <- ""
    }
    text <- paste0(number(abs(md), accuracy=10^(-decimals)),
                   " ",
                   units_str,
                   interpret_measure,
                   posttext)
  }
  if(md > 0) {
    if(interpret_direction) {
      posttext <- "increase"
    } else {
      posttext <- ""
    }
    text <- paste0(number(md, accuracy=10^(-decimals)),
                   " ",
                   units_str,
                   interpret_measure,
                   posttext)
  }
  text
}

# Convert a measure (RR, HR, IRR, OR, RD, or MD) to a descriptive text.
measure_to_text <- function(number, decimals=2, measure="RR", units="", interpret_measure="", interpret_direction=TRUE) {
  if(measure %in% c("RR", "HR", "IRR", "OR")) {
    text=relative_to_percent(relative=number, 
                             decimal=decimals-2, 
                             interpret_measure=interpret_measure, 
                             interpret_direction=interpret_direction)
  }
  if(measure=="RD") {
    text=rd_to_percent(rd=number, 
                       decimal=decimals-2, 
                       interpret_measure=interpret_measure, 
                       interpret_direction=interpret_direction)
  }
  if(measure=="MD") {
    text=md_to_text(md=number,
                    decimals=decimals,
                    units=units,
                    interpret_measure=interpret_measure,
                    interpret_direction=interpret_direction)
  }
  text
}

scenario_superiority_noninferiority <- function(
    ci_lower=NA,
    ci_upper=NA,
    superiority=NA,
    noninferiority=NA,
    no_effect=1,
    direction_benefical="small") {
  
  # Testing input.
  stopifnot(
    "direction_benefical must be 'small' or 'large'" =
      direction_benefical %in% c("small", "large"),
    "ci_lower, ci_upper, superiority, noninferiority, and no_effect must be non-NA" =
      !anyNA(c(ci_lower, ci_upper, superiority, noninferiority, no_effect)),
    "ci_lower must be <= ci_upper" =
      ci_lower <= ci_upper
  )
  if (direction_benefical == "small") {
    stopifnot(
      "for direction_benefical='small', thresholds must satisfy: superiority < no_effect < noninferiority" =
        superiority < no_effect && no_effect < noninferiority
    )
  } else {
    stopifnot(
      "for direction_benefical='large', thresholds must satisfy: noninferiority < no_effect < superiority" =
        noninferiority < no_effect && no_effect < superiority
    )
  }

  scenario <- case_when(
    (direction_benefical=="small" & ci_upper<superiority) | 
      (direction_benefical=="large" & superiority<ci_lower) ~ 1,
    (direction_benefical=="small" & ci_lower<superiority & superiority<=ci_upper & ci_upper<no_effect) | 
      (direction_benefical=="large" & no_effect<ci_lower & ci_lower<=superiority & superiority<ci_upper) ~ 2,
    (direction_benefical=="small" & ci_lower<superiority & no_effect<=ci_upper & ci_upper<noninferiority) | 
      (direction_benefical=="large" & noninferiority<ci_lower & ci_lower<=no_effect & superiority<ci_upper) ~ 3,
    (direction_benefical=="small" & ci_lower<superiority & noninferiority<=ci_upper) |
      (direction_benefical=="large" & ci_lower<=noninferiority & superiority<ci_upper ) ~ 4,
    (direction_benefical=="small" & superiority<=ci_lower & ci_upper<no_effect) |
      (direction_benefical=="large" & no_effect<ci_lower & ci_upper<=superiority) ~ 5,
    (direction_benefical=="small" & superiority<=ci_lower & ci_lower<=no_effect & no_effect<=ci_upper & ci_upper<noninferiority) |
      (direction_benefical=="large" & noninferiority<ci_lower & ci_lower<=no_effect & no_effect<=ci_upper & ci_upper<=superiority) ~ 6,
    (direction_benefical=="small" & superiority<=ci_lower & ci_lower<=no_effect & noninferiority<=ci_upper) |
      (direction_benefical=="large" & ci_lower<=noninferiority & no_effect<=ci_upper & ci_upper<=superiority) ~ 7,
    (direction_benefical=="small" & no_effect<ci_lower & ci_upper<noninferiority) |
      (direction_benefical=="large" & noninferiority<ci_lower & ci_upper<no_effect) ~ 8,
    (direction_benefical=="small" & no_effect<ci_lower & ci_lower<noninferiority & noninferiority<=ci_upper) |
      (direction_benefical=="large" & ci_lower<=noninferiority & noninferiority<ci_upper & ci_upper<no_effect) ~ 9,
    (direction_benefical=="small" & noninferiority<=ci_lower) | 
      (direction_benefical=="large" & ci_upper<=noninferiority) ~ 10,
    TRUE ~ 0
  )
  scenario
}

scenario_superiority <- function(
    ci_lower=NA,
    ci_upper=NA,
    superiority=NA,
    no_effect=1,
    direction_benefical="small"
    ) {
  
  # Testing input.
  stopifnot(
    "direction_benefical must be 'small' or 'large'" =
      direction_benefical %in% c("small", "large"),
    "ci_lower, ci_upper, superiority, and no_effect must be non-NA" =
      !anyNA(c(ci_lower, ci_upper, superiority, no_effect)),
    "ci_lower must be <= ci_upper" =
      ci_lower <= ci_upper
  )
  if (direction_benefical == "small") {
    stopifnot(
      "for direction_benefical='small', thresholds must satisfy: superiority < no_effect" =
        superiority < no_effect 
    )
  } else {
    stopifnot(
      "for direction_benefical='large', thresholds must satisfy: no_effect < superiority" =
        no_effect < superiority
    )
  }
  
  scenario <- case_when(
    (direction_benefical=="small" & ci_upper<superiority) | 
      (direction_benefical=="large" & superiority<ci_lower) ~ 1,
    (direction_benefical=="small" & ci_lower<superiority & superiority<=ci_upper & ci_upper<no_effect) | 
      (direction_benefical=="large" & no_effect<ci_lower & ci_lower<=superiority & superiority<ci_upper) ~ 2,
    (direction_benefical=="small" & ci_lower<superiority & no_effect<=ci_upper) |
      (direction_benefical=="large" & ci_lower<=no_effect & superiority<ci_upper ) ~ 3,
    (direction_benefical=="small" & superiority<=ci_lower & ci_upper<no_effect) |
      (direction_benefical=="large" & no_effect<ci_lower & ci_upper<=superiority) ~ 4,
    (direction_benefical=="small" & superiority<=ci_lower & ci_lower<=no_effect & no_effect<=ci_upper) |
      (direction_benefical=="large" & ci_lower<=no_effect & no_effect<=ci_upper & ci_upper<=superiority) ~ 5,
    (direction_benefical=="small" & no_effect<ci_lower) | 
      (direction_benefical=="large" & ci_upper<no_effect) ~ 6,
    TRUE ~ 0
  )
  scenario
}

scenario_noninferiority <- function(
    ci_lower=NA,
    ci_upper=NA,
    noninferiority=NA,
    no_effect=1,
    direction_benefical="small") {
  
  # Testing input.
  stopifnot(
    "direction_benefical must be 'small' or 'large'" =
      direction_benefical %in% c("small", "large"),
    "ci_lower, ci_upper, noninferiority, and no_effect must be non-NA" =
      !anyNA(c(ci_lower, ci_upper, noninferiority, no_effect)),
    "ci_lower must be <= ci_upper" =
      ci_lower <= ci_upper
  )
  if (direction_benefical == "small") {
    stopifnot(
      "for direction_benefical='small', thresholds must satisfy: no_effect < noninferiority" =
        no_effect < noninferiority
    )
  } else {
    stopifnot(
      "for direction_benefical='large', thresholds must satisfy: noninferiority < no_effect" =
        noninferiority < no_effect 
    )
  }
  
  scenario <- case_when(
    (direction_benefical=="small" & ci_upper<no_effect) | 
      (direction_benefical=="large" & no_effect<ci_lower) ~ 1,
    (direction_benefical=="small" & ci_lower<no_effect & no_effect<=ci_upper & ci_upper<noninferiority) | 
      (direction_benefical=="large" & noninferiority<ci_lower & ci_lower<=no_effect & no_effect<ci_upper) ~ 2,
    (direction_benefical=="small" & ci_lower<no_effect & noninferiority<=ci_upper) |
      (direction_benefical=="large" & ci_lower<=noninferiority & no_effect<ci_upper ) ~ 3,
    (direction_benefical=="small" & no_effect<=ci_lower & ci_upper<noninferiority) |
      (direction_benefical=="large" & noninferiority<ci_lower & ci_upper<=no_effect) ~ 4,
    (direction_benefical=="small" & no_effect<=ci_lower & ci_lower<noninferiority & noninferiority<=ci_upper) |
      (direction_benefical=="large" & ci_lower<=noninferiority & noninferiority<ci_upper & ci_upper<=no_effect) ~ 5,
    (direction_benefical=="small" & noninferiority<=ci_lower) | 
      (direction_benefical=="large" & ci_upper<=noninferiority) ~ 6,
    TRUE ~ 0
  )
  scenario
}



# The conclusion generation in Statistical Mode and concise version
# 
# estimate The estimate.
# ci_lower The lower confidence limit.
# ci_upper The upper confidence limit.
# noninferiority Noninferiority margin. Optional. 
# superiority Superiority margin. Optional. 
# measure The association measure, RR (risk ratio), RD (risk difference), 
#          HR (hazard ratio) and IRR (incidence rate ratio).
# decimals Number of decimals.
# direction_benefical The direction of a benefical effect: "small" or "lager" 
#          values of the association measure.
conclusion_statistical_short <- function(
    estimate=NA,
    ci_lower=NA,
    ci_upper=NA,
    noninferiority=NA,
    superiority=NA,
    measure="RR",
    units="",
    decimals=2,
    direction_benefical="small") {
  
  measure_type=""
  if(measure=="RR" & direction_benefical=="small") {
    measure_type="relative risk"
    no_effect=1
  }
  if(measure=="RD" & direction_benefical=="small") {
    measure_type="risk difference"
    no_effect=0
  }
  if(measure=="RR" & direction_benefical=="large") {
    measure_type="relative response rate"
    no_effect=1
  }
  if(measure=="RD" & direction_benefical=="large") {
    measure_type="response rate difference"
    no_effect=0
  }
  if(measure=="HR"|measure=="IRR") {
    measure_type="rate ratio"
    no_effect=1
  }
  if(measure=="OR") {
    measure_type="odds ratio"
    no_effect=1
  }
  if(measure=="MD") {
    measure_type="mean difference"
    no_effect=0
  }
  units_text <- if(measure=="MD" & nchar(units) > 0) paste0(" ", units) else ""
  

  # Estimate
  conclusion <- paste0("Assuming no uncontrolled biases, our results are most compatible with a ",
                       measure_type,
                       " of ",
                       number(estimate, accuracy=10^(-decimals)),
                       units_text
  )
  
  # Interval.
  conclusion <- paste0(conclusion, 
                       ", although compatible with ",
                       measure_type,
                       "s ranging ",
                       number(ci_lower, accuracy=10^(-decimals)),
                       units_text,
                       " to ",
                       number(ci_upper, accuracy=10^(-decimals)),
                       units_text,
                       ". "
  )
  
  # Both superiority and non-inferiority margins ----
  if(!is.na(superiority) & !is.na(noninferiority)) {
    scenario <- scenario_superiority_noninferiority(
      ci_lower=ci_lower,
      ci_upper=ci_upper,
      superiority=superiority,
      noninferiority=noninferiority,
      no_effect=no_effect,
      direction_benefical=direction_benefical
    )
    
    # Mention superiority and non-inferiority.
    mention_superiority_noninferiority <-  paste0("With a superiority bound of ",
                                                  number(superiority, accuracy=10^(-decimals)),
                                                  units_text,
                                                  " and a noninferiority bound of ",
                                                  number(noninferiority, accuracy=10^(-decimals)),
                                                  units_text,
                                                  ",")
    mention_superiority <-  paste0("With a superiority bound of ",
                                   number(superiority, accuracy=10^(-decimals)),
                                   units_text,
                                   ",")
    mention_noninferiority <-  paste0("With a noninferiority bound of ",
                                      number(noninferiority, accuracy=10^(-decimals)),
                                      units_text,
                                      ",")
    conclusion <- paste0(conclusion, 
                         mention_superiority_noninferiority)
    conclusion <- case_when(
      scenario==1 ~ paste0(conclusion, " these results are largely compatible with a beneficial effect."),
      scenario==2 ~ paste0(conclusion, " these results provide evidence of non-inferiority, but cannot confirm superiority of the treatment."),
      scenario==3 ~ paste0(conclusion, " these results provide evidence of non-inferiority, but cannot confirm superiority of the treatment."),
      scenario==4 ~ paste0(conclusion, " the effect of the treatment remains uncertain."),
      scenario==5 ~ paste0(conclusion, " our results provide evidence of non-inferiority, but not superiority of the treatment."),
      scenario==6 ~ paste0(conclusion, " our results provide evidence of non-inferiority, but not superiority of the treatment."),
      scenario==7 ~ paste0(conclusion, " our results provide no evidence of noninferiority or superiority, but it may be associated with an important harm."),
      scenario==8 ~ paste0(conclusion, " our results provide evidence for harm of the treatment below the non-inferiority bound, but no superiority demonstrated."),
      scenario==9 ~ paste0(conclusion, " our results provide evidence for harm of the treatment, and non-inferiority is possible, but not demonstrated. No superiority is demonstrated."),
      scenario==10 ~ paste0(conclusion, " our results provide no evidence of noninferiority or superiority, but important harm of the treatment.")
    )
  }
  
  # Only superiority margin ----
  # The division into scenarios is as superiority+non-inferiority with no_effect 
  # instead of the noninferiority margin.
  if(!is.na(superiority) & is.na(noninferiority)) {
    scenario <- scenario_superiority(
      ci_lower=ci_lower,
      ci_upper=ci_upper,
      superiority=superiority,
      no_effect=no_effect,
      direction_benefical=direction_benefical
    )
    
    # Superiority.
    mention_superiority <-  paste0("With a superiority bound of ",
                                   number(superiority, accuracy=10^(-decimals)),
                                   units_text,
                                   ",")
    
    conclusion <- paste0(conclusion, mention_superiority)
    conclusion <- case_when(
      scenario==1 ~ paste0(conclusion, " these results are largely compatible with a beneficial effect."),
      scenario==2 ~ paste0(conclusion, " the effect of the treatment remains uncertain."),
      scenario==3 ~ paste0(conclusion, " the effect of the treatment remains uncertain."),
      scenario==4 ~ paste0(conclusion, " our results provide evidence of an effect, but not superiority of the treatment."),
      scenario==5 ~ paste0(conclusion, " our results provide evidence for no important benefit of the treatment, but it may be associated with important harm."),
      scenario==6 ~ paste0(conclusion, " our results provide evidence for no benefit of the treatment, but important harm of the treatment.")
    )
  }
    
  # Only non-inferiority margin ----
  # The division into Scenarios is as with superiority+non-inferiority with 
  # no_effect instead of the superiority margin.
  if(is.na(superiority) & !is.na(noninferiority)) {
    scenario <- scenario_noninferiority(
      ci_lower=ci_lower,
      ci_upper=ci_upper,
      noninferiority=noninferiority,
      no_effect=no_effect,
      direction_benefical=direction_benefical
    )
    
    mention_noninferiority <-  paste0("With a noninferiority bound of ",
                                      number(noninferiority, accuracy=10^(-decimals)),
                                      units_text,
                                      ",")
    conclusion <- paste0(conclusion, mention_noninferiority)
    conclusion <- case_when(
      scenario==1 ~ paste0(conclusion, " the results provide evidence for non-inferiority, but also a beneficial effect."),
      scenario==2 ~ paste0(conclusion, " the results provide evidence for non-inferiority."),
      scenario==3 ~ paste0(conclusion, " the effect of the treatment remains uncertain."),
      scenario==4 ~ paste0(conclusion, " the results provide evidence for non-inferiority, but no benefit of the treatment."),
      scenario==5 ~ paste0(conclusion, " the results provide no evidence for non-inferiority, but provide evidence for no benefit of the treatment."),
      scenario==6 ~ paste0(conclusion, " the results provide no evidence for non-inferiority, but important harm of the treatment.")
    )
  }
  
  conclusion
}


# The conclusion generation in Statistical Mode and elaborate version.
# 
# estimate The estimate.
# ci_lower The lower confidence limit.
# ci_upper The upper confidence limit.
# noninferiority Noninferiority margin. Optional. 
# superiority Superiority margin. Optional. 
# measure The association measure, RR (risk ratio), RD (risk difference), 
#          HR (hazard ratio), IRR (incidence rate ratio), or MD (mean difference).
# decimals Number of decimals.
# direction_benefical The direction of a benefical effect: "small" or "lager" 
#          values of the association measure.
conclusion_statistical_long <- function(estimate=NA,
                         ci_lower=NA,
                         ci_upper=NA,
                         noninferiority=NA,
                         superiority=NA,
                         measure="RR",
                         units="",
                         decimals=2,
                         direction_benefical="small") {
  
  
  measure_type=""
  if(measure=="RR" & direction_benefical=="small"){
    measure_type="risk"
    no_effect=1
  }
  if(measure=="RD" & direction_benefical=="small") {
    measure_type="risk"
    no_effect=0
  }
  if(measure=="RR" & direction_benefical=="large") {
    measure_type="response rate"
    no_effect=1
  }
  if(measure=="RD" & direction_benefical=="large") {
    measure_type="response rate"
    no_effect=0
  }
  if(measure=="HR"|measure=="IRR") { 
    measure_type="rate"
    no_effect=1
  }
  if(measure=="OR") {
    measure_type="odds"
    no_effect=1
  }
  if(measure=="MD") {
    measure_type="mean"
    no_effect=0
  }
  # Setting the units_text to be empty text unless MD is specified.
  units_text <- if(measure=="MD" & nchar(units) > 0) paste0(" ", units) else ""
  if(direction_benefical=="small") {
    direction_superiority="a reduction"
    direction_noninferiority="an increase"
  }
  if(direction_benefical=="large") {
    direction_superiority="an increase"
    direction_noninferiority="a reduction"
  }
  
  # Estimate
  conclusion <- paste0("Assuming no uncontrolled biases, the point estimate of ",
                       number(estimate, accuracy=10^(-decimals)),
                       units_text,
                       " corresponds to a ",
                       measure_to_text(estimate, decimals=decimals, measure=measure, units=units, interpret_measure=paste0(measure_type, " ")),
                       " as the most likely effect given the data"
  )
  
  # Interval.
  conclusion <- paste0(conclusion, 
                       ", although the interval estimate is compatible with a range from a ",
                       measure_to_text(ci_lower, decimals=decimals, measure=measure, units=units),
                       " to a ",
                       measure_to_text(ci_upper, decimals=decimals, measure=measure, units=units),
                       " in ",
                       measure_type, 
                       " from treatment. "
  )

  # Both superiority and non-inferiority margins ----  
  if(!is.na(superiority) & !is.na(noninferiority)) {
    scenario <- scenario_superiority_noninferiority(
      ci_lower=ci_lower,
      ci_upper=ci_upper,
      superiority=superiority,
      noninferiority=noninferiority,
      no_effect=no_effect,
      direction_benefical=direction_benefical
    )
    
    # Mention superiority and non-inferiority.
    mention_superiority_noninferiority <-  paste0("Given that ", direction_superiority, " of ",
                                                  measure_to_text(superiority, decimals=decimals, measure=measure, units=units, interpret_direction=FALSE),
                                                  " or more would be considered clinically and economically worthwhile",
                                                  ", and ", direction_noninferiority, " of up to ",
                                                  measure_to_text(noninferiority, decimals=decimals, measure=measure, units=units, interpret_direction=FALSE),
                                                  " would be considered an acceptable risk,")
    mention_superiority <-  paste0("Given that ", direction_superiority, " of ",
                                   measure_to_text(superiority, decimals=decimals, measure=measure, units=units, interpret_direction=FALSE),
                                   " or more would be considered clinically and economically worthwhile,")
    mention_noninferiority <-  paste0("Given that ", direction_noninferiority, " of up to ",
                                      measure_to_text(noninferiority, decimals=decimals, measure=measure, units=units, interpret_direction=FALSE),
                                      " would be considered an acceptable risk,")
    
    conclusion  <- paste0(conclusion, mention_superiority_noninferiority)
    # Same as for the short version (copy-paste)
    conclusion <- case_when(
      scenario==1 ~ paste0(conclusion, " these results are largely compatible with a beneficial effect."),
      scenario==2 ~ paste0(conclusion, " these results provide evidence of non-inferiority, but cannot confirm superiority of the treatment."),
      scenario==3 ~ paste0(conclusion, " these results provide evidence of non-inferiority, but cannot confirm superiority of the treatment."),
      scenario==4 ~ paste0(conclusion, " the effect of the treatment remains uncertain."),
      scenario==5 ~ paste0(conclusion, " our results provide evidence of non-inferiority, but not superiority of the treatment."),
      scenario==6 ~ paste0(conclusion, " our results provide evidence of non-inferiority, but not superiority of the treatment."),
      scenario==7 ~ paste0(conclusion, " our results provide no evidence of noninferiority or superiority, but it may be associated with an important harm."),
      scenario==8 ~ paste0(conclusion, " our results provide evidence for harm of the treatment below the non-inferiority bound, but no superiority demonstrated."),
      scenario==9 ~ paste0(conclusion, " our results provide evidence for harm of the treatment, and non-inferiority is possible, but not demonstrated. No superiority is demonstrated."),
      scenario==10 ~ paste0(conclusion, " our results provide no evidence of noninferiority or superiority, but important harm of the treatment.")
    )
  }
  
  # Only superiority margin ----
  # The division into cases is as above with no_effect instead of the noninferiority margin.
  if(!is.na(superiority) & is.na(noninferiority)) {
    scenario <- scenario_superiority(
      ci_lower=ci_lower,
      ci_upper=ci_upper,
      superiority=superiority,
      no_effect=no_effect,
      direction_benefical=direction_benefical
    )
    
    # Superiority.
    mention_superiority <-  paste0("Given that ", direction_superiority, " of ",
                                   measure_to_text(superiority, decimals=decimals, measure=measure, units=units, interpret_direction=FALSE),
                                   " or more would be considered clinically and economically worthwhile,")
    
    conclusion  <- paste0(conclusion, mention_superiority)
    conclusion <- case_when(
      scenario==1 ~ paste0(conclusion, " our results show evidence for important benefit of the treatment."),
      scenario==2 ~ paste0(conclusion, " the effect of the treatment remains uncertain."),
      scenario==3 ~ paste0(conclusion, " the effect of the treatment remains uncertain."),
      scenario==4 ~ paste0(conclusion, " our results provide evidence for no important benefit of the treatment, but also evidence for no harm of the treatment."),
      scenario==5 ~ paste0(conclusion, " our results provide evidence for no important benefit of the treatment, but it may be associated with an important harm."),
      scenario==6 ~ paste0(conclusion, " our results provide evidence for no benefit of the treatment, but a harm of the treatment.")
    )
  }
    
  # Only non-inferiority margin ----
  if(is.na(superiority) & !is.na(noninferiority)) {
    scenario <- scenario_noninferiority(
      ci_lower=ci_lower,
      ci_upper=ci_upper,
      noninferiority=noninferiority,
      no_effect=no_effect,
      direction_benefical=direction_benefical
    )
    
    # Non-inferiority.
    mention_noninferiority <-  paste0("Given that ", direction_noninferiority, " of up to ",
                                      measure_to_text(noninferiority, decimals=decimals, measure=measure, units=units, interpret_direction=FALSE),
                                      " would be considered an acceptable risk,")
    conclusion <- paste0(conclusion, mention_noninferiority)
    conclusion <- case_when(
      scenario==1 ~ paste0(conclusion, " there is evidence for non-inferiority and our results show evidence for important benefit of the treatment."),
      scenario==2 ~ paste0(conclusion, " there is evidence for non-inferiority."),
      scenario==3 ~ paste0(conclusion, " the effect of the treatment remains uncertain."),
      scenario==4 ~ paste0(conclusion, " the results provide evidence for non-inferiority, but no benefit of the treatment."),
      scenario==5 ~ paste0(conclusion, " the results provide no evidence for non-inferiority, but provide evidence for no benefit of the treatment."),
      scenario==6 ~ paste0(conclusion, " the results provide no evidence for non-inferiority, but important harm of the treatment.")
    )
    
  }
  
  conclusion
}

# The conclusion generation in Statistical Mode
# 
# estimate The estimate.
# ci_lower The lower confidence limit.
# ci_upper The upper confidence limit.
# noninferiority Noninferiority margin. Optional. 
# superiority Superiority margin. Optional. 
# measure The association measure, RR (risk ratio), RD (risk difference), 
#          HR (hazard ratio) and IRR (incidence rate ratio).
# decimals Number of decimals.
# direction_benefical The direction of a benefical effect: "small" or "lager" 
#          values of the association measure.
# type "short" or "long" for the short or long version of the conclusion.
conclusion_statistical <- function(
    estimate=NA,
    ci_lower=NA,
    ci_upper=NA,
    noninferiority=NA,
    superiority=NA,
    measure="RR",
    units="",
    decimals=2,
    direction_benefical="small",
    type="short") {
  
  if(type=="short") {
    conclusion <- conclusion_statistical_short(
      estimate=estimate,
      ci_lower=ci_lower,
      ci_upper=ci_upper,
      noninferiority=noninferiority,
      superiority=superiority,
      measure=measure,
      units=units,
      decimals=decimals,
      direction_benefical=direction_benefical
    )
  }
  if(type=="long") {
    conclusion <- conclusion_statistical_long(
      estimate=estimate,
      ci_lower=ci_lower,
      ci_upper=ci_upper,
      noninferiority=noninferiority,
      superiority=superiority,
      measure=measure,
      units=units,
      decimals=decimals,
      direction_benefical=direction_benefical
    )
  }
  conclusion
}  
    

# The conclusion generation in Clinical Mode.
# 
# estimate The estimate.
# estimate_importance Interpretation of the estimate: "Important benefit";
#          "Too small to be important benefit"; "Too small to be important harm";
#          "Important harm".
# ci_lower The lower confidence limit.
# ci_lower_importance Interpretation of the lower confidence limit: 
#          "Important benefit"; "Too small to be important benefit"; 
#          "Too small to be important harm"; "Important harm".
# ci_upper The upper confidence limit.
# ci_upper_importance Interpretation of the upper confidence limit: 
#          "Important benefit"; "Too small to be important benefit"; 
#          "Too small to be important harm"; "Important harm".
# measure The association measure, RR (risk ratio), RD (risk difference), 
#          HR (hazard ratio) and IRR (incidence rate ratio).
# decimals Number of decimals.
# direction_benefical The direction of a benefical effect: "small" or "lager" 
#          values of the association measure.
conclusion_clinical <- function(estimate=NA,
                        estimate_importance="",
                        ci_lower=NA,
                        ci_lower_importance="",
                        ci_upper=NA,
                        ci_upper_importance="",
                        measure="RR",
                        units="",
                        decimals=2,
                        direction_benefical="small") {
  
  conclusion <- ""
  units_text <- if(measure=="MD" & nchar(units) > 0) paste0(" ", units) else ""
  
  measure_type=""
  if(measure=="RR" & direction_benefical=="small") measure_type="risk"
  if(measure=="RD" & direction_benefical=="small") measure_type="risk"
  if(measure=="RR" & direction_benefical=="large") measure_type="response"
  if(measure=="RD" & direction_benefical=="large") measure_type="response"
  if(measure=="HR"|measure=="IRR") measure_type="rate"
  if(measure=="OR") measure_type="odds"
  if(measure=="MD") measure_type="mean"
  
  # For checking consistency of input.
  importance_list <- c("Important benefit", 
                       "Too small to be important benefit", 
                       "Too small to be important harm",
                       "Important harm")
  estimate_number <- match(estimate_importance, importance_list)
  ci_lower_number <- match(ci_lower_importance, importance_list)
  ci_upper_number <- match(ci_upper_importance, importance_list)
  
  if(direction_benefical=="small") {
    if(ci_lower_importance!=estimate_importance | estimate_importance!=ci_upper_importance) {
      
      # Estimate.
      conclusion_estimate <- paste0("Assuming no uncontrolled biases, our estimated effect of ", 
                                    number(estimate, accuracy=10^(-decimals)),
                                    units_text,
                                    ", corresponding to a ",
                                    measure_to_text(estimate, 
                                                       decimals=decimals, 
                                                       measure=measure,
                                                       units=units,
                                                       interpret_measure=paste0(measure_type, " "))
                                    )
  
      if(estimate_importance=="Important benefit") {
        conclusion_estimate <- paste0(conclusion_estimate, ", supported a clinically important benefit.")
      }
      
      if(estimate_importance=="Too small to be important benefit") {
        conclusion_estimate <- paste0(conclusion_estimate, ", supported a benefit too small to be clinically important.")
      }
      
      if(estimate_importance=="Too small to be important harm") {
        conclusion_estimate <- paste0(conclusion_estimate, ", supported a harm too small to be clinically important.")
      }
      
      if(estimate_importance=="Important harm") {
        conclusion_estimate <- paste0(conclusion_estimate, ", supported a clinically important harm.")
      }
      
      # Lower limit.
      conclusion_ci_lower <- ""
      if(estimate_importance=="Important benefit" & ci_lower_importance=="Important benefit") {
        conclusion_ci_lower <- paste0(
          " The data were compatible with an even stronger beneficial effect of as much as ",
          number(ci_lower, accuracy=10^(-decimals)),
          units_text,
          ",")
        
      }
      if(estimate_importance!="Important benefit" & ci_lower_importance=="Important benefit") {
        conclusion_ci_lower <- paste0(
          " The data were compatible with a beneficial effect up to ",
          number(ci_lower, accuracy=10^(-decimals)),
          units_text,
          ",")
        
      }
      if(ci_lower_importance=="Too small to be important benefit") {
        conclusion_ci_lower <- paste0(
          " The data provided evidence against an effect larger than ",
          number(ci_lower, accuracy=10^(-decimals)),
          units_text,
          ",")
      }
      if(ci_lower_importance=="Too small to be important harm") {
        conclusion_ci_lower <- paste0(
          " The data provided strong evidence against a substantial harmful effect worse than ",
          number(ci_lower, accuracy=10^(-decimals)),
          units_text,
          ",")
      }
      if(ci_lower_importance=="Important harm") {
        # Then "ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance", 
        # see below (assuming input is consistent).
      }
      
      # Upper limit.
      conclusion_ci_upper <- ""
      if(ci_upper_importance=="Important benefit") {
        # Then "ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance", 
        # see below (assuming input is consistent).
      }
      if(ci_upper_importance=="Too small to be important benefit") {
        conclusion_ci_upper <- paste0(
          " and incompatible with an effect smaller than ",
          number(ci_upper, accuracy=10^(-decimals)),
          units_text,
          ".")
      }
      if(ci_upper_importance=="Too small to be important harm") {
        conclusion_ci_upper <- paste0(
          " and incompatible with a harmful effect substantially larger than ",
          number(ci_upper, accuracy=10^(-decimals)),
          units_text,
          ".")
      }
      if(estimate_importance=="Important harm" & ci_upper_importance=="Important harm") {
        conclusion_ci_upper <- paste0(
          " but also an even stronger harmful effect up to  ",
          number(ci_upper, accuracy=10^(-decimals)),
          units_text,
          ".")
      }
      if(estimate_importance!="Important harm" & ci_upper_importance=="Important harm") {
        conclusion_ci_upper <- paste0(
          " but also a strong harmful effect up to ",
          number(ci_upper, accuracy=10^(-decimals)),
          units_text,
          ".")
      }
      
      # Create conclusion.
      conclusion <- paste0(conclusion_estimate, 
                           conclusion_ci_lower,
                           conclusion_ci_upper)
    }
  
    # Same interpretation of estimate and confidence limits.
    if(ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance) {
      if(estimate_importance=="Important benefit") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)),
                                      units_text,
                                      ", together with evidence for a beneficial effect between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      units_text,
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      units_text,
                                      ", supported a clinically important benefit.")
      }
      if(estimate_importance=="Too small to be important benefit") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)),
                                      units_text,
                                      ", together with evidence for an effect too small to be clinically important between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      units_text,
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      units_text,
                                      ", supported a benefit too small to be clinically important.")
      }
      if(estimate_importance=="Too small to be important harm") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)),
                                      units_text,
                                      ", together with evidence for a harmful effect too small to be important between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      units_text,
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      units_text,
                                      ", supported a harm too small to be clinically important.")
      }
      if(estimate_importance=="Important harm") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)),
                                      units_text,
                                      ", together with evidence for a harmful effect between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      units_text,
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      units_text,
                                      ", supported a clinically important harm.")
      }
      conclusion <- paste0(conclusion_estimate)
    }
    # Check if input is consistent.
    if(!(ci_lower_number<=estimate_number & estimate_number<=ci_upper_number)) {
        conclusion <- "The input is inconsistent."
    }
  }
  
  if(direction_benefical=="large") {
    if(ci_lower_importance!=estimate_importance | estimate_importance!=ci_upper_importance) {
      
      # Estimate.
      conclusion_estimate <- paste0("Assuming no uncontrolled biases, our estimated effect of ", 
                                    number(estimate, accuracy=10^(-decimals)),
                                    units_text,
                                    ", corresponding to a ",
                                    measure_to_text(estimate, 
                                                       decimals=decimals, 
                                                       measure=measure,
                                                       units=units,
                                                       interpret_measure=paste0(measure_type, " "))
      )
      
      if(estimate_importance=="Important benefit") {
        conclusion_estimate <- paste0(conclusion_estimate, ", supported a clinically important benefit.")
      }
      if(estimate_importance=="Too small to be important benefit") {
        conclusion_estimate <- paste0(conclusion_estimate, ", supported a benefit too small to be clinically important.")
      }
      if(estimate_importance=="Too small to be important harm") {
        conclusion_estimate <- paste0(conclusion_estimate, ", supported a harm too small to be clinically important.")
      }
      if(estimate_importance=="Important harm") {
        conclusion_estimate <- paste0(conclusion_estimate, ", supported a clinically important harm.")
      }
      
      # Lower limit. 
      conclusion_ci_lower <- ""
      if(ci_lower_importance=="Important benefit") {
        # Then "ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance", 
        # see below (assuming input is consistent).
      }
      if(ci_lower_importance=="Too small to be important benefit") {
        conclusion_ci_lower <- paste0(
          " and incompatible with an effect smaller than ",
          number(ci_lower, accuracy=10^(-decimals)),
          units_text,
          ".")
      }
      if(ci_lower_importance=="Too small to be important harm") {
        conclusion_ci_lower <- paste0(
          " and incompatible with a harmful effect substantially larger than ",
          number(ci_lower, accuracy=10^(-decimals)),
          units_text,
          ".")
      }
      if(estimate_importance=="Important harm" & ci_lower_importance=="Important harm") {
        conclusion_ci_lower <- paste0(
          " but also an even stronger harmful effect up to  ",
          number(ci_lower, accuracy=10^(-decimals)),
          units_text,
          ".")
      }
      if(estimate_importance!="Important harm" & ci_lower_importance=="Important harm") {
        conclusion_ci_lower <- paste0(
          " but also a strong harmful effect up to ",
          number(ci_lower, accuracy=10^(-decimals)),
          units_text,
          ".")
      }
      
      # Upper limit.
      conclusion_ci_upper <- ""
      if(estimate_importance=="Important benefit" & ci_upper_importance=="Important benefit") {
        conclusion_ci_upper <- paste0(
          " The data were compatible with an even stronger beneficial effect of as much as ",
          number(ci_upper, accuracy=10^(-decimals)),
          units_text,
          ",")
      }
      if(estimate_importance!="Important benefit" & ci_upper_importance=="Important benefit") {
        conclusion_ci_upper <- paste0(
          " The data were compatible with a beneficial effect up to ",
          number(ci_upper, accuracy=10^(-decimals)),
          units_text,
          ",")
      }
      if(ci_upper_importance=="Too small to be important benefit") {
        conclusion_ci_upper <- paste0(
          " The data provided evidence against an effect larger than ",
          number(ci_upper, accuracy=10^(-decimals)),
          units_text,
          ",")
      }
      if(ci_upper_importance=="Too small to be important harm") {
        conclusion_ci_upper <- paste0(
          " The data provided strong evidence against a substantial harmful effect worse than ",
          number(ci_upper, accuracy=10^(-decimals)),
          units_text,
          ",")
      }
      if(ci_upper_importance=="Important harm") {
        # Then "ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance", 
        # see below (assuming input is consistent).
      }
      
      # Create conclusion.
      conclusion <- paste0(conclusion_estimate, 
                           conclusion_ci_upper,
                           conclusion_ci_lower)
    }
    
    # Same interpretation of estimate and confidence limits.
    if(ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance) {
      if(estimate_importance=="Important benefit") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)),
                                      units_text,
                                      ", together with evidence for a beneficial effect between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      units_text,
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      units_text,
                                      ", supported a clinically important benefit.")
      }
      if(estimate_importance=="Too small to be important benefit") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)),
                                      units_text,
                                      ", together with evidence for an effect too small to be clinically important between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      units_text,
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      units_text,
                                      ", supported a benefit too small to be clinically important.")
      }
      if(estimate_importance=="Too small to be important harm") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)),
                                      units_text,
                                      ", together with evidence for a harmful effect too small to be important between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      units_text,
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      units_text,
                                      ", supported a harm too small to be clinically important.")
      }
      if(estimate_importance=="Important harm") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)),
                                      units_text,
                                      ", together with evidence for a harmful effect between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      units_text,
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      units_text,
                                      ", supported a clinically important harm.")
      }
      conclusion <- paste0(conclusion_estimate)
    }
    # Check if input is consistent.
    if(!(ci_lower_number>=estimate_number & estimate_number>=ci_upper_number)) {
      conclusion <- "The input is inconsistent."
    }
  }

  conclusion
}


