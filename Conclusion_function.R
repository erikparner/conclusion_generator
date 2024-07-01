# library(scales)

relative_to_percent <- function(relative, decimals=2, interpret_measure="", interpret_direction=TRUE) {
  if(relative<=1) {
    if(interpret_direction) {
      posttext <- "reduction"
    } else {
      posttext <- ""
    }
    text <- paste0(percent(1-relative, accuracy=10^(-(decimals-2))),
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
    text <- paste0(percent(relative-1, accuracy=10^(-(decimals-2))), 
                   " ",
                   interpret_measure, 
                   posttext
    )
  }
  text
}

rd_to_percent <- function(rd, decimals=2, interpret_measure="", interpret_direction=TRUE) {
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

measure_to_percent <- function(number, decimals=2, measure="RR", interpret_measure="", interpret_direction=TRUE) {
  if(measure %in% c("RR", "HR", "IRR", "OR")) {
    text=relative_to_percent(relative=number, 
                             decimal=decimals, 
                             interpret_measure=interpret_measure, 
                             interpret_direction=interpret_direction)
  }
  if(measure=="RD") {
    text=rd_to_percent(rd=number, 
                       decimal=decimals, 
                       interpret_measure=interpret_measure, 
                       interpret_direction=interpret_direction)
  }
  text
}


conclusionAB_short <- function(
    estimate=NA,
    ci_lower=NA,
    ci_upper=NA,
    noninferiority=NA,
    superiority=NA,
    measure="RR",
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
  

  # Estimate
  conclusion <- paste0("Assuming no uncontrolled biases, our results are most compatible with a ",
                       measure_type,
                       " of ",
                       number(estimate, accuracy=10^(-decimals))
  )
  
  # Interval.
  conclusion <- paste0(conclusion, 
                       ", although highly compatible with ",
                       measure_type,
                       "s ranging ",
                       number(ci_lower, accuracy=10^(-decimals)),
                       " to ",
                       number(ci_upper, accuracy=10^(-decimals)),
                       ". "
  )
  
  if(!is.na(superiority) & !is.na(noninferiority)) {
    # Superiority and non-inferiority.
    mention_superiority_noninferiority <-  paste0("With a superiority bound of ",
                                                  number(superiority, accuracy=10^(-decimals)),
                                                  " and a noninferiority bound of ",
                                                  number(noninferiority, accuracy=10^(-decimals)),
                                                  ",")
    mention_superiority <-  paste0("With a superiority bound of ",
                                   number(superiority, accuracy=10^(-decimals)),
                                   ",")
    mention_noninferiority <-  paste0("With a noninferiority bound of ",
                                      number(noninferiority, accuracy=10^(-decimals)),
                                      ",")
    
    # Case 1. Example D. 
    if( (direction_benefical=="small" & ci_upper<superiority) | 
        (direction_benefical=="large" & superiority<ci_lower) ) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority,
                            " these results are largely compatible with a beneficial effect."
      )
    }
    # Case 2.a Example C. 
    if( (direction_benefical=="small" & ci_lower<superiority & superiority<ci_upper & ci_upper<=no_effect) | 
        (direction_benefical=="large" & no_effect<=ci_lower & ci_lower<superiority & superiority<ci_upper) ) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " these results are largely compatible with a beneficial effect, although they are also highly compatible with little effect."
      )
    }
    # Case 2.b Example Vitamin. 
    if( (direction_benefical=="small" & ci_lower<superiority & no_effect<ci_upper & ci_upper<=noninferiority) | 
        (direction_benefical=="large" & noninferiority<=ci_lower & ci_lower<no_effect & superiority<ci_upper) ) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " these results are largely compatible with a beneficial effect, although they are also highly compatible with little or no effect."
      )
    }
    # Case 3. Example A, B.
    if( (direction_benefical=="small" & ci_lower<superiority & noninferiority<ci_upper) |
        (direction_benefical=="large" & ci_lower<noninferiority & superiority<ci_upper ) ) {
      conclusion  <- paste0(conclusion,
                            mention_superiority_noninferiority,
                            " the effect of the treatment remains uncertain."
      )
    }
    # Case 4. Example E.
    if( (direction_benefical=="small" & superiority<=ci_lower & ci_upper<=noninferiority) |
        (direction_benefical=="large" & noninferiority<=ci_lower & ci_upper<=superiority) ) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results provide evidence for no important benefit of the treatment, but also evidence for no important increase in risk."
      )
    }
    # Case 5. Example F.
    if( (direction_benefical=="small" & superiority<=ci_lower & ci_lower<=noninferiority & noninferiority<ci_upper) |
        (direction_benefical=="large" & ci_lower<noninferiority & noninferiority<=ci_upper & ci_upper<=superiority) ) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results provide evidence for no important benefit of the treatment, but it may be associated with an important increase in risk."
      )
    }
    # Case 6.
    if( (direction_benefical=="small" & noninferiority<ci_lower) | 
        (direction_benefical=="large" & ci_upper<noninferiority) ) {
      conclusion  <- paste0(conclusion, 
                            mention_noninferiority,
                            " our results provide evidence for important harm of the treatment."
      )
    }
  }
  
  conclusion
}


conclusionAB_long <- function(estimate=NA,
                         ci_lower=NA,
                         ci_upper=NA,
                         noninferiority=NA,
                         superiority=NA,
                         measure="RR",
                         decimals=2,
                         direction_benefical="small") {
  
  
  measure_type=""
  if(measure=="RR" & direction_benefical=="small") measure_type="risk"
  if(measure=="RD" & direction_benefical=="small") measure_type="risk"
  if(measure=="RR" & direction_benefical=="large") measure_type="response rate"
  if(measure=="RD" & direction_benefical=="large") measure_type="response rate"
  if(measure=="HR"|measure=="IRR") measure_type="rate"
  if(measure=="OR") measure_type="odds"
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
                       " corresponds to a ",
                       measure_to_percent(estimate, decimals=decimals, measure=measure, interpret_measure=paste0(measure_type, " ")),
                       " as the most likely effect given the data"
  )
  
  # Interval.
  conclusion <- paste0(conclusion, 
                       ", although the interval estimate is compatible with a range from a ",
                       measure_to_percent(ci_lower, decimals=decimals, measure=measure),
                       " to a ",
                       measure_to_percent(ci_upper, decimals=decimals, measure=measure),
                       " in ",
                       measure_type, 
                       " from treatment. "
  )
  
  if(!is.na(superiority) & !is.na(noninferiority)) {
    # Superiority and non-inferiority.
    
    mention_superiority_noninferiority <-  paste0("Given that ", direction_superiority, " of ",
                                                  measure_to_percent(superiority, decimals=decimals, measure=measure, interpret_direction=FALSE),
                                                  " or more would be considered clinically worth the cost and side effects of the treatment (if any)",
                                                  ", and ", direction_noninferiority, " of up to ",
                                                  measure_to_percent(noninferiority, decimals=decimals, measure=measure, interpret_direction=FALSE),
                                                  " would be considered an acceptable risk,")
    mention_superiority <-  paste0("Given that ", direction_superiority, " of ",
                                   measure_to_percent(superiority, decimals=decimals, measure=measure, interpret_direction=FALSE),
                                   " or more would be considered worth the cost and side effects of the treatment (if any),")
    mention_noninferiority <-  paste0("Given that ", direction_noninferiority, " of up to ",
                                      measure_to_percent(noninferiority, decimals=decimals, measure=measure, interpret_direction=FALSE),
                                      " would be considered an acceptable risk given the large balance of evidence is toward benefit,")
    
    # Case 1. Example D. 
    if(direction_benefical=="small" & ci_upper<superiority) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority,
                            " our results show evidence for important benefit of the treatment."
      )
    }
    if(direction_benefical=="large" & superiority<ci_lower) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority,
                            " our results show evidence for important benefit of the treatment."
      )
    }
    # Case 2. Example C. 
    if(direction_benefical=="small" & ci_lower<superiority & superiority<ci_upper & ci_upper<=noninferiority) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results support further trials of the treatment."
      )
    }
    if(direction_benefical=="large" & noninferiority<=ci_lower & ci_lower<superiority & superiority<ci_upper) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results support further trials of the treatment."
      )
    }
    # Case 3. Example A.
    if(direction_benefical=="small" & ci_lower<superiority & noninferiority<ci_upper) {
      conclusion  <- paste0(conclusion,
                            mention_superiority_noninferiority,
                            " the effect of the treatment remains uncertain."
      )
    }
    if(direction_benefical=="large" & ci_lower<noninferiority & superiority<ci_upper ) {
      conclusion  <- paste0(conclusion,
                            mention_superiority_noninferiority,
                            " the effect of the treatment remains uncertain."
      )
    }
    # Case 4. Example E.
    if(direction_benefical=="small" & superiority<=ci_lower & ci_upper<=noninferiority) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results provide evidence for no important benefit of the treatment, but also evidence for no important increase in risk."
      )
    }
    if(direction_benefical=="large" & noninferiority<=ci_lower & ci_upper<=superiority) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results provide evidence for no important benefit of the treatment, but also evidence for no important decrease in response chance."
      )
    }
    # Case 5. Example F.
    if(direction_benefical=="small" & superiority<=ci_lower & ci_lower<=noninferiority & noninferiority<ci_upper) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results provide evidence for no important benefit of the treatment, but it may be associated with an important increase in risk."
      )
    }
    if(direction_benefical=="large" & ci_lower<noninferiority & noninferiority<=ci_upper & ci_upper<=superiority) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results provide evidence for no important benefit of the treatment, but it may be associated with an important decrease in response chance."
      )
    }
    # Case 6.
    if(direction_benefical=="small" & noninferiority<ci_lower) {
      conclusion  <- paste0(conclusion, 
                            mention_noninferiority,
                            " our results provide evidence for important harm of the treatment."
      )
    }
    if(direction_benefical=="large" & ci_upper<noninferiority) {
      conclusion  <- paste0(conclusion, 
                            mention_noninferiority,
                            " our results provide evidence for important harm of the treatment."
      )
    }
  }
  
  conclusion
}





conclusionC <- function(estimate=NA,
                        estimate_importance="",
                        ci_lower=NA,
                        ci_lower_importance="",
                        ci_upper=NA,
                        ci_upper_importance="",
                        measure="RR",
                        decimals=2,
                        direction_benefical="small") {
  
  conclusion <- ""
  
  measure_type=""
  if(measure=="RR" & direction_benefical=="small") measure_type="risk"
  if(measure=="RD" & direction_benefical=="small") measure_type="risk"
  if(measure=="RR" & direction_benefical=="large") measure_type="response"
  if(measure=="RD" & direction_benefical=="large") measure_type="response"
  if(measure=="HR"|measure=="IRR") measure_type="rate"
  if(measure=="OR") measure_type="odds"
  
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
                                    ", corresponding to a ",
                                    measure_to_percent(estimate, 
                                                       decimals=decimals, 
                                                       measure=measure, 
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
          ",")
        
      }
      if(estimate_importance!="Important benefit" & ci_lower_importance=="Important benefit") {
        conclusion_ci_lower <- paste0(
          " The data were compatible with a beneficial effect up to ",
          number(ci_lower, accuracy=10^(-decimals)),
          ",")
        
      }
      if(ci_lower_importance=="Too small to be important benefit") {
        conclusion_ci_lower <- paste0(
          " The data provided evidence against an effect larger than ",
          number(ci_lower, accuracy=10^(-decimals)),
          ",")
      }
      if(ci_lower_importance=="Too small to be important harm") {
        conclusion_ci_lower <- paste0(
          " The data provided strong evidence against a substantial harmful effect worse than ",
          number(ci_lower, accuracy=10^(-decimals)),
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
          ".")
      }
      if(ci_upper_importance=="Too small to be important harm") {
        conclusion_ci_upper <- paste0(
          " and incompatible with a harmful effect substantially larger than ",
          number(ci_upper, accuracy=10^(-decimals)),
          ".")
      }
      if(estimate_importance=="Important harm" & ci_upper_importance=="Important harm") {
        conclusion_ci_upper <- paste0(
          " but also an even stronger harmful effect up to  ",
          number(ci_upper, accuracy=10^(-decimals)),
          ".")
      }
      if(estimate_importance!="Important harm" & ci_upper_importance=="Important harm") {
        conclusion_ci_upper <- paste0(
          " but also a strong harmful effect up to ",
          number(ci_upper, accuracy=10^(-decimals)),
          ".")
      }
      
      # Create conclusion.
      conclusion <- paste0(conclusion_estimate, 
                           conclusion_ci_lower,
                           conclusion_ci_upper)
    }
  
    # Same interpretation of estimate and confidence limits.
    if(ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance) {
      # Estimate.
      if(estimate_importance=="Important benefit") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)), 
                                      ", together with evidence for a beneficial effect between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      ", supported a clinically important benefit.")
      }
      
      if(estimate_importance=="Too small to be important benefit") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)), 
                                      ", together with evidence for an effect too small to be clinically important between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      ", supported a benefit too small to be clinically important.")
      }
      if(estimate_importance=="Too small to be important harm") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)), 
                                      ", together with evidence for a harmful effect too small to be important between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      ", supported a harm too small to be clinically important.")
      }
      if(estimate_importance=="Important harm") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)), 
                                      ", together with evidence for a harmful effect between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      ", supported a clinically important harm.")
      }
      
      # Create conclusion.
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
                                    ", corresponding to a ",
                                    measure_to_percent(estimate, 
                                                       decimals=decimals, 
                                                       measure=measure, 
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
          ".")
      }
      if(ci_lower_importance=="Too small to be important harm") {
        conclusion_ci_lower <- paste0(
          " and incompatible with a harmful effect substantially larger than ",
          number(ci_lower, accuracy=10^(-decimals)),
          ".")
      }
      if(estimate_importance=="Important harm" & ci_lower_importance=="Important harm") {
        conclusion_ci_lower <- paste0(
          " but also an even stronger harmful effect up to  ",
          number(ci_lower, accuracy=10^(-decimals)),
          ".")
      }
      if(estimate_importance!="Important harm" & ci_lower_importance=="Important harm") {
        conclusion_ci_lower <- paste0(
          " but also a strong harmful effect up to ",
          number(ci_lower, accuracy=10^(-decimals)),
          ".")
      }
      
      # Upper limit.
      conclusion_ci_upper <- ""
      if(estimate_importance=="Important benefit" & ci_upper_importance=="Important benefit") {
        conclusion_ci_upper <- paste0(
          " The data were compatible with an even stronger beneficial effect of as much as ",
          number(ci_upper, accuracy=10^(-decimals)),
          ",")
        
      }
      if(estimate_importance!="Important benefit" & ci_upper_importance=="Important benefit") {
        conclusion_ci_upper <- paste0(
          " The data were compatible with a beneficial effect up to ",
          number(ci_upper, accuracy=10^(-decimals)),
          ",")
        
      }
      if(ci_upper_importance=="Too small to be important benefit") {
        conclusion_ci_upper <- paste0(
          " The data provided evidence against an effect larger than ",
          number(ci_upper, accuracy=10^(-decimals)),
          ",")
      }
      if(ci_upper_importance=="Too small to be important harm") {
        conclusion_ci_upper <- paste0(
          " The data provided strong evidence against a substantial harmful effect worse than ",
          number(ci_upper, accuracy=10^(-decimals)),
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
    # -> As as small.
    if(ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance) {
      # Estimate.
      if(estimate_importance=="Important benefit") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)), 
                                      ", together with evidence for a beneficial effect between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      ", supported a clinically important benefit.")
      }
      
      if(estimate_importance=="Too small to be important benefit") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)), 
                                      ", together with evidence for an effect too small to be clinically important between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      ", supported a benefit too small to be clinically important.")
      }
      if(estimate_importance=="Too small to be important harm") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)), 
                                      ", together with evidence for a harmful effect too small to be important between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      ", supported a harm too small to be clinically important.")
      }
      if(estimate_importance=="Important harm") {
        conclusion_estimate <- paste0("Our estimated effect of ", 
                                      number(estimate, accuracy=10^(-decimals)), 
                                      ", together with evidence for a harmful effect between ", 
                                      number(ci_lower, accuracy=10^(-decimals)),
                                      " and ", 
                                      number(ci_upper, accuracy=10^(-decimals)),
                                      ", supported a clinically important harm.")
      }
      
      # Create conclusion.
      conclusion <- paste0(conclusion_estimate)
    }
    # Check if input is consistent.
    if(!(ci_lower_number>=estimate_number & estimate_number>=ci_upper_number)) {
      conclusion <- "The input is inconsistent."
    }
  }

  conclusion
}

