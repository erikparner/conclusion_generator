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

conclusionA <- function(estimate=NA,
                        estimate_importance="",
                        ci_lower=NA,
                        ci_lower_importance="",
                        ci_upper=NA,
                        ci_upper_importance="",
                        measure="RR",
                        decimals=2) {
  
  conclusion <- ""
  measure_type=""
  if(measure=="RR") measure_type="risk"
  if(measure=="RD") measure_type="risk"
  if(measure=="HR"|measure=="IRR") measure_type="rate"
  if(measure=="OR") measure_type="odds"
  
  if(ci_lower_importance!=estimate_importance | estimate_importance!=ci_upper_importance) {
    
    # Estimate.
    if(estimate_importance=="Important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    number(estimate, accuracy=10^(-decimals)), 
                                    ", corresponded to a ",
                                    measure_to_percent(estimate, decimals=decimals, measure=measure, interpret_measure=paste0(measure_type, " ")),
                                    ", supported a clinically important benefit.")
    }
    
    if(estimate_importance=="Too small to be important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    number(estimate, accuracy=10^(-decimals)), 
                                    ", corresponded to a ",
                                    measure_to_percent(estimate, decimals=decimals, measure=measure, interpret_measure=paste0(measure_type, " ")),
                                    ", supported a benefit too small to be clinically important.")
    }
    
    if(estimate_importance=="Too small to be important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    number(estimate, accuracy=10^(-decimals)), 
                                    ", corresponded to a ",
                                    measure_to_percent(estimate, decimals=decimals, measure=measure, interpret_measure=paste0(measure_type, " ")),
                                    ", supported a harm too small to be clinically important.")
    }
    
    if(estimate_importance=="Important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    number(estimate, accuracy=10^(-decimals)), 
                                    ", corresponded to a ",
                                    measure_to_percent(estimate, decimals=decimals, measure=measure, interpret_measure=paste0(measure_type, " ")),
                                    ", supported a clinically important harm.")
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
        " The data provided evidence against a effect larger than ",
        number(ci_lower, accuracy=10^(-decimals)),
        ",")
    }
    if(ci_lower_importance=="Too small to be important harm") {
      conclusion_ci_lower <- paste0(
        " The data provided strong evidence against a substantial harmful effect worse than ",
        number(ci_lower, accuracy=10^(-decimals)),
        ",")
    }
    if(estimate_importance=="Important harm" & ci_lower_importance=="Important harm") {
      conclusion_ci_lower <- paste0(
        " The data leaves open an even stronger harmful effect up to ",
        number(ci_lower, accuracy=10^(-decimals)),
        ",")
    }
    if(estimate_importance=="Important harm" & ci_lower_importance=="Important harm") {
      conclusion_ci_lower <- paste0(
        " The data leaves open an strong harmful effect up to ",
        number(ci_lower, accuracy=10^(-decimals)),
        ",")
    }
    
    # Upper limit.
    conclusion_ci_upper <- ""
    if(ci_upper_importance=="Important benefit") {
      conclusion_ci_upper <- paste0(
        " and it is compatible with a strong beneficial effect up to  ",
        number(ci_upper, accuracy=10^(-decimals)),
        ". ")
    }
    if(ci_upper_importance=="Too small to be important benefit") {
      conclusion_ci_upper <- paste0(
        " and it provided evidence against a harmful effect larger than ",
        number(ci_upper, accuracy=10^(-decimals)),
        ".")
    }
    if(ci_upper_importance=="Too small to be important harm") {
      conclusion_ci_upper <- paste0(
        " and incompatible with an effect substantially larger than ",
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
                                    ", together with evidence for an harmful effect too small to be important between ", 
                                    number(ci_lower, accuracy=10^(-decimals)),
                                    " and ", 
                                    number(ci_upper, accuracy=10^(-decimals)),
                                    ", supported a harm too small to be clinically important.")
    }
    if(estimate_importance=="Important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    number(estimate, accuracy=10^(-decimals)), 
                                    ", together with evidence for an harmful effect between ", 
                                    number(ci_lower, accuracy=10^(-decimals)),
                                    " and ", 
                                    number(ci_upper, accuracy=10^(-decimals)),
                                    ", supported a clinically important harm.")
    }
    
    
    # Create conclusion.
    conclusion <- paste0(conclusion_estimate)
  }
  
  conclusion
}

conclusionBC <- function(estimate=NA,
                         ci_lower=NA,
                         ci_upper=NA,
                         noninferiority=NA,
                         superiority=NA,
                         measure="RR",
                         decimals=2) {
  
  
  measure_type=""
  if(measure=="RR") measure_type="risk"
  if(measure=="RD") measure_type="risk"
  if(measure=="HR"|measure=="IRR") measure_type="rate"
  if(measure=="OR") measure_type="odds"
  
  # Estimate
  conclusion <- paste0("Assuming no uncontrolled biases, the point estimate of ",
                       number(estimate, accuracy=10^(-decimals)),
                       " corresponded to a ",
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
    mention_superiority_noninferiority <-  paste0("Given that a reduction of ",
                                                  measure_to_percent(superiority, decimals=decimals, measure=measure, interpret_direction=FALSE),
                                                  " or more would be considered clinically worth the cost and side effects of the treatment (if any)",
                                                  ", and an increase of up to ",
                                                  measure_to_percent(noninferiority, decimals=decimals, measure=measure, interpret_direction=FALSE),
                                                  " would be considered an acceptable risk,")
    mention_superiority <-  paste0("Given that a reduction of ",
                                   measure_to_percent(superiority, decimals=decimals, measure=measure, interpret_direction=FALSE),
                                   " or more would be considered worth the cost and side effects of the treatment (if any),")
    mention_noninferiority <-  paste0("Given that an increase of up to ",
                                      measure_to_percent(noninferiority, decimals=decimals, measure=measure, interpret_direction=FALSE),
                                      " would be considered an acceptable risk given the large balance of evidence is toward benefit,")
    if(ci_lower<=superiority & ci_upper<=superiority) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority,
                            " our results show evidence for important benefit of the treatment."
      )
    }
    if(ci_lower<=superiority & superiority<ci_upper & ci_upper<=noninferiority) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results support further trials of the treatment."
      )
    }
    if(ci_lower<=superiority & noninferiority<ci_upper ) {
      conclusion  <- paste0(conclusion,
                            mention_superiority_noninferiority,
                            " the effect of the treatment remains uncertain."
      )
    }
    if(superiority<ci_lower & ci_lower<=noninferiority & ci_upper<=noninferiority) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results provide evidence for no important benefit of the treatment, but also evidence for no important increase in risk."
      )
    }
    if(superiority<ci_lower & ci_lower<=noninferiority & noninferiority<ci_upper) {
      conclusion  <- paste0(conclusion, 
                            mention_superiority_noninferiority,
                            " our results provide evidence for no important benefit of the treatment, but it may be associated with an important increase in risk."
      )
    }
    if(noninferiority<ci_lower & ci_upper<noninferiority) {
      conclusion  <- paste0(conclusion, 
                            mention_noninferiority,
                            " our results provide evidence for important harm of the treatment."
      )
    }
  }
  
  conclusion
}

measure_to_percent(-0.20, measure="RD", interpret_measure=paste0("risk", " "))
# rd_to_percent(-0.20, interpret_measure=paste0("risk", " "))
