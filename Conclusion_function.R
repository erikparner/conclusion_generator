  # This version has intermediate levels.
conclusion_course <- function(estimate="",
                       estimate_importance="",
                       ci_lower="",
                       ci_lower_importance="",
                       ci_upper="",
                       ci_upper_importance="",
                       significance="",
                       mode="") {
  
  conclusion <- ""
  
  if(ci_lower_importance!=estimate_importance | estimate_importance!=ci_upper_importance) {
    
    # Estimate.
    if(estimate_importance=="Important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " provides support for a benefit large enough to be clinically important")
    }
    
    if(estimate_importance=="Maybe important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " provides support for a benefit large enough to be clinically important")
    }
    
    if(estimate_importance=="Too small to be important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " provides support for a benefit too small to be clinically important")
    }
    
    if(estimate_importance=="Too small to be important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " provides support for a harm too small to be clinically important")
    }
    
    if(estimate_importance=="Maybe important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " provides support for a harm big enough to be clinically important")
    }
    
    if(significance=="Yes" & estimate_importance=="Important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " provides support for a harm big enough to be clinically important")
    }
    
    # Significance.
    if(significance=="Leave out") {
      conclusion_significance <- ". "
    } else {
      if(significance=="Yes") {
        conclusion_significance <- ", and this was statistical significant. "
      } else {
        conclusion_significance <- ", but this did not reach statistical significance. "
      }
      
    }
    
    # Lower limit.
    conclusion_ci_lower <- ""
    if(ci_lower_importance=="Important benefit") {
      conclusion_ci_lower <- paste0(
        " The confidence interval is compatible with a strong beneficial effect of as much as ",
        ci_lower,
        ", which is large enough to clearly be clinically important,")
      
    }
    if(ci_lower_importance=="Maybe important benefit") {
      conclusion_ci_lower <- paste0(
        " The confidence interval is compatible with a beneficial effect of as much as ",
        ci_lower,
        ".")
    }
    if(ci_lower_importance=="Too small to be important benefit") {
      conclusion_ci_lower <- paste0(
        " The confidence interval provides evidence against a harmful effect larger than ",
        ci_lower,
        ".")
    }
    if(ci_lower_importance=="Too small to be important harm") {
      conclusion_ci_lower <- paste0(
        " The confidence interval provides strong evidence against a substantial harmful effect worse than ",
        ci_lower,
        ".")
    }
    if(ci_lower_importance=="Maybe important harm") {
      conclusion_ci_lower <- paste0(
        " The confidence interval is compatible with a harmful effect of as much as ",
        ci_lower,
        ".")
    }
    if(ci_lower_importance=="Important harm") {
      conclusion_ci_lower <- paste0(
        " The confidence interval leaves open a strong harmful effect of as large as ",
        ci_lower,
        ".")
    }
    
    # Upper limit.
    conclusion_ci_upper <- ""
    if(ci_upper_importance=="Important benefit") {
      conclusion_ci_upper <- paste0(
        " and it is compatible with a strong beneficial effect of as much as ",
        ci_upper,
        ", which is large enough to clearly be important,")
    }
    if(ci_upper_importance=="Maybe important benefit") {
      conclusion_ci_upper <- paste0(
        " and it is compatible with a benefitcial effect of as much as ",
        ci_upper,
        ".")
    }
    if(ci_upper_importance=="Too small to be important benefit") {
      conclusion_ci_upper <- paste0(
        " and it provides evidence against a harmful effect larger than ",
        ci_upper,
        ".")
    }
    if(ci_upper_importance=="Too small to be important harm") {
      conclusion_ci_upper <- paste0(
        " and it provides strong evidence against a substantial harmful effect worse than ",
        ci_upper,
        ".")
    }
    if(ci_upper_importance=="Maybe important harm") {
      conclusion_ci_upper <- paste0(
        " but it is compatible with a harmful effect of as much as ",
        ci_upper,
        ".")
    }
    if(ci_upper_importance=="Important harm") {
      conclusion_ci_upper <- paste0(
        " but it leaves open a strong harmful effect of as large as ",
        ci_upper,
        ".")
    }
    
    # Create conclusion.
    conclusion <- paste0(conclusion_estimate, 
                         conclusion_significance,
                         conclusion_ci_lower,
                         conclusion_ci_upper)
  }
  
  if(ci_lower_importance==estimate_importance & estimate_importance==ci_upper_importance) {
    
    # Estimate.
    if(estimate_importance=="Important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an effect between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", provides support for a benefit large enough to be important")
    }
    
    if(estimate_importance=="Maybe important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an effect between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", provides support for a benefit large enough to be important")
    }
    
    if(estimate_importance=="Too small to be important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an effect between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", provides support for a benefit too small to be important")
    }
    
    if(estimate_importance=="Too small to be important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an effect between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", provides support for a harm too small to be important")
    }
    
    if(estimate_importance=="Maybe important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an effect between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", provides support for a harm big enough to be important")
    }
    
    if(estimate_importance=="Important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an effect between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", provides support for a harm big enough to be important")
    }
    
    # Significance.
    if(significance=="Leave out") {
      conclusion_significance <- ". "
    } else {
      if(significance=="Yes") {
        conclusion_significance <- ", and this was statistical significant. "
      } else {
        conclusion_significance <- ", but this did not reach statistical significance. "
      }
      
    }
    
    # Create conclusion.
    conclusion <- paste0(conclusion_estimate, 
                         conclusion_significance)
  }
  
  if(mode=="MD") {
    if(significance=="Yes") {
      conclusion <- "The study provide evidence that the exposure (treatment) effects the outcome."
    }
    if(significance=="No") {
      conclusion <- "There was no effect of the exposure (treatment) on the outcome."
    }
  }
  
  conclusion
}




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

conclusionA <- function(estimate=NA,
                        ci_lower=NA,
                        ci_upper=NA,
                        noninferiority=NA,
                        superiority=NA,
                        measure="RR",
                        outcome="negative",
                        design="RCT",
                        decimals=2) {
  
  
  measure_type="xxx"
  if(measure=="RR") measure_type="risk"
  if(measure=="HR") measure_type="rate"
  
  # Estimate
  conclusion <- paste0("Assuming no uncontrolled biases, the point estimate of ",
                       number(estimate, accuracy=10^(-decimals)),
                       " corresponded to a ",
                       relative_to_percent(estimate, interpret_measure=paste0(measure_type, " ")),
                       " as the most likely effect given the data"
                       )

  # Interval.
  conclusion <- paste0(conclusion, 
                       ", although the interval estimate is compatible with a range from a ",
                       relative_to_percent(ci_lower),
                       " to a ",
                       relative_to_percent(ci_upper),
                       " in ",
                       measure_type, 
                       " from treatment. "
                       )
  
  if(!is.na(superiority) & !is.na(noninferiority)) {
    # Superiority and noninferiority.
    mention_superiority_noninferiority <-  paste0("Given that a reduction of ",
                                                  relative_to_percent(superiority, interpret_direction=FALSE),
                                                  " or more would be considered clinically worth the cost and side effects of the treatment (if any)",
                                                  ", and an increase of up to ",
                                                  relative_to_percent(noninferiority, interpret_direction=FALSE),
                                                  " would be considered an acceptable risk,")
    mention_superiority <-  paste0("Given that a reduction of ",
                                   relative_to_percent(superiority, interpret_direction=FALSE),
                                   " or more would be considered worth the cost and side effects of the treatment (if any),")
    mention_noninferiority <-  paste0("Given that an increase of up to ",
                                      relative_to_percent(noninferiority, interpret_direction=FALSE),
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



conclusionC <- function(estimate="",
                        estimate_importance="",
                        ci_lower="",
                        ci_lower_importance="",
                        ci_upper="",
                        ci_upper_importance="",
                        mode="") {
  
  conclusion <- ""
  
  if(ci_lower_importance!=estimate_importance | estimate_importance!=ci_upper_importance) {
    
    # Estimate.
    if(estimate_importance=="Important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " supported a clinically important benefit.")
    }
    
    if(estimate_importance=="Too small to be important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " supported a benefit too small to be clinically important.")
    }
    
    if(estimate_importance=="Too small to be important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " supported a harm too small to be clinically important.")
    }
    
    if(estimate_importance=="Important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    " supported a clinically important harm.")
    }
    
    # Lower limit.
    conclusion_ci_lower <- ""
    if(estimate_importance=="Important benefit" & ci_lower_importance=="Important benefit") {
      conclusion_ci_lower <- paste0(
        " The data were compatible with an even stronger beneficial effect of as much as ",
        ci_lower,
        ",")
      
    }
    if(estimate_importance!="Important benefit" & ci_lower_importance=="Important benefit") {
      conclusion_ci_lower <- paste0(
        " The data were compatible with a beneficial effect up to ",
        ci_lower,
        ",")
      
    }
    if(ci_lower_importance=="Too small to be important benefit") {
      conclusion_ci_lower <- paste0(
        " The data provided evidence against a effect larger than ",
        ci_lower,
        ",")
    }
    if(ci_lower_importance=="Too small to be important harm") {
      conclusion_ci_lower <- paste0(
        " The data provided strong evidence against a substantial harmful effect worse than ",
        ci_lower,
        ",")
    }
    if(estimate_importance=="Important harm" & ci_lower_importance=="Important harm") {
      conclusion_ci_lower <- paste0(
        " The data leaves open an even stronger harmful effect up to ",
        ci_lower,
        ",")
    }
    if(estimate_importance=="Important harm" & ci_lower_importance=="Important harm") {
      conclusion_ci_lower <- paste0(
        " The data leaves open an strong harmful effect up to ",
        ci_lower,
        ",")
    }
    
    # Upper limit.
    conclusion_ci_upper <- ""
    if(ci_upper_importance=="Important benefit") {
      conclusion_ci_upper <- paste0(
        " and it is compatible with a strong beneficial effect up to  ",
        ci_upper,
        ". ")
    }
    if(ci_upper_importance=="Too small to be important benefit") {
      conclusion_ci_upper <- paste0(
        " and it provided evidence against a harmful effect larger than ",
        ci_upper,
        ".")
    }
    if(ci_upper_importance=="Too small to be important harm") {
      conclusion_ci_upper <- paste0(
        " and incompatible with an effect substantially larger than ",
        ci_upper,
        ".")
    }
    if(estimate_importance=="Important harm" & ci_upper_importance=="Important harm") {
      conclusion_ci_upper <- paste0(
        " but also an even stronger harmful effect up to  ",
        ci_upper,
        ".")
    }
    if(estimate_importance!="Important harm" & ci_upper_importance=="Important harm") {
      conclusion_ci_upper <- paste0(
        " but also a strong harmful effect up to ",
        ci_upper,
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
                                    estimate, 
                                    ", together with evidence for a beneficial effect between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", supported a clinically important benefit.")
    }
    
    if(estimate_importance=="Too small to be important benefit") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an effect too small to be clinically important between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", supported a benefit too small to be clinically important.")
    }
    if(estimate_importance=="Too small to be important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an harmful effect too small to be important between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", supported a harm too small to be clinically important.")
    }
    if(estimate_importance=="Important harm") {
      conclusion_estimate <- paste0("Our estimated effect of ", 
                                    estimate, 
                                    ", together with evidence for an harmful effect between ", 
                                    ci_lower, 
                                    " and ", 
                                    ci_upper,
                                    ", supported a clinically important harm.")
    }
    

    # Create conclusion.
    conclusion <- paste0(conclusion_estimate)
  }
  
  conclusion
}

