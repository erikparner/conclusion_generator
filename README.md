## README

## Conclusion generator

**Conclusion Generator** is an R implementation of the framework proposed in

> Schmidt, M., & Parner, E. (2024). *The Conclusion Generator*. **Annals of Epidemiology**. https://doi.org/10.1016/j.annepidem.2024.06.008

for generating conclusions for scientific papers based on the values and clinical interpretation of the point estimate and confidence interval. 
It offers two modes for interpretation: (1) **Statistical Mode** provides an accurate statistical interpretation of results, with an optional 
specification of superiority and noninferiority bounds; (2) **Clinical Mode** evaluates the clinical importance of the point estimate and confidence limits as specified by the user. Both modes assume no uncontrolled biases.

The file `Conclusion_function.R` contains updated functions for generating the conclusions that appeared in the paper Schmidt and Parner (2024).

* `conclusion_statistical` creates the conclusions in the *Statistical Mode*.
* `conclusion_clinical` creates the conclusion in the *Clinical Mode*.

A Shiny web app is found [here](https://apps.biostat.au.dk/erikparner/Conclusion_app/). 

The file `Conclusion_function_1994.R` contains the functions for generating the exact conclusions in Schmidt and Parner (2024).


## Installation

The functions can be used directly by downloading the repository and sourcing the main function file.

```r
library(dplyr)
library(scales)
source("Conclusion_function.R")
```


## Example 

Rumbold et al (2006) reported the results of a randomized placebo-controlled trial investigated whether vitamins C and E 
supplements influenced the risk of preeclampsia and perinatal complications. The trial reported *... no significant 
differences between the vitamin and placebo groups in the risk of ... death or serious outcomes in the infant 
(9.5% and 12.1%; relative risk, 0.79; 95% CI, 0.61–1.02)...* The authors concluded that *Supplementation with vitamins 
C and E during pregnancy does not reduce the risk ... of death or other serious outcomes in their infants*.

The conclusion generator based on the reported results can be generated with the following code:
``` r
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
```

which produces the conclusions 

* **Statistical Mode (short)**: <br>
  *Assuming no uncontrolled biases, our results are most compatible with a relative risk of 0.79, although compatible with relative risks ranging 0.61 to 1.02. With a superiority bound of 0.96 and a noninferiority bound of 1.02, the effect of the treatment remains uncertain.*
* **Statistical Mode (long)**: <br>
  *Assuming no uncontrolled biases, the point estimate of 0.79 corresponds to a 21% risk reduction as the most likely effect given the data, although the interval estimate is compatible with a range from a 39% reduction to a 2% increase in risk from treatment. Given that a reduction of 4%  or more would be considered clinically and economically worthwhile, and an increase of up to 2%  would be considered an acceptable risk, the effect of the treatment remains uncertain.*
* **Clinical Mode**: <br>
  *Assuming no uncontrolled biases, our estimated effect of 0.79, corresponding to a 21% risk reduction, supported a clinically important benefit. The data were compatible with an even stronger beneficial effect of as much as 0.61, and incompatible with a harmful effect substantially larger than 1.02.*

## Citation

If you use **Conclusion Generator** in your research, please cite:

> Schmidt, M., & Parner, E. (2024). *The Conclusion Generator*. **Annals of Epidemiology**. https://doi.org/10.1016/j.annepidem.2024.06.008

**BibTeX**

```bibtex
@article{schmidt2024conclusion,
  author  = {Schmidt, M. and Parner, E.},
  title   = {The Conclusion Generator},
  journal = {Annals of Epidemiology},
  year    = {2024},
  doi     = {10.1016/j.annepidem.2024.06.008}
}
```

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/erikparner/conclusion_generator">Conclusion generator</a> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://www.au.dk/vis/person/parner@ph.au.dk">Erik Parner</a> is licensed under <a href="http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1"></a></p>