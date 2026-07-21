## README

## Conclusion generator

**Conclusion Generator** is an R implementation of the framework proposed by Schmidt and Parner (2024) for generating standardized textual conclusions from effect estimates and confidence intervals. It provides conclusions in both **Statistical Mode** and **Clinical Mode**, facilitating consistent interpretation and reporting of study results.

The file `Conclusion_function.R` contains updated functions for generating the conclusions that appeared in the paper [Schmidt and Parner (2024)](https://www.sciencedirect.com/science/article/pii/S1047279724001042), _The Conclusion Generator_, _Annals of Epidemiology_:

* `conclusion_statistical` creates the conclusions in the *Statistical Mode*.
* `conclusion_clinical` creates the conclusion in the *Clinical Mode*.

A Shiny web app is found [here](https://apps.biostat.au.dk/erikparner/Conclusion_app/). 

The file `Conclusion_function_1994.R` contains the functions for generating the exact conclusions in Schmidt and Parner (2024).

## Citation

If you use **Conclusion Generator** in your research, please cite:

> Schmidt, M., & Parner, E. (2024). *The Conclusion Generator*. **Annals of Epidemiology**. https://doi.org/10.1016/j.annepidem.2024.05.002

**BibTeX**

```bibtex
@article{schmidt2024conclusion,
  author  = {Schmidt, M. and Parner, E.},
  title   = {The Conclusion Generator},
  journal = {Annals of Epidemiology},
  year    = {2024},
  doi     = {10.1016/j.annepidem.2024.05.002}
}
```

## Installation

The functions can be used directly by downloading the repository and sourcing the main function file.

```r
source("Conclusion_function.R")
```

The examples in this README use the following packages:

```r
library(dplyr)
library(scales)
```

Alternatively, clone the repository using Git:

```bash
git clone https://github.com/erikparner/conclusion_generator.git
```

and then source the function file:

```r
source("Conclusion_function.R")
```


## Example 

Rumbold et al (2006)  examines the effect of vitamins C and E supplements on the risk of preeclampsia and perinatal complications.

``` r
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
```

produces the conclusions 

* **Statistical Mode (short)**: <br>
  *Assuming no uncontrolled biases, our results are most compatible with a relative risk of 0.79, although compatible with relative risks ranging 0.61 to 1.02. With a superiority bound of 0.96 and a noninferiority bound of 1.02, the effect of the treatment remains uncertain.*
* **Statistical Mode (long)**: <br>
  *Assuming no uncontrolled biases, the point estimate of 0.79 corresponds to a 21% risk reduction as the most likely effect given the data, although the interval estimate is compatible with a range from a 39% reduction to a 2% increase in risk from treatment. Given that a reduction of 4%  or more would be considered clinically and economically worthwhile, and an increase of up to 2%  would be considered an acceptable risk, the effect of the treatment remains uncertain.*
* **Clinical Mode**: <br>
  *Assuming no uncontrolled biases, our estimated effect of 0.79, corresponding to a 21% risk reduction, supported a clinically important benefit. The data were compatible with an even stronger beneficial effect of as much as 0.61, and incompatible with a harmful effect substantially larger than 1.02.*

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/erikparner/conclusion_generator">Conclusion generator</a> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://www.au.dk/vis/person/parner@ph.au.dk">Erik Parner</a> is licensed under <a href="http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1"></a></p>