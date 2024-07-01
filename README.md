## README

### Conclusion generator

The file Conclusion_function.R contains functions for generating the conclusions in the paper [Schmidt and Parner (2024)](https://www.sciencedirect.com/science/article/pii/S1047279724001042), The Conclusion Generator, Annals of Epidemiology:

* conclusionAB_short and conclusionAB_long create the conclusions in the Statistical Mode in the Concise Mode and Elaborated version, respectively.
* conclusionC creates the conclusion in the Clinical Mode.

The code 

``` r
library(scales)

conclusionAB_short(estimate=0.79,
                   ci_lower=0.61,
                   ci_upper=1.02,
                   superiority= 0.96,
                   noninferiority=1.02,
                   measure = "RR",
                   direction_benefical="small",
                   decimals=2
                   )
                   
conclusionAB_long(estimate=0.79,
                   ci_lower=0.61,
                   ci_upper=1.02,
                   superiority= 0.96,
                   noninferiority=1.02,
                   measure = "RR",
                   direction_benefical="small",
                   decimals=2
                  )

conclusionC(estimate=0.79,
            estimate_importance="Important benefit",
            ci_lower=0.61,
            ci_lower_importance="Important benefit",
            ci_upper=1.02,
            ci_upper_importance="Too small to be important harm",
            decimals=2)                 
```

produces the conclusions 

* Assuming no uncontrolled biases, our results are most compatible with a relative risk of 0.79, although highly compatible with relative risks ranging 0.61 to 1.02. With a superiority bound of 0.96 and a noninferiority bound of 1.02, these results are largely compatible with a beneficial effect, although they are also highly compatible with little or no effect.
*  Assuming no uncontrolled biases, the point estimate of 0.79 corresponds to a 21% risk reduction as the most likely effect given the data, although the interval estimate is compatible with a range from a 39% reduction to a 2% increase in risk from treatment. Given that a reduction of 4%  or more would be considered clinically worth the cost and side effects of the treatment (if any), and an increase of up to 2%  would be considered an acceptable risk, our results support further trials of the treatment.
* Assuming no uncontrolled biases, our estimated effect of 0.79, corresponding to a 21% risk reduction, supported a clinically important benefit. The data were compatible with an even stronger beneficial effect of as much as 0.61, and incompatible with a harmful effect substantially larger than 1.02.

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/erikparner/conclusion_generator">Conclusion generator</a> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://www.au.dk/vis/person/parner@ph.au.dk">Erik Parner</a> is licensed under <a href="http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1"></a></p>