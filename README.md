## README

### Conclusion generator

The file Conclusion_function.R contains functions for generating the conclusions in the paper [Schmidt and Parner (2024)](https://www.sciencedirect.com/science/article/pii/S1047279724001042), The Conclusion Generator, Annals of Epidemiology:

* conclusionAB_short and conclusionAB_long create the conclusions in the Statistical Mode in the Concise Mode and Elaborated version, respectively.
* conclusionC creates the conclusion in the Clinical Mode.

The functions uses the *scales* package. 

``` r
# install.packages("devtools")
devtools::install_github("erikparner/winloss")
```

<p xmlns:cc="http://creativecommons.org/ns#" xmlns:dct="http://purl.org/dc/terms/"><a property="dct:title" rel="cc:attributionURL" href="https://github.com/erikparner/conclusion_generator">Conclusion generator</a> by <a rel="cc:attributionURL dct:creator" property="cc:attributionName" href="https://www.au.dk/vis/person/parner@ph.au.dk">Erik Parner</a> is licensed under <a href="http://creativecommons.org/licenses/by/4.0/?ref=chooser-v1" target="_blank" rel="license noopener noreferrer" style="display:inline-block;">CC BY 4.0<img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1"><img style="height:22px!important;margin-left:3px;vertical-align:text-bottom;" src="https://mirrors.creativecommons.org/presskit/icons/by.svg?ref=chooser-v1"></a></p>