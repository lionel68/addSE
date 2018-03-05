# addSE

This package provide one function: *add_se* which computes standard errors and confidence intervals for group-level effects adding up standard errors on the way.

You can install the package by running the following code in R

```r
library(devtools)
install_github("lionel68/addSE")
```

The function can then be used with code looking like:

```r
data("iris")
m <- lm(Sepal.Length ~ Species * Sepal.Width, iris)
#to get the fitted average Sepal length per species
add_se(m, "Species")
##                               Coef      LCI      UCI
##(Intercept):Speciessetosa 2.639001 1.518851 3.759152
##Speciesversicolor         3.539735 2.446018 4.633452
##Speciesvirginica          3.906836 2.764838 5.048835

#to get the fitted Sepal length ~ Sepal width slopes per species
add_se(m, name_f = "Species", name_x = "Sepal.Width")
##                                   Coef       LCI      UCI
##Sepal.Width:Speciessetosa     0.6904897 0.3656651 1.015314
##Speciesversicolor:Sepal.Width 0.8650777 0.4726938 1.257462
##Speciesvirginica:Sepal.Width  0.9015345 0.5197338 1.283335
```

These results can then be used for plotting or reporting in result tables.

## TODOs:

* support for other model types
* add plot functions?
* improve documentation
* set up tests cases
