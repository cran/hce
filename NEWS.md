# hce 0.6.7

* The function `simHCE()` is updated to include a new `alpha` argument for Gumbel dependence coefficient of the Weibull distributions for time-to-event outcomes. Default is `alpha = 1` which assumes independence of time-to-event outcomes. The argument is still experimental.
* `calcWO()` is updated to return the confidence interval for the win probability as well.
* `plot()` method for `hce` objects (created by the function `as_hce()`) is implemented to provide the ordinal dominance graph as suggested by Bamber (1975).

# hce 0.6.5

* The functions `powerWO(), sizeWO(), minWO()` are updated to include a new argument `alternative` to specify the class of alternative hypothesis. All formulas are based on the Bamber (1975) paper.
* Added a new dataset `COVID19plus.` 

# hce 0.6.3

* Added a `NEWS.md` file to track changes to the package.
* The hex sticker of the package has been created and is included in all vignettes.
* `HCE1 - HCE4` datasest are updated to follow the standard structure.
* A new argument `dec` is added to `simHCE()` for decimal places used for rounding the continuous outcome in the simulated dataset. Additionally, the default value for the standard deviation of the continuous variable in the placebo group `CSD_P` is changed to be equal to that of the active group `CSD_A` instead of being equal to 1.
* A new function `simADHCE()` which simulates `adhce` objects, that is, an `hce` object with its source datasets. Works similar to `simHCE()` which provides only an `hce` object.
* A new function `simORD()` which simulates ordinal endpoint by categorizing beta distributions.