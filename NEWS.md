# hce 0.7.5

* Fixed a bug in `summaryWO.formula()` that previously caused errors when `GROUP` values were used.
* The function `simADHCE()` has been replaced by the `all_data = TRUE` implementation in `simHCE()`.
* The function `simHCE()` now returns an object of a new class called `adhce`. This class inherits from the `hce` class, which itself is a subclass of `data.frame`. The underlying structure of the returned object remains unchanged. The introduction of the `adhce` class is intended to clearly distinguish these structured outputs from the more general `hce` objects. Specifically, an `adhce` object is an analysis-ready `hce` object that is derived using multiple time-to-event outcomes and a single continuous (ordinal or score) endpoint.
* The function `as_hce()` has been updated to support additional output flexibility. If the input data includes the variables `TRTP`, `GROUP`, `AVAL0`, and `PADY`, the function will return an `adhce` object. In this scenario, even if the `AVAL` variable is present, it will be recalculated based on the provided data to ensure consistency with the `adhce` structure. If only the `TRTP` and `AVAL` variables are available, `as_hce()` will return a standard `hce` object. This enhancement allows users to generate either general or analysis-ready `hce` objects, depending on the available input variables.

# hce 0.7.2

* `regWO()` and `stratWO()` are updated to return the confidence interval for the adjusted and stratified (or adjusted/stratified) win probability as well.
* Added the formula implementation of `regWO().`

# hce 0.7.0

* `plot()` method for `hce` objects (created by the function `as_hce()`) is updated to include a `fill` argument for filling the area above the graph.
* `calcWINS()` is updated to include the `SE_WP_Type` argument with default `"biased"` (original implementation) and a new `"unbiased"` implementation of the Bamber-Brunner-Konietschke (see Bamber (1975), Brunner and Konietschke (2025)) standard error for the win proportion.
* New function `IWP()` is added to calculated patient-level individual win proportions.
* Default method for the generic `as_hce()` is added.
* The vignette on hierarchical composite endpoints is updated to include the theoretical framework for the simulation of dependent outcomes using the given copula.
*  The function `simHCE()` is updated to correct for the copula implementation so that `theta = 1` (case of independence) and `theta` close to 1 now give similar results (as expected).

# hce 0.6.7

* The function `simHCE()` is updated to include a new `theta` argument for Gumbel dependence coefficient of the Weibull distributions for time-to-event outcomes. Default is `theta = 1` which assumes independence of time-to-event outcomes. The argument is still experimental.
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