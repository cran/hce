# hce 0.6.3

* Added a `NEWS.md` file to track changes to the package.
* The hex sticker of the package has been created and is included in all vignettes.
* `HCE1 - HCE4` datasest are upadted to follow the standard structure.
* A new argument `dec` is added to `simHCE()` for decimal places used for rounding the continuous outcome in the simulated dataset. Additionally, the default value for the standard deviation of the continuous variable in the placebo group `CSD_P` is changed to be equal to that of the active group `CSD_A` instead of being equal to 1.
* A new function `simHCE()` which simulates `adhce` objects, that is, an `hce` object with its source datasets. Works similar to `simHCE()` which provides only an `hce` object.
* A new function `simORD()` which simulates ordinal endpoint by categorizing beta distributions.