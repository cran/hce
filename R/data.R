#' `HCE1`, `HCE2`, `HCE3`, `HCE4` datasets for 1000 patients with different treatment effects.
#'
#' A simulated dataset containing the ordinal values and other attributes for 1000 patients.
#' HCE1
#'
#' @format a data frame with 1000 rows and 6 variables:
#' \describe{
#'   \item{SUBJID}{subject ID, numbers from 1 to 1000}
#'   \item{GROUP}{type of the event, either Time-To-Event (TTE) or Continuous (C), character}
#'   \item{GROUPN}{type of the event, for the ordering of outcomes in the `GROUP` variable, numeric}
#'   \item{AVAL}{`AVAL = AVAL0 + GROUPN`, ordinal analysis values for the HCE analysis, numeric, but caution NOT to apply numeric operations; will give meaningless results}
#'   \item{AVAL0}{original values for each type of the event, time for TTE outcomes, numeric values for Continuous outcomes, numeric}
#'   \item{TRTP}{treatment values, A Active or P Placebo, character}
#' }
"HCE1"

#' `HCE1`, `HCE2`, `HCE3`, `HCE4` datasets for 1000 patients with different treatment effects.
#'
#' A simulated dataset containing the ordinal values and other attributes for 1000 patients.
#' HCE2
#'
#' @format a data frame with 1000 rows and 6 variables:
#' \describe{
#'   \item{SUBJID}{subject ID, numbers from 1 to 1000}
#'   \item{GROUP}{type of the event, either Time-To-Event (TTE) or Continuous (C), character}
#'   \item{GROUPN}{type of the event, for the ordering of outcomes in the `GROUP` variable, numeric}
#'   \item{AVAL}{`AVAL = AVAL0 + GROUPN`, ordinal analysis values for the HCE analysis, numeric, but caution NOT to apply numeric operations; will give meaningless results}
#'   \item{AVAL0}{original values for each type of the event, time for TTE outcomes, numeric values for Continuous outcomes, numeric}
#'   \item{TRTP}{treatment values, A Active or P Placebo, character}
#' }
"HCE2"

#' `HCE1`, `HCE2`, `HCE3`, `HCE4` datasets for 1000 patients with different treatment effects.
#'
#' A simulated dataset containing the ordinal values and other attributes for 1000 patients.
#' HCE3
#'
#' @format a data frame with 1000 rows and 6 variables:
#' \describe{
#'   \item{SUBJID}{subject ID, numbers from 1 to 1000}
#'   \item{GROUP}{type of the event, either Time-To-Event (TTE) or Continuous (C), character}
#'   \item{GROUPN}{type of the event, for the ordering of outcomes in the `GROUP` variable, numeric}
#'   \item{AVAL}{`AVAL = AVAL0 + GROUPN`, ordinal analysis values for the HCE analysis, numeric, but caution NOT to apply numeric operations; will give meaningless results}
#'   \item{AVAL0}{original values for each type of the event, time for TTE outcomes, numeric values for Continuous outcomes, numeric}
#'   \item{TRTP}{treatment values, A Active or P Placebo, character}
#' }
"HCE3"


#' `HCE1`, `HCE2`, `HCE3`, `HCE4` datasets for 1000 patients with different treatment effects.
#'
#' A simulated dataset containing the ordinal values and other attributes for 1000 patients.
#' HCE4
#'
#' @format a data frame with 1000 rows and 6 variables:
#' \describe{
#'   \item{SUBJID}{subject ID, numbers from 1 to 1000}
#'   \item{GROUP}{type of the event, either Time-To-Event (TTE) or Continuous (C), character}
#'   \item{GROUPN}{type of the event, for the ordering of outcomes in the `GROUP` variable, numeric}
#'   \item{AVAL}{`AVAL = AVAL0 + GROUPN`, ordinal analysis values for the HCE analysis, numeric, but caution NOT to apply numeric operations; will give meaningless results}
#'   \item{AVAL0}{original values for each type of the event, time for TTE outcomes, numeric values for Continuous outcomes, numeric}
#'   \item{TRTP}{treatment values, A Active or P Placebo, character}
#' }
"HCE4"


#' COVID-19 ordinal scale dataset (full report).
#'
#' A dataset containing dataset with COVID-19 ordinal scale outcomes for 1062 patients.
#'
#' @format a data frame with 1062 rows and 2 variables:
#' \describe{
#'   \item{GROUP}{type of the event, ordinal outcomes 1-8, where a higher value means a better outcome}
#'   \item{TRTP}{treatment values, A Active or P Placebo, character}
#' }
#' @source Beigel et al. (2020) <doi:10.1056/NEJMoa2007764>.
#' @examples
#' #Frequencies
#' table(COVID19)
#' mosaicplot(table(COVID19), col = c(1, 8, 6, 2, 4, 5, 3, 7), 
#' xlab = "Treatment", ylab = "Ordinal Scale", main = "COVID-19 ordinal scale")
#' # Convert to an hce object
#' COVID19HCE <- hce(GROUP = COVID19$GROUP, TRTP = COVID19$TRTP)
#' # Summary wins, losses, and ties with win odds
#' summaryWO(COVID19HCE)
"COVID19"


#' COVID-19 ordinal scale dataset (preliminary report).
#'
#' A dataset containing dataset with COVID-19 ordinal scale outcomes for 844 patients.
#'
#' @format a data frame with 844 rows and 2 variables:
#' \describe{
#'   \item{GROUP}{type of the event, ordinal outcomes 1-8, where a higher value means a better outcome}
#'   \item{TRTP}{treatment values, Active or Placebo, character}
#' }
#' @source Beigel et al. (2020) <doi:10.1056/NEJMoa2007764>.
#' @examples
#' #Frequencies
#' table(COVID19b)
#' mosaicplot(table(COVID19b), col = c(1, 8, 6, 2, 4, 5, 3, 7), 
#' xlab = "Treatment", ylab = "Ordinal Scale", main = "COVID-19 ordinal scale")
#' # Calculate win statistics
#' calcWINS(x = COVID19b, AVAL = "GROUP", TRTP = "TRTP", ref = "Placebo")
"COVID19b"