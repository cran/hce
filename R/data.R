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
