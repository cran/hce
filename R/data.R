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
#' A dataset with COVID-19 ordinal scale outcomes for 1062 patients.
#'
#' @format a data frame with 1062 rows and 2 variables:
#' \describe{
#'   \item{GROUP}{type of the event, ordinal outcomes 1-8, where a higher value means a better outcome}
#'   \item{TRTP}{treatment values, A Active or P Placebo, character}
#' }
#' @source Beigel JH et al "Remdesivir for the treatment of Covid-19-final report." New England Journal of Medicine 383.19 (2020): 1813-1836 <doi:10.1056/NEJMoa2007764>.
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
#' A dataset with COVID-19 ordinal scale outcomes for 844 patients.
#'
#' @format a data frame with 844 rows and 2 variables:
#' \describe{
#'   \item{GROUP}{type of the event, ordinal outcomes 1-8, where a higher value means a better outcome}
#'   \item{TRTP}{treatment values, Active or Placebo, character}
#' }
#' @source Beigel JH et al "Remdesivir for the treatment of Covid-19-final report." New England Journal of Medicine 383.19 (2020): 1813-1836 <doi:10.1056/NEJMoa2007764>.
#' @examples
#' #Frequencies
#' table(COVID19b)
#' mosaicplot(table(COVID19b), col = c(1, 8, 6, 2, 4, 5, 3, 7), 
#' xlab = "Treatment", ylab = "Ordinal Scale", main = "COVID-19 ordinal scale")
#' # Calculate win statistics
#' calcWINS(x = COVID19b, AVAL = "GROUP", TRTP = "TRTP", ref = "Placebo")
"COVID19b"



#' Kidney Hierarchical Composite Endpoint dataset.
#'
#' A dataset with kidney ordinal scale outcomes of 1500 patients in the `ADSL` dataset.
#'
#' @format a data frame with 1500 rows and 11 variables:
#' \describe{
#'   \item{ID}{patient identifiers, numeric}
#'   \item{TRTPN}{treatment values, 1 Active or 2 Placebo, numeric}
#'   \item{AVAL0}{original values for each type of the event, time for TTE outcomes 1-6, numeric values for Continuous outcome 7, numeric}
#'   \item{AVAL}{`AVAL = AVAL0 + GROUPN`, ordinal analysis values for the HCE analysis, numeric, but caution NOT to apply numeric operations; will give meaningless results}
#'   \item{GROUP}{name of the event, character}
#'   \item{GROUPN}{ordinal outcomes corresponding to `PARAMN` values, numeric}
#'   \item{PARAMCD}{coded name of the event, character}
#'   \item{PARAMN}{severity of the event, outcomes 1-7, where a higher value means a better outcome, character}
#'   \item{STRATAN}{strata 1-4, higher value means more severe kidney disease, numeric}
#'   \item{EGFRBL}{Baseline GFR values of patients, numeric}
#'   \item{TRTP}{treatment values, A Active or P Placebo, character}
#'   \item{PADY}{primary analysis day (in years), length of the fixed follow-up, numeric}
#' }
#' @source 
#' Heerspink HL et al "Development and validation of a new hierarchical composite endpoint for clinical trials of kidney disease progression." Journal of the American Society of Nephrology (2023).
#' @examples
#' # Adjusted win odds
#' res <- regWO(x = KHCE, AVAL = "AVAL", TRTP = "TRTP", COVAR = "STRATAN", ref = "P")
#' res
"KHCE"



#' Event-Time dataset for kidney outcomes.
#'
#' A dataset with multiple kidney outcomes over time scale outcomes of 1500 patients in the `ADSL` dataset.
#'
#' @format a data frame with 604 rows (events) and 6 variables:
#' \describe{
#'   \item{ID}{patient identifiers, numeric}
#'   \item{AVAL}{occurence time of the event, numeric}
#'   \item{PARAM}{name of the event, character}
#'   \item{PARAMCD}{coded name of the event, character}
#'   \item{PARAMN}{type of the event, outcomes 1-7, where a higher value means a better outcome, numeric}
#'   \item{TRTPN}{treatment values, 1 Active or 2 Placebo, numeric}
#' }
#' @source
#' Heerspink HL et al "Development and validation of a new hierarchical composite endpoint for clinical trials of kidney disease progression." Journal of the American Society of Nephrology (2023). 
#' @examples
#' head(ADET)
#' # Number of unique patients
#' length(unique(ADET$ID)) 
#' # Number of events per event type
#' barplot(table(ADET$PARAM))
"ADET"



#' Laboratory dataset for Glomerular Filtration Rate (GFR) measurements.
#'
#' A dataset of laboratory measurements of kidney function over time for the 1500 patients in the `ADSL` dataset.
#'
#' @format a data frame with 13980 rows and 8 variables:
#' \describe{
#'   \item{ID}{patient identifiers, numeric}
#'   \item{TRTPN}{treatment values, 1 Active or 2 Placebo, numeric}
#'   \item{AVAL}{measurement value, numeric}
#'   \item{ADAY}{measurement day in the study, numeric}
#'   \item{AVISITN}{hospital visit number, numeric}
#'   \item{PARAM}{name of the event, GFR measurments, character}
#'   \item{PARAMCD}{coded name of the event, GFR, character}
#'   \item{PARAMN}{type of the event is set to 7 for all measurements, numeric}
#' }
#' @source 
#' Heerspink HL et al "Development and validation of a new hierarchical composite endpoint for clinical trials of kidney disease progression." Journal of the American Society of Nephrology (2023).
#' @examples
#' head(ADLB)
"ADLB"



#' Baseline characteristics dataset of patients with kidney function assessments.
#'
#' A data frame with baseline characteristics for 1500 patients used to derive `KHCE` dataset.
#'
#' @format a data frame with 1500 rows and 4 variables:
#' \describe{
#'   \item{ID}{patient identifiers, numeric}
#'   \item{TRTPN}{treatment values, 1 Active or 2 Placebo, numeric}
#'   \item{EGFRBL}{Baseline GFR values of patients, numeric}
#'   \item{STRATAN}{strata 1-4, higher value means a higher risk for kidney disease progression, numeric}
#' }
#' @source 
#' Heerspink HL et al "Development and validation of a new hierarchical composite endpoint for clinical trials of kidney disease progression." Journal of the American Society of Nephrology (2023).
#' @examples
#' head(ADSL)
"ADSL"