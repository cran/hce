## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(hce)
packageVersion("hce")

## ----echo=FALSE---------------------------------------------------------------
HCE <- data.frame(Order = c("I", "II", "III", "IV", "V"), 
                  Category = c("Death", "More than one new or worsened organ dysfunction events", 
                               "One new or worsened organ dysfunction event", 
                               "Hospitalized at the end of follow-up (Day 30)", 
                               "Discharged from hospital before Day 30")
)
HCE

