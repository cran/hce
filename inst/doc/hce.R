## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, out.width = '33%'--------------------------------------------
knitr::include_graphics("hex-hce.png")

## ----setup--------------------------------------------------------------------
library(hce)
packageVersion("hce")

## ----echo=FALSE---------------------------------------------------------------
HCE <- data.frame(Order = c("I", "II", "III", "IV", "V"), 
                  Category = c("Death", 
                               "More than one new or worsened organ dysfunction events", 
                               "One new or worsened organ dysfunction event", 
                               "Hospitalized at the end of follow-up (Day 30)", 
                               "Discharged from hospital before Day 30")
)
HCE

## -----------------------------------------------------------------------------
table(COVID19)

## -----------------------------------------------------------------------------
COVID19HCE <- hce(GROUP = COVID19$GROUP, TRTP = COVID19$TRTP)
SUM <- summaryWO(COVID19HCE, ref = "Placebo")$summary
SUM$Ptie <- round(SUM$TIE/SUM$TOTAL, 2)
SUM

## ----echo=FALSE---------------------------------------------------------------
HCE2 <- data.frame(Order = c("I", "II", "III", "IV", "V", "VI", "VII"), 
                  Category = c("Death", 
                               "Dialysis or kidney transplantation", 
                               "Sustained GFR < 15 ml/min per 1.73 m2", 
                               "Sustained GFR decline from baseline of >= 57%", 
                               "Sustained GFR decline from baseline of >= 50%",
                               "Sustained GFR decline from baseline of >= 40%",
                               "Individual GFR slope")
)
HCE2

## -----------------------------------------------------------------------------
dat <- KHCE
Order <- c("Death (adj)", "Chronic dialysis (adj) >=90 days", 
           "Sustained eGFR<15 (mL/min/1.73 m2)", "Sustained >=57% decline in eGFR", 
           "Sustained >=50% decline in eGFR", "Sustained >=40% decline in eGFR", "eGFR slope")   
dat$GROUP <- factor(dat$GROUP, levels = Order)
table(dat$GROUP, dat$TRTP)

## ----echo=FALSE---------------------------------------------------------------
HCE3 <- data.frame(Order = c("I", "II", "III", "IV"), 
                  Category = c("Cardiovascular death", 
                               "Total (first and recurrent) HF hospitalizations", 
                               "Total urgent HF visits", 
                               "Improvement/deterioration in KCCQ-TSS"))
HCE3

## -----------------------------------------------------------------------------
Rates_A <- c(10, 20)
Rates_P <- c(20, 20)
dat1 <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P, 
CM_A = -3, CM_P = -6, CSD_A = 15, fixedfy = 3, theta = 1, seed = 1)
dat2 <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P, 
CM_A = -3, CM_P = -6, CSD_A = 15, fixedfy = 3, theta = 1.0001, seed = 1)
dat3 <- simHCE(n = 2500, TTE_A = Rates_A, TTE_P = Rates_P, 
CM_A = -3, CM_P = -6, CSD_A = 15, fixedfy = 3, theta = 10, seed = 1)
calcWO(dat1)
calcWO(dat2)
calcWO(dat3)

