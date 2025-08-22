## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
R <- function() knitr::include_graphics("Rlogo.png", dpi = 5000)

## ----echo=FALSE, out.width = '33%'--------------------------------------------
knitr::include_graphics("hex-hce.png")

## ----eval = TRUE--------------------------------------------------------------
library(hce)
packageVersion("hce")

## ----eval = FALSE, include = FALSE--------------------------------------------
# devtools::load_all()

## -----------------------------------------------------------------------------
ls("package:hce")

## -----------------------------------------------------------------------------
args("hce")

## -----------------------------------------------------------------------------
set.seed(2022)
n <- 100
dat <- hce(GROUP = rep(x = c("I", "II", "III"), each = 100), 
           TRTP = sample(x = c("Active", "Control"), size = n*3, replace = TRUE))
class(dat)

## -----------------------------------------------------------------------------
head(dat)

## -----------------------------------------------------------------------------
unique(dat[, c("GROUP", "PARAMN")])

## -----------------------------------------------------------------------------
set.seed(2022)
n <- 100
GROUP = rep(x = c("I", "II", "III"), each = 100) 
GROUP <- factor(GROUP, levels = c("III", "II", "I"))
dat <- hce(GROUP = GROUP, 
           TRTP = sample(x = c("Active", "Control"), size = n*3, replace = TRUE))
unique(dat[, c("GROUP", "PARAMN")])

## -----------------------------------------------------------------------------
data(HCE1)
unique(HCE1$GROUP)

## -----------------------------------------------------------------------------
HCE <- hce(GROUP = factor(HCE1$GROUP, levels = c("TTE1", "TTE2", "TTE3", "TTE4", "C")), 
           TRTP = HCE1$TRTP, AVAL0 = HCE1$AVAL0, PADY = 1080)
class(HCE)
head(HCE)

## -----------------------------------------------------------------------------
data(HCE1, package = "hce")
class(HCE1)
head(HCE1)

## -----------------------------------------------------------------------------
dat1 <- as_hce(HCE2)
str(dat1)

## -----------------------------------------------------------------------------
args("simHCE")

## -----------------------------------------------------------------------------
Rates_A <- c(1.72, 1.74, 0.58, 1.5, 1) 
Rates_P <- c(2.47, 2.24, 2.9, 4, 6) 
dat3 <- simHCE(n = 2500, n0 = 1500, TTE_A = Rates_A, 
               TTE_P = Rates_P, 
               CM_A = -3, CM_P = -6, 
               CSD_A = 16, CSD_P = 15, 
               fixedfy = 3, seed = 2023)

## -----------------------------------------------------------------------------
class(dat3)
head(dat3)

## -----------------------------------------------------------------------------
methods(class = "hce")

## -----------------------------------------------------------------------------
HCE <- hce(GROUP = factor(HCE3$GROUP, levels = c("TTE1", "TTE2", "TTE3", "TTE4", "C")), 
           TRTP = HCE3$TRTP, AVAL0 = HCE3$AVAL0, PADY = 1080)
calcWO(HCE)
calcWINS(HCE)  
HCE$TRTP <- factor(HCE$TRTP, levels = c("P", "A"))
plot(HCE, fill = TRUE, col = "#865A4F", type = 'l', lwd = 2)
abline(a = 0, b = 1, lwd = 2, col = "#999999", lty = 2)

## -----------------------------------------------------------------------------
methods(class = "adhce")

## -----------------------------------------------------------------------------
summaryWO(HCE)

