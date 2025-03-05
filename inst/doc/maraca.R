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

## ----eval=FALSE---------------------------------------------------------------
# library(maraca)
# Rates_A <- 10
# Rates_P <- 15
# dat <- simHCE(n = 1000, n0 = 500, TTE_A = Rates_A, TTE_P = Rates_P,
#               CM_A = 0.2, CM_P = 0, seed = 2, shape = 0.35)
# plot(dat)

## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics("maraca.png", dpi = 100)

