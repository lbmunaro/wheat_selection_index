# Title ----

# Objective ----

# Clean workspace & track time ----

#| include: false
rm(list=objects())
start.time <- Sys.time()

# Packages ----

#| message: false
#| warning: false
#| results: hide
library(tidyverse) # R packages for data science
library(asreml)

# Load data ----
## BLUES & K2 ----
load("data/step2-subBLUES_K2.RData")

# MT-GBLUP ----

## 2022 ----
bluesG_22 <- bluesG_22 |> 
  filter(trait!="maturity") |> 
  droplevels() |>
  glimpse()
K_22 |> glimpse()

#model
asreml.options(maxit= 150)
mod_MT_GBLUP_22 <- asreml(fixed = predicted.value~trait+trial:trait,
                          random = ~corgh(trait):vm(germplasm, K_22),
                          weights = weight,
                          family=asr_gaussian(dispersion = 1),
                          data = bluesG_22,
                          na.action = na.method(y='include', x='include'),
                          workspace = "32gb")
GEBVs_MT_GBLUP_22 <- predict(mod_MT_GBLUP_22, classify='trait:germplasm', pworkspace='16gb')$pvals

## 2023 ----
bluesG_23 <- bluesG_23 |> 
  filter(trait!="maturity") |> 
  droplevels() |>
  glimpse()
K_23 |> glimpse()

#model
asreml.options(maxit= 150)
mod_MT_GBLUP_23 <- asreml(fixed = predicted.value~trait+trial:trait,
                          random = ~corgh(trait):vm(germplasm, K_23),
                          weights = weight,
                          family=asr_gaussian(dispersion = 1),
                          data = bluesG_23,
                          na.action = na.method(y='include', x='include'),
                          workspace = "32gb")
GEBVs_MT_GBLUP_23 <- predict(mod_MT_GBLUP_23, classify='trait:germplasm', pworkspace='16gb')$pvals

## 2022 & 2023 ----
bluesG_22.23 <- bluesG_22.23 |> 
  filter(trait!="maturity") |> 
  droplevels() |>
  glimpse()
#K_22.23 |> glimpse()

#model
asreml.options(maxit= 150)
mod_MT_GBLUP_22.23 <- asreml(fixed = predicted.value~trait+trial:trait,
                          random = ~corgh(trait):vm(germplasm, K_22.23),
                          weights = weight,
                          family=asr_gaussian(dispersion = 1),
                          data = bluesG_22.23,
                          na.action = na.method(y='include', x='include'),
                          workspace = "32gb")
GEBVs_MT_GBLUP_22.23 <- predict(mod_MT_GBLUP_22.23, classify='trait:germplasm', pworkspace='16gb')$pvals

# END ----
time <- Sys.time() - start.time
time

# Save ----
save.image("data/step4-MT-GBLUP.RData")
