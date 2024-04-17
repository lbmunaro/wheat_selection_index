# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP for each scenario
# - Get GEBVs
# - Calculate reliability of each model

rm(list=objects()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package

# Load data ----
## BLUES & K2 ----
load('data/step2-subBLUES_K2.RData')

# MT-GBLUP ----

## 2022 ----
bluesG_22 <- bluesG_22 |> 
  filter(trait!='maturity') |> 
  droplevels() |>
  glimpse()
K_22 |> glimpse()

#model
mod_MT_GBLUP_22 <- asreml(fixed = predicted.value~trait,
                          random = ~corgh(trait):vm(germplasm, K_22) + trial,
                          weights = weight, asmv = trait, data = bluesG_22,
                          family=asr_gaussian(dispersion = 1),
                          na.action = na.method(y='include', x='include'),
                          workspace = '32gb')

GEBVs_MT_GBLUP_22 <- predict(mod_MT_GBLUP_22, classify='germplasm:trait',
                             pworkspace='16gb')$pvals

## 2023 ----
bluesG_23 <- bluesG_23 |> 
  filter(trait!='maturity') |> 
  droplevels() |>
  glimpse()
K_23 |> glimpse()

#model
mod_MT_GBLUP_23 <- asreml(fixed = predicted.value~trait,
                          random = ~corgh(trait):vm(germplasm, K_23) + trial,
                          weights = weight, asmv = trait, data = bluesG_23,
                          family=asr_gaussian(dispersion = 1),
                          na.action = na.method(y='include', x='include'),
                          workspace = '32gb')

GEBVs_MT_GBLUP_23 <- predict(mod_MT_GBLUP_23, classify='germplasm:trait',
                             pworkspace='16gb')$pvals

## 2022 & 2023 ----
bluesG_22.23 <- bluesG_22.23 |> 
  filter(trait!='maturity') |> 
  droplevels() |>
  glimpse()
K_22.23 |> glimpse()

#model
mod_MT_GBLUP_22.23 <- asreml(fixed = predicted.value~trait,
                             random = ~corgh(trait):vm(germplasm, K_22.23) + trial,
                             weights = weight, asmv = trait, data = bluesG_22.23,
                             family=asr_gaussian(dispersion = 1),
                             na.action = na.method(y='include', x='include'),
                             workspace = '32gb')

GEBVs_MT_GBLUP_22.23 <- predict(mod_MT_GBLUP_22.23, classify='germplasm:trait',
                                pworkspace='16gb')$pvals

# Save ----
#rm(list=c(ls(pattern = '^bluesG_'), ls(pattern = '^K_'), ls(pattern = '^check_')))
save.image('data/step4-MT-GBLUP.RData')
#load('data/step4-MT-GBLUP.RData')
