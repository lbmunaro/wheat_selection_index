# Multi-trait GBLUP group ----

# Objective ----

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
  mutate(group=as.factor(paste(trait,trial,sep = "_"))) |>
#  arrange(germplasm, trial, trait) |>
  glimpse()
K_22 |> glimpse()

#model
mod_MT_GBLUP_22_group <- asreml(fixed = predicted.value~group,
                                random = ~fa(group,3):vm(germplasm, K_22),
                                weights = weight, data = bluesG_22,
                                family=asr_gaussian(dispersion = 1),
                                na.action = na.method(y='include', x='include'),
                                workspace = '32gb', maxit = 50)
mod_MT_GBLUP_22_group <- update.asreml(mod_MT_GBLUP_22_group)
#GEBVs_MT_GBLUP_22 <- predict(mod_MT_GBLUP_22, classify='germplasm:group', pworkspace='16gb')$pvals

save.image('data/troubleshooting2.RData')