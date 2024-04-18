# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP using the combination of trait and trial as groups
# - Get GEBVs

rm(list=objects()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package

# Load data ----
## BLUES & K2 ----
load('data/step2-subBLUES_K2.RData')

# MT-GBLUP ----

#model
## 2023 ----
bluesG_23 <- bluesG_23 |> 
  filter(trait!='maturity') |> 
  droplevels() |>
  mutate(group=as.factor(paste(trait,trial,sep = "_"))) |>
  glimpse()
K_23 |> glimpse()

#model
mod_MT_GBLUP_23_group <- asreml(fixed = predicted.value~group,
                                random = ~fa(group,3):vm(germplasm, K_23),
                                weights = weight, data = bluesG_23,
                                family=asr_gaussian(dispersion = 1),
                                na.action = na.method(y='include', x='include'),
                                workspace = '32gb', maxit = 20)
mod_MT_GBLUP_23_group <- update.asreml(mod_MT_GBLUP_23_group)

# Save the model
saveRDS(mod_MT_GBLUP_23_group, file = 'data/mod_MT_GBLUP_23_group.RDS')

save.image('data/mod_MT_GBLUP_23_group.RData')

