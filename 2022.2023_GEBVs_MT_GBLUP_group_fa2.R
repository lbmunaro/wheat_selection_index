# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP using the combination of trait and trial as groups
# - Get GEBVs

rm(list=ls()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package
library(doMC) # Parallel computing with doMC

# Load data ----
## BLUES & K2 ----
load('data/mod_MT_GBLUP_22.23_group_fa2.RData')

# Set up parallel processing
num_cores <- 40
registerDoMC(num_cores)

# MT-GBLUP ----

summary(mod_MT_GBLUP_22.23_group)$varcomp |>
  as.data.frame() |>
  rownames_to_column()

mod_MT_GBLUP_22.23_group$call
## 2022 & 2023 ----

GEBVs_MT_GBLUP_22.23_group <- predict(mod_MT_GBLUP_22.23_group, 
                                      pworkspace='4gb',
                                      classify='germplasm:group')$pvals

saveRDS(GEBVs_MT_GBLUP_22.23_group, file = 'data/GEBVs_MT_GBLUP_22.23_group_fa2.RDS')

save.image('data/test_GEBVs_MT_GBLUP_group_fa2.RData')
#load('data/test_GEBVs_MT_GBLUP_group.RData')

# Clean up
registerDoSEQ() # Reset to sequential processing