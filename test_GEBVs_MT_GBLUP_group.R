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
load('data/mod_MT_GBLUP_22.23_group.RData')

# Set up parallel processing
num_cores <- 40
registerDoMC(num_cores)

# MT-GBLUP ----

summary(mod_MT_GBLUP_22.23_group)$varcomp |>
  as.data.frame() |>
  rownames_to_column()

mod_MT_GBLUP_22.23_group$call
## 2022 & 2023 ----

asreml.options(workspace='120gb')

GEBVs_MT_GBLUP_22.23_group <- predict(mod_MT_GBLUP_22.23_group, 
                                      pworkspace='16gb',
                                      classify='group:germplasm')$pvals

saveRDS(GEBVs_MT_GBLUP_22.23_group, file = 'data/GEBVs_MT_GBLUP_22.23_group.RDS')


#GEBVs_MT_GBLUP_22.23_group_yld <- predict(mod_MT_GBLUP_22.23_group, classify='germplasm',
#                                          pworkspace='16gb',
#                                         average = list(group = c(rep(1/10,10), rep(0,19))))$pvals

save.image('data/test_GEBVs_MT_GBLUP_group.RData')
#load('data/test_GEBVs_MT_GBLUP_group.RData')

# Clean up
registerDoSEQ() # Reset to sequential processing