# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP using the combination of trait and trial as groups
# - Get GEBVs

rm(list=ls()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package
library(doParallel) # Parallel computing with doParallel

# Load data ----
## BLUES & K2 ----
load('data/step2-subBLUES_K2.RData')

# Set up parallel processing
num_cores <- 40
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# MT-GBLUP ----

## 2022 & 2023 ----
bluesG_22.23 <- bluesG_22.23 %>%
  filter(trait != 'maturity') %>%
  droplevels() %>%
  mutate(group = as.factor(paste(trait, trial, sep = "_"))) 

#model
mod_MT_GBLUP_22.23_group <- asreml(fixed = predicted.value ~ group,
                                   random = ~ fa(group, 2):vm(germplasm, K_22.23),
                                   weights = weight, data = bluesG_22.23,
                                   family = asr_gaussian(dispersion = 1),
                                   na.action = na.method(y = 'include', x = 'include'),
                                   workspace = '96gb', maxit = 20)
mod_MT_GBLUP_22.23_group <- update.asreml(mod_MT_GBLUP_22.23_group)
mod_MT_GBLUP_22.23_group <- update.asreml(mod_MT_GBLUP_22.23_group)
mod_MT_GBLUP_22.23_group <- update.asreml(mod_MT_GBLUP_22.23_group)

# Save ----
# Save the model
saveRDS(mod_MT_GBLUP_22.23_group, file = 'data/mod_MT_GBLUP_22.23_group_fa2.RDS')

save.image('data/mod_MT_GBLUP_22.23_group_fa2.RData')

# Clean up
stopCluster(cl) # Stop parallel processing