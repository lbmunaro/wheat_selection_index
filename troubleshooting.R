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

## 2022 & 2023 ----
bluesG_22.23 <- bluesG_22.23 |> 
  filter(trait!='maturity') |> 
  droplevels() |>
  mutate(group=as.factor(paste(trait,trial,sep = "_"))) |>
  glimpse()
K_22.23 |> glimpse()

#model
mod_MT_GBLUP_22.23_group <- asreml(fixed = predicted.value~group,
                                   random = ~fa(group,3):vm(germplasm, K_22.23),
                                   weights = weight, data = bluesG_22.23,
                                   family=asr_gaussian(dispersion = 1),
                                   na.action = na.method(y='include', x='include'),
                                   workspace = '64gb', maxit = 20)
mod_MT_GBLUP_22.23_group <- update.asreml(mod_MT_GBLUP_22.23_group)

# Save ----
#rm(list=c(ls(pattern = '^bluesG_'), ls(pattern = '^K_'), ls(pattern = '^check_'),
#          'object_names', 'obj_name', 'save_plot', 'data'))
save.image('data/troubleshooting_group.RData')
#load('data/step4-MT-GBLUP_group.RData')
