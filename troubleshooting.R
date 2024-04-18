# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP using the combination of trait and trial as groups
# - Get GEBVs

rm(list=ls()) # clean workspace

# Packages ----

library(dplyr)
library(tibble)
library(stringr)
library(asreml) # ASReml-R package

# Load data ----
## BLUES & K2 ----
load('data/mod_MT_GBLUP_22.23_group.RData')

# MT-GBLUP ----

groups <- summary(mod_MT_GBLUP_22.23_group)$varcomp |>
  as.data.frame() |>
  rownames_to_column("source") |>
  filter(str_detect(source,'var')) |>
  mutate(source=str_extract(source,'(?<=!)[^!]+(?=!)')) |>
  rownames_to_column('group') |>
  glimpse()

GEBVs <- predict(mod_MT_GBLUP_22.23_group,
                 pworkspace = '16gb',
                 classify = 'germplasm:group')$pvals


save.image('data/troubleshooting.RData')
