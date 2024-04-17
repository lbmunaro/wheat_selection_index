# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP using the combination of trait and trial as groups
# - Get GEBVs

rm(list=ls()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package

# Load data ----
## BLUES & K2 ----
load('data/step4.1-MT-GBLUP_group_fa3.RData')

# MT-GBLUP ----

groups <- summary(mod_MT_GBLUP_22.23_group)$varcomp |>
  as.data.frame() |>
  rownames_to_column("source") |>
  dplyr:filter(str_detect(source,'var')) |>
  mutate(source=str_extract(source,'(?<=!)[^!]+(?=!)')) |>
  rownames_to_column('group') |>
  glimpse()

group_levels <- groups$source

# Initialize a list to store the results
GEBVs_df <- data.frame(group = character(), GEBVs = list())

# Loop through each group level
for (level in group_levels) {
  # Run the model
  GEBVs <- predict(mod_MT_GBLUP_22.23_group,
                   pworkspace = '8gb',
                   classify = 'germplasm:group',
                   levels = list(group = level))$pvals
  
  # Append the results to the data frame
  GEBVs_df <- bind_rows(GEBVs_df, data.frame(group = level, GEBVs = list(GEBVs)))
}

# Save the results
saveRDS(GEBVs_df, file = 'data/GEBVs-MT-GBLUP_group_fa3.RDS')
save.image('data/step4.2-MT-GBLUP-GEBVs_group_fa3.RData')