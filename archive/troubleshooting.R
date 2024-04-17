# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP using the combination of trait and trial as groups
# - Get GEBVs

rm(list=ls()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package
library(asremlPlus)
library(foreach)
library(doParallel)
#asreml.options(workspace='16gb')

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

group_levels <- groups$source

group_levels

## 2022 & 2023 ----
# Set the number of cores to use in parallel
num_cores <- detectCores()

# Register the parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Export necessary objects and functions to the parallel workers
clusterExport(cl, c('bluesG_22.23','K_22.23','mod_MT_GBLUP_22.23_group',
                    'group_levels',
                    'predict'))

# Split group_levels into chunks of 5
group_chunks <- split(group_levels, ceiling(seq_along(group_levels)/1))

# Initialize a list to store the results
GEBVs_list <- list()

# Loop through each chunk of group_levels
for (chunk in group_chunks) {
  # Run the parallel loop for the current chunk of group_levels
  chunk_results <- foreach(level = chunk, .combine = c) %dopar% {
    # Load required packages within the parallel loop
    library(asreml)
#    asreml.options(workspace='16gb')
    
    # Run the model and store the results in the list
    GEBVs <- predict(mod_MT_GBLUP_22.23_group,
                     pworkspace = '2gb',
                     classify = 'germplasm:group',
                     levels = list(group = level))$pvals
    
    # Return the results
    return(list(level = level, GEBVs = GEBVs))
  }
  
  # Append the results from the current chunk to the overall results list
  GEBVs_list <- c(GEBVs_list, chunk_results)
}


# Stop the parallel backend
stopCluster(cl)




#GEBVs_MT_GBLUP_22.23_group_fa3 <- predict(mod_MT_GBLUP_22.23_group, 
#                                          pworkspace='2gb',
#                                          classify='germplasm:group',
#                                          levels = list(group = 'grain_yield_YT_Addie_22'))$pvals

saveRDS(GEBVs_list_fa3, file = 'data/GEBVs_MT_GBLUP_22.23_group_fa3.RDS')

save.image('data/GEBVs_MT_GBLUP_group_fa3.RData')
#load('data/GEBVs_MT_GBLUP_group_fa3.RData')