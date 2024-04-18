# Single-trait GBLUP ----

# Objective ----
# - Run Single-trait GBLUP for each scenario
# - Get GEBVs
# - Calculate reliability of each model

rm(list=objects()) # clean workspace

# Packages ----
library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package

# Load data ----
## BLUES & K2 ----
load('data/step2-subBLUES_K2.RData')

# Set parallel ----
library(doParallel)
library(foreach)

# Set the number of cores to use in parallel
num_cores <- detectCores()

# Register the parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

clusterExport(cl, c(ls(), 'asreml', 'update.asreml','predict.asreml'))

# ST-GBLUP ----

## 2022 ----
bluesG_22 |> glimpse()
K_22 |> glimpse()


# Initialize a list to store the results
GEBVs_list <- foreach(trait = c('grain_yield', 'test_weight', 'heading_time', 'plant_height'), .combine = c) %dopar% {
  # Load required packages within the parallel loop
  library(asreml)
  
  # Run Single-trait GBLUP for the current trait
  mod <- asreml(fixed=predicted.value~1,
                random=~vm(germplasm, K_22)+trial,
                weights=weight,
                family=asr_gaussian(dispersion = 1),
                data=bluesG_22 |> filter(trait == trait), 
                na.action=na.method(y='include', x='include'),
                workspace='8gb', maxit=20)
  mod <- update.asreml(mod)
  mod <- update.asreml(mod)
  
  # Calculate GEBVs for the current trait
  GEBVs <- predict(mod, classify='germplasm', pworkspace='8gb')$pvals |>
    as.data.frame() |>
    mutate(m_rel <- predict(mod, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
             as.data.frame() |>
             mutate(pev = std.error^2,
                    Vg = summary(mod)$varcomp['vm(germplasm, K_22)','component'],
                    rel = 1-(pev/Vg),
                    m_rel=mean(rel)) |>
             select(germplasm, m_rel))
  
  # Return the results for the current trait
  return(list(trait = trait, GEBVs = GEBVs))
}

# Stop the parallel backend
stopCluster(cl)

# Convert the results to a data frame
GEBVs_df <- do.call(rbind, GEBVs_list)

save.image('data/troubleshooting2.RData')