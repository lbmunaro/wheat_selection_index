# Single-trait GBLUP ----

# Objective ----
# - Run Single-trait GBLUP for each scenario
# - Get GEBVs
# - Calculate reliability of each model

rm(list=ls()) # Clean workspace

# Packages ----
library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package

# Load data ----
## BLUES & K2 ----
load('data/step2-subBLUES_K2.RData')

# Set the list of traits
traits <- c('grain_yield', 'test_weight', 'heading_time', 'plant_height')

# Create an empty list to store the models and GEBVs
models <- list()
GEBVs <- list()

# Loop through each trait
for (trait in traits) {
  # Run the code for the current trait
  
  ### Grain yield ----
  # model
  mod <- asreml(fixed = predicted.value ~ 1,
                random = ~vm(germplasm, K_22) + trial,
                weights = weight,
                family = asr_gaussian(dispersion = 1),
                data = bluesG_22 |> filter(trait == 'grain_yield'), 
                na.action = na.method(y = 'include', x = 'include'),
                workspace = '8gb', maxit = 20)
  mod <- update.asreml(mod)
  mod <- update.asreml(mod)
  
  # Save the model
  models[[trait]] <- mod
  
  #gebvs
  GEBV <- predict(mod, classify = 'germplasm', pworkspace = '8gb')$pvals |>
    as.data.frame() |>
    mutate(m_rel <- predict(mod, classify = 'germplasm', ignore = c('(Intercept)'))$pvals |>
             as.data.frame() |>
             mutate(pev = std.error^2,
                    Vg = summary(mod)$varcomp['vm(germplasm, K_22)', 'component'],
                    rel = 1 - (pev / Vg),
                    m_rel = mean(rel)) |>
             select(germplasm, m_rel))
  
  # Save the GEBV
  GEBVs[[trait]] <- GEBV
  
  ### Save the results or perform further analysis
  
  # Remove unnecessary objects to free up memory
  rm(GEBV)
}

# Save the models and GEBVs
save(models, GEBVs, file = "models_and_GEBVs.RData")


save.image('data/troubleshooting2.RData')

load('data/troubleshooting2.RData')

GEBVs |>
  unlist() |>
  glimpse()

models
