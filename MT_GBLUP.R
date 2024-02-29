rm(list=objects()) # clean objects from workspace
start.time <- Sys.time()

# Load packages
#library(tidyverse)
library(asreml)

# Get directory
getwd()

#Set directory
#setwd("/home/lucasb4/Documents/wheat_selection_index")

# Load phenotypic and genotypic data
load("data/blues_K.RData")
ls()

# Data
#blues_geno |> glimpse()

#MT-GBLUP model
asreml.options(maxit= 150)
mod_MT.GBLUP <- asreml(fixed = predicted.value~trait,
                       random = ~diag(trait):vm(germplasm_name, K2),
                       weights = weight,
                       family=asr_gaussian(dispersion = 1),
                       na.action = na.method(y='include', x='include'),
                       data = blues_geno,
                       workspace = "32gb")

save.image("data/MT_GBLUP.RData")

#gebvs <- predict(mod_MT.GBLUP, classify='germplasm_name:trait', pworkspace='8gb')$pvals
gebvs <- predict(mod_MT.GBLUP, classify='trait:germplasm_name', pworkspace='8gb')$pvals

pred <-  predict(mod_MT.GBLUP, classify = "germplasm_name", pworkspace = "8gb")

save.image("data/MT_GBLUP.RData")

time <- Sys.time() - start.time
time