# Troubleshooting ----

# Objective ----
# - Solve the fact MT-BLUP is not predicting all traits

rm(list=objects()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package
library(datawizard) # Easy Data Wrangling and Statistical Transformations

# Load data ----
## BLUES & K2 ----
load('data/step2-subBLUES_K2.RData')

# MT-GBLUP ----

## 2022 & 2023 ----
bluesG_22.23 <- bluesG_22.23 |> 
  filter(trait!='maturity') |> 
  droplevels() |>
  glimpse()
K_22.23 |> glimpse()

#model
mod_MT_GBLUP_22.23 <- asreml(fixed = predicted.value~trait,
                             random = ~corgh(trait):vm(germplasm, K_22.23) + 
                               us(trait):trial,
                             weights = weight, data = bluesG_22.23,
                             family=asr_gaussian(dispersion = 1),
                             na.action = na.method(y='include', x='include'),
                             workspace = '32gb', maxit = 50)
mod_MT_GBLUP_22.23 <- update(mod_MT_GBLUP_22.23)
GEBVs_MT_GBLUP_22.23 <- predict(mod_MT_GBLUP_22.23, classify='germplasm:trait', ignore=c('trial'),
                                pworkspace='16gb')$pvals

## 2022 & 2023 ----
bluesG_22.23_scaled <- bluesG_22.23 |> 
  mutate(predicted.value=as.numeric(normalize(predicted.value))) |>
  glimpse()


#model
mod_MT_GBLUP_22.23_scaled <- asreml(fixed = predicted.value~trait,
                                    random = ~corgh(trait):vm(germplasm, K_22.23) + trial,
                                    weights = weight, data = bluesG_22.23_scaled,
                                    family=asr_gaussian(dispersion = 1),
                                    na.action = na.method(y='include', x='include'),
                                    workspace = '32gb', maxit = 50)
mod_MT_GBLUP_22.23_scaled <- update(mod_MT_GBLUP_22.23_scaled)
GEBVs_MT_GBLUP_22.23_scaled <- predict(mod_MT_GBLUP_22.23_scaled, classify='germplasm:trait', ignore=c('trial'),
                                pworkspace='16gb')$pvals


save('data/troubleshooting.RData')
