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

# ST-GBLUP ----

## 2022 ----
bluesG_22 |> glimpse()
K_22 |> glimpse()

### Grain yield ----
# model
mod_gy_22 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_22)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22 |> filter(trait=='grain_yield'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_gy_22 <- predict(mod_gy_22, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_gy_22, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_gy_22)$varcomp['vm(germplasm, K_22)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Test weight ----
# model
mod_tw_22 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_22)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22 |> filter(trait=='test_weight'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_tw_22 <- predict(mod_tw_22, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_tw_22, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_tw_22)$varcomp['vm(germplasm, K_22)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Heading time ----
# model
mod_ht_22 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_22)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22 |> filter(trait=='heading_time'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_ht_22 <- predict(mod_ht_22, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_ht_22, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_ht_22)$varcomp['vm(germplasm, K_22)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Plant height ----
# model
mod_ph_22 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_22)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22 |> filter(trait=='plant_height'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_ph_22 <- predict(mod_ph_22, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_ph_22, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_ph_22)$varcomp['vm(germplasm, K_22)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### GEBVs ----
# 2022 GEBVs - single-trait GBLUP
GEBVs_ST_GBLUP_22 <- data.frame() |>
  bind_rows(GEBV_gy_22 |> mutate(trait='grain_yield'),
            GEBV_tw_22 |> mutate(trait='test_weight'),
            GEBV_ht_22 |> mutate(trait='heading_time'),
            GEBV_ph_22 |> mutate(trait='plant_height')) |>
  glimpse()
# remove individual GEBV files
rm('GEBV_gy_22', 'GEBV_tw_22', 'GEBV_ht_22', 'GEBV_ph_22')

## 2023 ----
bluesG_23 |> glimpse()
K_23 |> glimpse()

### Grain yield ----
# model
mod_gy_23 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_23)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_23 |> filter(trait=='grain_yield'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_gy_23 <- predict(mod_gy_23, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_gy_23, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_gy_23)$varcomp['vm(germplasm, K_23)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Test weight ----
# model
mod_tw_23 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_23)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_23 |> filter(trait=='test_weight'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_tw_23 <- predict(mod_tw_23, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_tw_23, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_tw_23)$varcomp['vm(germplasm, K_23)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Heading time ----
# model
mod_ht_23 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_23)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_23 |> filter(trait=='heading_time'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_ht_23 <- predict(mod_ht_23, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_ht_23, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_ht_23)$varcomp['vm(germplasm, K_23)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Plant height ----
# model
mod_ph_23 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_23)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_23 |> filter(trait=='plant_height'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_ph_23 <- predict(mod_ph_23, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_ph_23, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_ph_23)$varcomp['vm(germplasm, K_23)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### GEBVs ----
# 2023 GEBVs - single-trait GBLUP
GEBVs_ST_GBLUP_23 <- data.frame() |>
  bind_rows(GEBV_gy_23 |> mutate(trait='grain_yield'),
            GEBV_tw_23 |> mutate(trait='test_weight'),
            GEBV_ht_23 |> mutate(trait='heading_time'),
            GEBV_ph_23 |> mutate(trait='plant_height')) |>
  glimpse()
# remove individual GEBV files
rm('GEBV_gy_23', 'GEBV_tw_23', 'GEBV_ht_23', 'GEBV_ph_23')

## 2022 & 2023 ----
bluesG_22.23 |> glimpse()
K_22.23 |> glimpse()

### Grain yield ----
# model
mod_gy_22.23 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_22.23)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22.23 |> filter(trait=='grain_yield'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_gy_22.23 <- predict(mod_gy_22.23, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_gy_22.23, classify='germplasm', ignore=c('(Intercept)'),
                          pworkspace='8gb')$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_gy_22.23)$varcomp['vm(germplasm, K_22.23)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Test weight ----
# model
mod_tw_22.23 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_22.23)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22.23 |> filter(trait=='test_weight'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_tw_22.23 <- predict(mod_tw_22.23, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_tw_22.23, classify='germplasm', ignore=c('(Intercept)'),
                          pworkspace='8gb')$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_tw_22.23)$varcomp['vm(germplasm, K_22.23)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Heading time ----
# model
mod_ht_22.23 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_22.23)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22.23 |> filter(trait=='heading_time'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_ht_22.23 <- predict(mod_ht_22.23, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_ht_22.23, classify='germplasm', ignore=c('(Intercept)'),
                          pworkspace='8gb')$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_ht_22.23)$varcomp['vm(germplasm, K_22.23)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### Plant height ----
# model
mod_ph_22.23 <- asreml(fixed=predicted.value~1,
                    random=~vm(germplasm, K_22.23)+trial,
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22.23 |> filter(trait=='plant_height'), 
                    na.action = na.method(y='include', x='include'),
                    workspace='8gb', maxit= 50)
#gebvs
GEBV_ph_22.23 <- predict(mod_ph_22.23, classify='germplasm', pworkspace='8gb')$pvals |>
  as.data.frame() |>
  mutate(m_rel <- predict(mod_ph_22.23, classify='germplasm', ignore=c('(Intercept)'),
                          pworkspace='8gb')$pvals |>
           as.data.frame() |>
           mutate(pev = std.error^2,
                  Vg = summary(mod_ph_22.23)$varcomp['vm(germplasm, K_22.23)','component'],
                  rel = 1-(pev/Vg),
                  m_rel=mean(rel)) |>
           select(germplasm, m_rel))

### GEBVs ----
# 2023 GEBVs - single-trait GBLUP
GEBVs_ST_GBLUP_22.23 <- data.frame() |>
  bind_rows(GEBV_gy_22.23 |> mutate(trait='grain_yield'),
            GEBV_tw_22.23 |> mutate(trait='test_weight'),
            GEBV_ht_22.23 |> mutate(trait='heading_time'),
            GEBV_ph_22.23 |> mutate(trait='plant_height')) |>
  glimpse()
# remove individual GEBV files
rm('GEBV_gy_22.23', 'GEBV_tw_22.23', 'GEBV_ht_22.23', 'GEBV_ph_22.23')

# Save ----
rm(list=c(ls(pattern = '^bluesG_'), ls(pattern = '^K_'), ls(pattern = '^check_')))

save.image('data/step3-ST-GBLUP.RData')
