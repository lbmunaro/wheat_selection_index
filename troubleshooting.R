# Troubleshooting ----

# Objective ----
# - Solve the fact MT-BLUP is not predicting all traits

rm(list=objects()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(rrBLUP)    # R package for Ridge Regression and Best Linear Unbiased Prediction
library(Matrix)    # R package for sparse and dense matrix classes and methods
library(ASRgenomics) # R package for Genomic Selection Analysis in R
library(asreml) # ASReml-R package

# Load data ----
## BLUES ----
load("data/step1-BLUES.RData")

## Genotypic data ----
geno <- read.csv("data/geno2023.csv", header = T) |>
  rename(germplasm = X)

# Subset data ----

## BLUES ----
## Subset blues with genotypic information

### 2022 & 2023 ----
bluesG_22.23 <- blues |>
  ungroup() |>
  inner_join(geno|>dplyr::select(germplasm), by="germplasm") |>
  filter(trial%in%c("YT_Urb_22", "YT_Neo_22", "YT_Neo_22", "YT_Stp_22", "YT_Addie_22", "YT_Blv_22")) |>
  mutate_if(is.character, as.factor) |>
  droplevels() |>
  glimpse()

### 2022 ----
bluesG_22 <- bluesG_22.23 |>
  filter(grepl("_22",trial)) |>
  droplevels() |>
  glimpse()

## Genotypic data ----
## Subset genotypic data with phenotypic information or recent lines
### 2022 ----
geno_22 <- geno |>
  filter(germplasm %in% unique(bluesG_22$germplasm)) |>
  column_to_rownames("germplasm") |>
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))


colNaN <- colnames(geno_22)[colSums(is.na(geno_22)) > 0]
colNaN

geno_22 <- geno_22 |>
  select(!all_of(colNaN))

unique(as.vector(as.matrix(geno_22)))

## 2022 ----
K <- geno_22 |>
  as.matrix() |> # convert to matrix
  A.mat() |> # relationship matrix
  nearPD() # make positive semi definite
K_22 <- K$mat # get only the matrix

# Check ----
# Check if relationship matrix and phenotypic data matches
# 2022
check_22 <- match.kinship2pheno(K=as.matrix(K_22), pheno.data=bluesG_22,
                                indiv = "germplasm", 
                                clean=F, mism=T)
check_22$mismatchesP # Indiv with pheno but not geno

#MT-GBLUP
## 2022 ----
bluesG_22 <- bluesG_22 |> 
  filter(trait!="maturity") |> 
  droplevels() |>
  glimpse()
#K_22 |> glimpse()

#model
asreml.options(maxit= 150, ai.sing = TRUE)
mod_MT_GBLUP_22 <- asreml(fixed = predicted.value~trait,
                          random = ~corgh(trait):vm(germplasm, K_22) + at(trait):trial,
                          weights = weight, data = bluesG_22,
                          family=asr_gaussian(dispersion = 1),
                          na.action = na.method(y='include', x='include'),
                          workspace = "32gb")
#mod_MT_GBLUP_22 <- mkConv(mod_MT_GBLUP_22)
GEBVs_MT_GBLUP_22 <- predict(mod_MT_GBLUP_22, classify='germplasm:trait', ignore=c('trial'),
                             pworkspace='16gb')$pvals

save.image("data/troubleshooting.RData")
#load("data/troubleshooting.RData")
summary(mod_MT_GBLUP_22)
wald(mod_MT_GBLUP_22)
summary(mod_MT_GBLUP_22)$fixeff
#load("data/troubleshooting.RData")
#View(summary(mod_MT_GBLUP_22, coef = TRUE)$coef.random)

dif <- gebvs |>
  rename(gebvs = predicted.value,
         se = std.error) |>
  left_join(GEBVs_MT_GBLUP_22) |>
  mutate(gebv_dif=gebvs-predicted.value,
         se_dif=se-std.error) |>
  glimpse()

summary(dif)

tbsmod$call

tbsmod <- mod_MT_GBLUP_22

tbsmod$loglik
load("data/step4-MT-GBLUP.RData")

mod_MT_GBLUP_22$call
mod_MT_GBLUP_22$loglik
