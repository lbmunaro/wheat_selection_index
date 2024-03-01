# Title ----

# Objective ----

# Clean workspace & track time ----
rm(list=objects())
start.time <- Sys.time()

# Packages ----
library(tidyverse) # R packages for data science
library(rrBLUP)
library(Matrix)

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
  mutate_if(is.character, as.factor) |>
  droplevels() |>
  glimpse()

### 2022 ----
bluesG_22 <- bluesG_22.23 |>
  filter(grepl("_22",trial)) |>
  droplevels() |>
  glimpse()

### 2023 ----
bluesG_23 <- bluesG_22.23 |>
  filter(grepl("_23",trial)) |>
  droplevels() |>
  glimpse()

## Genotypic data ----
## Subset genotypic data with phenotypic information or recent lines

### 2022 & 2023 ----
geno_22.23 <- geno |>
  filter(germplasm %in% unique(bluesG_22.23$germplasm)) |>
  column_to_rownames("germplasm")

### 2022 ----
geno_22 <- geno |>
  filter(germplasm %in% unique(bluesG_22$germplasm)) |>
  column_to_rownames("germplasm")

### 2022 ----
geno_23 <- geno |>
  filter(germplasm %in% unique(bluesG_23$germplasm)) |>
  column_to_rownames("germplasm")

# Relationship matrix ----
## 2022 & 2023 ----
K <- geno_22.23 |>
  as.matrix() |> # convert to matrix
  A.mat() |> # relationship matrix
  nearPD() # make positive semi definite
K_22.23 <- K$mat # get only the matrix

## 2022 & 2023 ----
K <- geno_22 |>
  as.matrix() |> # convert to matrix
  A.mat() |> # relationship matrix
  nearPD() # make positive semi definite
K_22 <- K$mat # get only the matrix

## 2022 & 2023 ----
K <- geno_23 |>
  as.matrix() |> # convert to matrix
  A.mat() |> # relationship matrix
  nearPD() # make positive semi definite
K_23 <- K$mat # get only the matrix

# Check ----
# Check if relationship matrix and phenotypic data matches
#check <- match.kinship2pheno(K=as.matrix(K_22), pheno.data=bluesG_22,
#                             indiv = "germplasm", 
#                             clean=F, mism=T)
#check$mismatchesP # Indiv with pheno but not geno

# END ----
time <- Sys.time() - start.time
time

# Save ----
rm("blues", "geno", "geno_22.23", "geno_22", "geno_23",  "K", "start.time")

save.image("data/step2-subBLUES_K2.RData")