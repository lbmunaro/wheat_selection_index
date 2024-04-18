# Genotypic data ----

# Objective ----
# - Subset phenotypic and genotypic data
# - Create relationship matrices


rm(list=objects()) # clean workspace

# Packages ----
library(tidyverse) # R packages for data science
library(rrBLUP)    # R package for Ridge Regression and Best Linear Unbiased Prediction
library(Matrix)    # R package for sparse and dense matrix classes and methods
library(ASRgenomics) # R package for Genomic Selection Analysis in R

# Load data ----
## BLUES ----
blues <- readRDS('data/single_trial_blues.RDS')

## Genotypic data ----
geno <- read.csv('data/geno2023.csv', header = T) |>
  column_to_rownames('X') |> # Transposing the genotype column to row names
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) # Replacing NA values with the mean of the respective column (marker average)

# Check for columns with NA, markers without information
colNaN <- colnames(geno)[colSums(is.na(geno)) > 0]
colNaN

geno <- geno |>
  select(!all_of(colnames(geno)[colSums(is.na(geno)) > 0])) |> # Removing markers (columns) from the dataset where all values are NA
  rownames_to_column('germplasm')

# Subset data ----

## BLUES ----
## Subset blues with genotypic information

### 2022 & 2023 ----
bluesG_22.23 <- blues |>
  ungroup() |>
  inner_join(geno|> select('germplasm')) |>
  mutate_if(is.character, as.factor) |>
  droplevels() |>
  glimpse()

### 2022 ----
bluesG_22 <- bluesG_22.23 |>
  filter(grepl('_22',trial)) |>
  droplevels() |>
  glimpse()

### 2023 ----
bluesG_23 <- bluesG_22.23 |>
  filter(grepl('_23',trial)) |>
  droplevels() |>
  glimpse()

## Genotypic data ----
## Subset genotypic data with phenotypic information or recent lines

### 2022 & 2023 ----
geno_22.23 <- geno |>
  filter(germplasm %in% unique(bluesG_22.23$germplasm)) |>
  column_to_rownames('germplasm')

### 2022 ----
geno_22 <- geno |>
  filter(germplasm %in% unique(bluesG_22$germplasm)) |>
  column_to_rownames('germplasm')

### 2023 ----
geno_23 <- geno |>
  filter(germplasm %in% unique(bluesG_23$germplasm)) |>
  column_to_rownames('germplasm')

# Relationship matrix ----
## 2022 & 2023 ----
K <- geno_22.23 |>
  as.matrix() |> # convert to matrix
  A.mat() |> # relationship matrix
  nearPD() # make positive semi definite
K_22.23 <- K$mat # get only the matrix

## 2022 ----
K <- geno_22 |>
  as.matrix() |> # convert to matrix
  A.mat() |> # relationship matrix
  nearPD() # make positive semi definite
K_22 <- K$mat # get only the matrix

## 2023 ----
K <- geno_23 |>
  as.matrix() |> # convert to matrix
  A.mat() |> # relationship matrix
  nearPD() # make positive semi definite
K_23 <- K$mat # get only the matrix

# Check ----
# Check if relationship matrix and phenotypic data matches
# 2022
check.K_22 <- match.kinship2pheno(K=as.matrix(K_22), pheno.data=bluesG_22,
                             indiv = 'germplasm', 
                             clean=F, mism=T)
check.K_22$mismatchesP # Indiv with pheno but not geno
#2023
check.K_23 <- match.kinship2pheno(K=as.matrix(K_23), pheno.data=bluesG_23,
                                indiv = 'germplasm', 
                                clean=F, mism=T)
check.K_23$mismatchesP # Indiv with pheno but not geno

#2022 & 2023
check.K_22.23 <- match.kinship2pheno(K=as.matrix(K_22.23), pheno.data=bluesG_22.23,
                                indiv = 'germplasm', 
                                clean=F, mism=T)
check.K_22.23$mismatchesP # Indiv with pheno but not geno

# Save ----
rm(list=c('blues', ls(pattern = 'geno'), ls(pattern = 'check.K'),  'K', 'colNaN'))

save.image('data/step2-subBLUES_K2.RData')