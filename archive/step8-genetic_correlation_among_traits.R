# Genetic correlation among traits ----

# Objective ----
# - Calculate the genetic correlation among traits

rm(list=objects()) # clean workspace

# Packages ----
library(tidyverse) # R packages for data science

# Load data ----
# MT-GBLUP models
#load('data/step2-subBLUES_K2.RData')
load('data/step4-MT-GBLUP.RData')

# Extract traits
traits <- as.vector(unique(GEBVs_MT_GBLUP_23$trait)) 

# Convert variance components into a matrix
mat_varcomp <- 
  as.data.frame(summary(mod_MT_GBLUP_23)$varcomp) |> # Get variance components
  dplyr::select(component) |>  # Select the column containing the variance components
  rownames_to_column('varcomp') |>  # Convert row names to a separate column
  filter(str_detect(varcomp, paste(traits, collapse = '|'))) |>  # Filter rows that contain variables
  mutate(varcomp=str_replace_all(varcomp,'.*!trait_','')) |>  # Replace specific patterns
  separate(varcomp, c('var1','var2'),sep = ':') |>  # Split the 'varcomp' column into two separate columns
  spread(var2,component) |>  # Spread the 'component' values into separate columns
  column_to_rownames('var1') |>  # Use 'var1' column as row names
  as.matrix() |>  # Convert the resulting data frame to a matrix
  t()  # Transpose the matrix

mat_varcomp

# Function to calculate genetic correlations from the matrix of variance components

calc_gen_corr <- function(mat) {
  # Get the number of rows in the matrix
  n <- nrow(mat)  
  # Create a square matrix to store the genetic correlations, initialized with 1's
  genetic_corr_mat <- matrix(1, nrow = n, ncol = n,  
                             dimnames = list(rownames(mat), colnames(mat)))  # Set the row and column names
  
  for (i in 1:(n - 1)) {  # Loop over the rows of the matrix (excluding the last row)
    for (j in (i + 1):n) {  # Loop over the columns of the matrix starting from i+1 to n
      
      genetic_corr <- mat[i, j] / (sqrt(mat[i, i]) * sqrt(mat[j, j]))  # Calculate the genetic correlation
      # Assign the calculated genetic correlation to the corresponding cell in the upper triangle of the matrix
      genetic_corr_mat[i, j] <- genetic_corr
      # Assign the same value to the corresponding cell in the lower triangle of the matrix
      genetic_corr_mat[j, i] <- genetic_corr
    }
  }
  return(genetic_corr_mat)  # Return the matrix containing the genetic correlations
}

# Calculate genetic correlations
genetic_corr <- calc_gen_corr(mat_varcomp)  # Call the function with the input matrix
genetic_corr

summary(mod_MT_GBLUP_23)$varcomp

vpredict(mod_MT_GBLUP_22.23, r_A ~V3/sqrt(V2*V4))


load('data/step2-subBLUES_K2.RData')
mod_MT_GBLUP_22.23_corgh <- asreml(fixed = predicted.value~trait,
                             random = ~corgh(trait):vm(germplasm, K_22.23) + trial,
                             weights = weight, data = bluesG_22.23,
                             family=asr_gaussian(dispersion = 1),
                             na.action = na.method(y='include', x='include'),
                             workspace = '32gb')
