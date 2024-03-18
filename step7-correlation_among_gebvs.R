# Correlation among trait GEBVs ----

# Objective ----
# - Calculate the correlation among trait gebvs

rm(list=objects()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science

# Load data ----
## GEBVs & Net merit ----
NET_MERIT <- read.csv("data/net_merit.csv") |>
  mutate_if(is.character, as.factor) |>
  glimpse()

calculate_correlation <- function(data, vars) {
  correlation_matrix <- data |>
    select(all_of(vars)) |>
    cor()
  
  return(correlation_matrix)
}

vars <- c("grain_yield", "test_weight", "heading_time", "plant_height", "net_merit")

# MT-GBLUP
MT_GBLUP <- NET_MERIT |>
  filter(scenario%in%"MT_GBLUP_22.23") |>
  glimpse()

# Calculate correlation
correlation_result_MT_BLUP <- calculate_correlation(MT_GBLUP, vars)
correlation_result_MT_BLUP

#ST-GBLUP
ST_GBLUP <- NET_MERIT |>
  filter(scenario%in%"ST_GBLUP_22.23") |>
  glimpse()

# Calculate correlation
correlation_result_ST_BLUP <- calculate_correlation(ST_GBLUP, vars)
correlation_result_ST_BLUP

# GPT ----
# Function to merge upper and lower triangular parts from two correlation matrices
merge_correlation_matrices <- function(cor1, cor2) {
  # Create an empty matrix with dimension 5x5
  merged_matrix <- matrix(NA, nrow = 5, ncol = 5)
  
  # Fill the upper triangular part with values from cor1
  merged_matrix[upper.tri(merged_matrix)] <- cor1[upper.tri(cor1)]
  
  # Fill the lower triangular part with values from cor2
  merged_matrix[lower.tri(merged_matrix)] <- cor2[lower.tri(cor2)]
  
  # Fill the diagonal with 1s
  diag(merged_matrix) <- 1
  
  return(merged_matrix)
}

# Create the merged correlation matrix
merged_correlation <- merge_correlation_matrices(correlation_result_MT_BLUP, correlation_result_ST_BLUP)

# Print the merged correlation matrix
print(merged_correlation)

# Define trait names
trait_names <- c("Grain yield", "Test weight", "Heading time", "Plant height", "Net merit")

# Assign trait names to row and column names
dimnames(merged_correlation) <- list(trait_names, trait_names)

merged_correlation
# PLOT ----

library(corrplot)

corrplot.mixed(merged_correlation, upper="pie", lower="pie")

corrplot.mixed(merged_correlation, upper = "pie", lower = "pie",  bg = list(upper = "lightblue", lower = "pink"))

png("figures/correlation_gebvs.png", width = 6, height = 4, units = "in", res = 320)
corrplot.mixed(merged_correlation, upper = "pie", lower = "pie",
               number.cex = 1.2, number.digits = 2, addCoef.col = "gray50",
               tl.col = "black", tl.cex = 0.75,
               upper.col = "#13294B", lower.col = "#FF552E")
dev.off()
