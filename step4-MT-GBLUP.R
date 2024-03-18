# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP for each scenario
# - Get GEBVs
# - Calculate reliability of each model

rm(list=objects()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package
asreml.options(maxit= 150)

# Load data ----
## BLUES & K2 ----
load("data/step2-subBLUES_K2.RData")

# MT-GBLUP ----

## 2022 ----
bluesG_22 <- bluesG_22 |> 
  filter(trait!="maturity") |> 
  droplevels() |>
  glimpse()
K_22 |> glimpse()

#model
mod_MT_GBLUP_22 <- asreml(fixed = predicted.value~trait,
                          random = ~us(trait):vm(germplasm, K_22) + trial,
                          weights = weight, data = bluesG_22,
                          family=asr_gaussian(dispersion = 1),
                          na.action = na.method(y='include', x='include'),
                          workspace = "32gb")
GEBVs_MT_GBLUP_22 <- predict(mod_MT_GBLUP_22, classify='germplasm:trait', ignore=c('trial'),
                             pworkspace='16gb')$pvals

## 2023 ----
bluesG_23 <- bluesG_23 |> 
  filter(trait!="maturity") |> 
  droplevels() |>
  glimpse()
K_23 |> glimpse()

#model
mod_MT_GBLUP_23 <- asreml(fixed = predicted.value~trait,
                          random = ~us(trait):vm(germplasm, K_23) + trial,
                          weights = weight, data = bluesG_23,
                          family=asr_gaussian(dispersion = 1),
                          na.action = na.method(y='include', x='include'),
                          workspace = "32gb")
GEBVs_MT_GBLUP_23 <- predict(mod_MT_GBLUP_23, classify='germplasm:trait', ignore=c('trial'),
                             pworkspace='16gb')$pvals

## 2022 & 2023 ----
bluesG_22.23 <- bluesG_22.23 |> 
  filter(trait!="maturity") |> 
  droplevels() |>
  glimpse()
K_22.23 |> glimpse()

#model
mod_MT_GBLUP_22.23 <- asreml(fixed = predicted.value~trait,
                             random = ~us(trait):vm(germplasm, K_22.23) + trial,
                             weights = weight, data = bluesG_22.23,
                             family=asr_gaussian(dispersion = 1),
                             na.action = na.method(y='include', x='include'),
                             workspace = "32gb")
GEBVs_MT_GBLUP_22.23 <- predict(mod_MT_GBLUP_22.23, classify='germplasm:trait', ignore=c('trial'),
                                pworkspace='16gb')$pvals

# Plot models ----

# Create a loop to save all models plots in a separate folder (MT-GBLUP mod) with file name referring to the model
object_names <- ls(pattern = "^mod_")

# Create a function to save plots
save_plot <- function(data, obj_name) {
  # Apply the plot() function
  plot(data)  # Adjust this line according to your plot function
  # Save the plot with the same name as the object in the specified folder
  plot_dir <- "~/Documents/wheat_selection_index/plot_mod/MT-GBLUP/"  # Update this path with the absolute path
  # Create the directory if it doesn't exist
  if (!dir.exists(plot_dir)) {
    if (!dir.create(plot_dir, recursive = TRUE)) {
      stop("Failed to create directory:", plot_dir)
    }
  }
  plot_name <- paste0(plot_dir, obj_name, ".png")
  # Save the plot as a PNG file
  if (!try(png(plot_name), silent = TRUE)) {
    stop("Failed to save plot:", plot_name)
  }
  dev.off()
}

# Iterate over each object
for (obj_name in object_names) {
  # Extract the object
  data <- get(obj_name)
  # Save the plot
  try(save_plot(data, obj_name), silent = TRUE)
}

# Save ----
rm(list=c(ls(pattern = "^bluesG_"), ls(pattern = "^K_"), ls(pattern = "^check_"),
          "object_names", "obj_name", "save_plot", "data"))
save.image("data/step4-MT-GBLUP.RData")
#load("data/step4-MT-GBLUP.RData")
