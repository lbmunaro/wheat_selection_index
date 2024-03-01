# Title ----

# Objective ----

# Clean workspace & track time ----
rm(list=objects())
start.time <- Sys.time()

# Packages ----
library(tidyverse) # R packages for data science
library(asreml)
asreml.options(maxit= 150)

# Load data ----
## BLUES & K2 ----
load("data/step2-subBLUES_K2.RData")

# ST-GBLUP ----

## 2022 ----
bluesG_22 |> glimpse()
K_22 |> glimpse()

### Grain yield ----
# model
mod_gy_22 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_22),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22 |> filter(trait=="grain_yield"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_gy_22 <- predict(mod_gy_22, classify='germplasm', pworkspace='8gb')$pvals

### Test weight ----
# model
mod_tw_22 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_22),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22 |> filter(trait=="test_weight"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_tw_22 <- predict(mod_tw_22, classify='germplasm', pworkspace='8gb')$pvals

### Heading time ----
# model
mod_ht_22 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_22),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22 |> filter(trait=="heading_time"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_ht_22 <- predict(mod_ht_22, classify='germplasm', pworkspace='8gb')$pvals

### Plant height ----
# model
mod_ph_22 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_22),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22 |> filter(trait=="plant_height"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_ph_22 <- predict(mod_ph_22, classify='germplasm', pworkspace='8gb')$pvals

### GEBVs ----
# 2022 GEBVs - single-trait GBLUP
GEBVs_ST_GBLUP_22 <- data.frame() |>
  bind_rows(GEBV_gy_22 |> mutate(trait="grain_yield"),
            GEBV_tw_22 |> mutate(trait="test_weight"),
            GEBV_ht_22 |> mutate(trait="heading_time"),
            GEBV_ph_22 |> mutate(trait="plant_height")) |>
  glimpse()
# remove individual GEBV files
rm("GEBV_gy_22", "GEBV_tw_22", "GEBV_ht_22", "GEBV_ph_22")

## 2023 ----
bluesG_23 |> glimpse()
K_23 |> glimpse()

### Grain yield ----
# model
mod_gy_23 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_23),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_23 |> filter(trait=="grain_yield"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_gy_23 <- predict(mod_gy_23, classify='germplasm', pworkspace='8gb')$pvals

### Test weight ----
# model
mod_tw_23 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_23),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_23 |> filter(trait=="test_weight"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_tw_23 <- predict(mod_tw_23, classify='germplasm', pworkspace='8gb')$pvals

### Heading time ----
# model
mod_ht_23 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_23),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_23 |> filter(trait=="heading_time"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_ht_23 <- predict(mod_ht_23, classify='germplasm', pworkspace='8gb')$pvals

### Plant height ----
# model
mod_ph_23 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_23),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_23 |> filter(trait=="plant_height"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_ph_23 <- predict(mod_ph_23, classify='germplasm', pworkspace='8gb')$pvals

### GEBVs ----
# 2023 GEBVs - single-trait GBLUP
GEBVs_ST_GBLUP_23 <- data.frame() |>
  bind_rows(GEBV_gy_23 |> mutate(trait="grain_yield"),
            GEBV_tw_23 |> mutate(trait="test_weight"),
            GEBV_ht_23 |> mutate(trait="heading_time"),
            GEBV_ph_23 |> mutate(trait="plant_height")) |>
  glimpse()
# remove individual GEBV files
rm("GEBV_gy_23", "GEBV_tw_23", "GEBV_ht_23", "GEBV_ph_23")

## 2022 & 2023 ----
bluesG_22.23 |> glimpse()
K_22.23 |> glimpse()

### Grain yield ----
# model
mod_gy_22.23 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_22.23),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22.23 |> filter(trait=="grain_yield"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_gy_22.23 <- predict(mod_gy_22.23, classify='germplasm', pworkspace='8gb')$pvals

### Test weight ----
# model
mod_tw_22.23 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_22.23),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22.23 |> filter(trait=="test_weight"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_tw_22.23 <- predict(mod_tw_22.23, classify='germplasm', pworkspace='8gb')$pvals

### Heading time ----
# model
mod_ht_22.23 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_22.23),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22.23 |> filter(trait=="heading_time"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_ht_22.23 <- predict(mod_ht_22.23, classify='germplasm', pworkspace='8gb')$pvals

### Plant height ----
# model
mod_ph_22.23 <- asreml(fixed=predicted.value~1,
                    random=~trial+vm(germplasm, K_22.23),
                    weights=weight,
                    family=asr_gaussian(dispersion = 1),
                    data=bluesG_22.23 |> filter(trait=="plant_height"), 
                    na.action = na.method(y='include', x='include'),
                    workspace="8gb")
#gebvs
GEBV_ph_22.23 <- predict(mod_ph_22.23, classify='germplasm', pworkspace='8gb')$pvals

### GEBVs ----
# 2023 GEBVs - single-trait GBLUP
GEBVs_ST_GBLUP_22.23 <- data.frame() |>
  bind_rows(GEBV_gy_22.23 |> mutate(trait="grain_yield"),
            GEBV_tw_22.23 |> mutate(trait="test_weight"),
            GEBV_ht_22.23 |> mutate(trait="heading_time"),
            GEBV_ph_22.23 |> mutate(trait="plant_height")) |>
  glimpse()
# remove individual GEBV files
rm("GEBV_gy_22.23", "GEBV_tw_22.23", "GEBV_ht_22.23", "GEBV_ph_22.23")

# Create a loop to save all models plots in a separate folder (ST-GBLUP mod) with file name referring to the model
object_names <- ls(pattern = "^mod_")

# Create a function to save plots
save_plot <- function(data, obj_name) {
  # Apply the plot() function
  plot(data)  # Adjust this line according to your plot function
  # Save the plot with the same name as the object in the specified folder
  plot_dir <- "~/Documents/wheat_selection_index/plot_mod/ST-GBLUP/"  # Update this path with the absolute path
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

# END ----
time <- Sys.time() - start.time
time

# Save ----
rm(list=c("start.time", "bluesG_22", "bluesG_23", "bluesG_22.23", "K_22", "K_23", "K_22.23",
          "object_names"))

save.image("data/step3-ST-GBLUP.RData")
