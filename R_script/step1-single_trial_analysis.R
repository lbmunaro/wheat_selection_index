# Title ----

# Objective ----

# Clean workspace & track time ----
rm(list=objects())
start.time <- Sys.time()

# Packages ----
library(tidyverse) # R packages for data science
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(asreml) # ASReml-R package

# Phenotypic data ----

## Load and organize ----

### Wide format ----
pheno_raw_w <- #raw data wide format
  read.csv('data/IL_22-23YT_02.27.24.csv', skip = 3) |> 
  remove_empty(which = c('cols')) |>#remove columns entirely empty
  clean_names() |> # clean names
  # replace location name
  mutate(study_name = str_replace(study_name,'YT_Stj_22','YT_Addie_22'),
         location_name = str_replace(location_name,'St. Jacob Township, IL','Addieville, IL')) |>
  # select variables
  dplyr::select(study_name, study_year, location_name, germplasm_name,
                replicate, block_number, plot_number, row_number, col_number,
                grain_yield_kg_ha_co_321_0001218, grain_test_weight_g_l_co_321_0001210,
                heading_time_julian_date_jd_co_321_0001233, plant_height_cm_co_321_0001301,
                maturity_time_spike_estimation_julian_date_jd_co_321_0501101) |>
  # rename variables
  rename(trial=study_name,
         year=study_year,
         location=location_name,
         germplasm=germplasm_name,
         rep=replicate,
         block=block_number,
         row=row_number,
         col=col_number,
         grain_yield=grain_yield_kg_ha_co_321_0001218,
         test_weight=grain_test_weight_g_l_co_321_0001210,
         heading_time=heading_time_julian_date_jd_co_321_0001233,
         plant_height=plant_height_cm_co_321_0001301,
         maturity=maturity_time_spike_estimation_julian_date_jd_co_321_0501101) |>
  # arrange data by study, row, and col
  arrange(trial,row,col) |>
  # convert to factors
  mutate_at(vars(trial:col),as.factor) |>
  # convert to numeric
  mutate_at(vars(grain_yield:maturity),
            as.numeric) |>
  # replace 0 by NA for response variables
  mutate_at(vars(grain_yield:maturity),
            ~ifelse(.<=0,NA,.)) |>
  filter(grepl("IL", location)) |>
  glimpse()

### Long format ----
pheno_raw_l <- pheno_raw_w |> #raw data long format
  pivot_longer(cols = grain_yield:maturity,
               names_to = 'trait', values_to = 'value', values_drop_na = F) |>
  mutate(trait=as.factor(trait)) |>
  arrange(trial, trait, as.numeric(row), as.numeric(col)) |>
  group_by(trial, trait) |>
  filter(!is.na(mean(value, na.rm = TRUE))) |>
  ungroup() |>
  glimpse()

### Plot data ----

#### Boxplot ----
pheno_raw_l |>
  ggplot(aes(x=trial, y=value)) +
  geom_boxplot() +
  facet_wrap(~trait, scales = "free", ncol = 1)

#### Tile plot ----
pheno_raw_l |>
  filter(trait=="grain_yield") |>
  ggplot(aes(x=col,y=row, fill=value)) +
  geom_tile() +
  facet_wrap(~trial, ncol = 2) +
  scale_fill_viridis_c(name= "Grain yield",
                       option = "viridis") +
  theme_bw()

# BLUP - single trial ----

# Single trial BLUP to calculate reliability

## ASReml options for spatial analysis

asreml.options(gammaPar= T) # gamma parameterization

## BLUP function ----
mod.blup_single_trial <- function(dat) {
  mod <- asreml(value~1,
                random = ~germplasm,
                residual = ~ar1v(row):ar1(col),
                data=dat)
  # Update model
  mod <- update(mod)
  # Mean reliabilty
  m_rel<- predict(mod, classify='germplasm', ignore=c('(Intercept)'))$pvals |>
    as.data.frame() |>
    mutate(pev = std.error^2,
           Vg = summary(mod)$varcomp['germplasm','component'],
           rel = 1-(pev/Vg),
           m_rel=mean(rel)) |>
    summarise(m_rel=mean(rel))
  return(m_rel)
}

## Run BLUP ----

# Use map() to run model for each individual trial and return the trial mean reliability
reliability <- pheno_raw_l |>
  group_by(trial, trait) |>
  nest() |>
  mutate(r2=map(data,~mod.blup_single_trial(.x))) |>
  dplyr::select(-c(data)) |>
  unnest(r2) |>
  glimpse()

## Plot reliability ----
reliability |>
  ggplot(aes(x=trial, y=m_rel)) +
  geom_point() +
  facet_wrap(~trait, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# BLUE - single trial ----
# Individual genotype blues, SE, and weights to be used in other models

## BLUE function ----
mod.blue_single_trial <- function(dat) {
  mod <- asreml(value~1 + germplasm,
                residual = ~ar1v(row):ar1(col),
                data=dat)
  # Update model
  mod <- update(mod)
  # Mean reliabilty
  blues <- predict(mod, classify='germplasm', pworkspace="1gb")$pvals
  # Result that will be printed
  return(blues)
}

## Run BLUE ----

# Use map() to run model for each individual trial and return indivudual genotype's blues, SE, and weight
blues <- pheno_raw_l |>
  group_by(trial,trait) |>
  nest() |>
  mutate(blues=map(data,~mod.blue_single_trial(.x))) |>
  dplyr::select(-data) |>
  unnest(blues) |>
  dplyr::select(-status) |>
  mutate(weight = 1/(std.error^2)) |>
  glimpse()

# END ----
time <- Sys.time() - start.time
time

# Save ----
rm("pheno_raw_l", "pheno_raw_w", "reliability", "mod.blue_single_trial", "mod.blup_single_trial", "start.time")

save.image("data/step1-BLUES.RData")
