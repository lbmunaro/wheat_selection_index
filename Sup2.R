rm(list=objects()) # clean workspace

# Packages ----
library(tidyverse) # R packages for data science
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(asreml) # ASReml-R package

# Functions ----
# Donvert Yield to bu/acre
convYld<- function(y){
  x<- y/c(60 * 0.453592 * 2.47105)
  return(x)
}

# Convert test weight to lbs/bu
convTwt<- function(y){
  x<- y/1000 *2.2046 *35.2391
  return(x)
} 

# Phenotypic data ----

## Load and organize ----

### Wide format ----
pheno_raw_w <- #raw data wide format
  read.csv('data/IL_22-23YT_02.27.24.csv', skip = 3) |> # read csv file
  remove_empty(which = c('cols')) |> #remove columns entirely empty
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
  # convert grain yield to bu/ac and test weight to lb/bu
  mutate(grain_yield=convYld(grain_yield),
         test_weight=convTwt(test_weight)) |>
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
  # filter trials to include only IL
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
YT_Urb_23 <- pheno_raw_w |>
  filter(trial=="YT_Neo_23") |>
  glimpse()



#### Tile plot ----
YT_Urb_23 |>
  ggplot(aes(x=col,y=row, fill=rep)) +
  geom_tile() +
  theme_bw()


# sparse test ----
pheno_raw_w |> glimpse()

sparse <- pheno_raw_w |>
  mutate(year_n=as.character(year)) |>
  filter(grepl("2023",year_n)) |>
  group_by(germplasm,trial) |>
  mutate(Rep=n()) |>
  mutate(Rep=as.factor(Rep)) |>
  mutate(location=fct_relevel(location,c("Urbana, IL", "Neoga, IL", "St Peter, IL",
                                   "Addieville, IL", "Belleville, IL"))) |>
  glimpse()

ggplot(sparse, aes(x=germplasm, y=location)) +
  geom_tile(aes(fill=Rep,width=1,height=1))+
  scale_fill_manual(values=c("1"="#FF5F05","2"="#13294B")) +
  xlab("Genotype") + ylab("Location") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("figures/sparse.png",width = 6,height = 2.5, dpi = 320)
