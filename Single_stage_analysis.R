# Single stage analysis ----

# Objectives ----
# 1 - Phenotypic data
## 1.1 - Load
## 1.2 - Organize

# 2 - Genotypic data
## 2.1 - Load
## 2.2 - Create relationship matrix

# 3 - Single-trait GBLUP
## 3.1 - Get GEBVs

# 4 - Multi-trait GBLUP
## 4.1 - Get GEBVs

rm(list=objects()) # clean workspace

# Packages ----
library(tidyverse) # R packages for data science
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data
library(asreml) # ASReml-R package

# Functions ----
# Convert Yield to bu/acre
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
                replicate, block_number, plot_number, row_number, col_number, entry_type,
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
  filter(grepl('IL', location)) |>
  # assign checks
  filter(germplasm!='FILL') |> # Can I consider all AgriMAXX503?
  # If germplasm is FILL all columns are NA?
  mutate(entry_type=ifelse(germplasm%in%c('07-19334','12-26004','16-8048','25R76',
                                        'AgriMAXX503','Kaskaskia','Pio 25R74'),'check',entry_type)) |>
  select(-maturity) |>
  glimpse()

# look what genotypes are checks
pheno_raw_w |>
  filter(entry_type=='check') |>
  group_by(germplasm) |>
  summarise(check=1)

#pheno_raw_w |> filter(germplasm=="07-19334")

# Genotypic data ----
## Load and organize ----
geno <- read.csv('data/geno2023.csv', header = T) |>
  column_to_rownames('X') |> # Transposing the genotype column to row names
  # Replacing NA values with the mean of the respective column (marker average)
  mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

# Check for columns with NA, markers without information
colNaN <- colnames(geno)[colSums(is.na(geno)) > 0]
colNaN

# Removing markers (columns) from the dataset where all values are NA
geno <- geno |>
  select(!all_of(colnames(geno)[colSums(is.na(geno)) > 0])) |> 
  rownames_to_column('germplasm')

## Subset genotypic data
# Subset genotypic data to include only 'test' germplasm
germplasm_test <- pheno_raw_w |> filter(entry_type=='test') |> 
  reframe(test = unique(germplasm)) |>
  glimpse()

geno_22.23 <- geno |>
  filter(germplasm %in% germplasm_test$test)# |>
#  column_to_rownames('germplasm')

checks <- unique(pheno_raw_w |> filter(entry_type=='check') |> select(germplasm) |> mutate(germplasm=as.character(germplasm)))
testG <- geno_22.23$germplasm
check_test <- c(checks$germplasm, testG)

pheno_filtered_w <- pheno_raw_w |>
  filter(germplasm%in%check_test) |>
  glimpse()

### Long format ----
pheno_filtered_l <- pheno_filtered_w |> #raw data long format
  pivot_longer(cols = grain_yield:plant_height,
               names_to = 'trait', values_to = 'value', values_drop_na = F) |>
  mutate(trait=as.factor(trait)) |>
  arrange(trial, trait, as.numeric(row), as.numeric(col)) |>
  group_by(trial, trait) |>
  filter(!is.na(mean(value, na.rm = TRUE))) |>
  ungroup() |>
  glimpse()

pheno_filtered_l |>
  filter(trait=='grain_yield') |>
  ggplot(aes(x=col,y=row, fill=entry_type)) +
  geom_tile() +
  facet_wrap(~trial, ncol = 2) +
  #  scale_fill_viridis_c(name= 'Grain yield',
  #                       option = 'viridis') +
  theme_bw()

pheno_raw_w |> group_by(germplasm) |> summarise(entry_type=unique(entry_type),obs=n()) |>
  left_join(pheno_filtered_w |> group_by(germplasm) |> summarise(G=T)) |>
  arrange(entry_type) |>
  filter(is.na(G)|entry_type=="check")


pheno_raw_l |>
  filter(trait=='grain_yield') |>
  ggplot(aes(x=col,y=row, fill=value)) +
  geom_tile() +
  facet_wrap(~trial, ncol = 2) +
  scale_fill_viridis_c(name= 'Grain yield',
                       option = 'viridis') +
  theme_bw()

glimpse(pheno_raw_w)

genotyped <- geno |>
  select(germplasm) |>
  group_by(germplasm) |>
  summarise(geno=1) |>
  filter(str_detect(germplasm,"Pio")) |>
  View()

phenotyped <- pheno_raw_w |>
  group_by(germplasm) |>
  summarise(pheno=n()) |>
  glimpse()

totals <- phenotyped |>
  left_join(genotyped) |>
  mutate(sum=pheno+geno) |>
  filter(is.na(sum)) |>
  View()



