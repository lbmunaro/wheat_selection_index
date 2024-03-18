# Net merit ----

# Objective ----
# - Calculate the net merit for each genotype

rm(list=objects()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science

# Load data ----
## ST-GBLUP
load("data/step3-ST-GBLUP.RData")
## MT-GBLUP ----
load("data/step4-MT-GBLUP.RData")

# remove unnecessary data
rm(list = c(ls(pattern = "^mod_")))

# Create a single data set with all scenarios
GEBVs <- data.frame() |>
  bind_rows(GEBVs_ST_GBLUP_22 |> mutate(scenario="ST_GBLUP_22"),
            GEBVs_ST_GBLUP_23 |> mutate(scenario="ST_GBLUP_23"),
            GEBVs_ST_GBLUP_22.23 |> mutate(scenario="ST_GBLUP_22.23"),
            GEBVs_MT_GBLUP_22 |> mutate(scenario="MT_GBLUP_22"),
            GEBVs_MT_GBLUP_23 |> mutate(scenario="MT_GBLUP_23"),
            GEBVs_MT_GBLUP_22.23 |> mutate(scenario="MT_GBLUP_22.23")) |>
  rename(gebv=predicted.value) |>
  select(scenario, trait, germplasm, gebv) |>
  pivot_wider(names_from = trait, values_from = gebv) |>
  glimpse()

# Economic values ----

# Wheat price 7 yr average
wheat_price0<- mean(c(9.9128, 7.0402, 5.4621, 4.9414, 4.9757, 4.4014, 4.3945))

# Soybean price 7 yr average
soybean_price<- mean(c(16.1773, 13.6890, 9.5344, 8.9298, 9.3456, 9.7820, 9.8753))

# Wheat price function
wheatPrice<- function(twt, wheat_price0){
  twtDiscount<- c(58-twt)*-.08
  wheat_price<- wheat_price0+twtDiscount
  return(wheat_price)
}

# Net merit function ----
netMerit<- function(headings, yields, twts, wheat_price0, soybean_price){
  wheat_price1<- wheatPrice(twts, wheat_price0)
  soy_yld_gain<- 0.5* (135-headings)
  soy_profit_gain<- soy_yld_gain*soybean_price
  wheat_profit<- yields*wheat_price1
  total_profit<- wheat_profit + soy_profit_gain
  return(total_profit)
}

# Net merit ----
# Calculate net merit
GEBVs_NetMerit <- GEBVs |>
  mutate(net_merit=netMerit(heading_time,grain_yield, test_weight,
                            wheat_price0, soybean_price)) |>
  glimpse()

write.csv(GEBVs_NetMerit,"data/net_merit.csv", row.names = F)
