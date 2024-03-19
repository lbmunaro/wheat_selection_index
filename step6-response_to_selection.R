# Response to selection ----

# Objective ----
# - Calculate the response to selection for each scenario

rm(list=objects()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science

# Load data ----
## GEBVs & Net merit ----
NET_MERIT <- read.csv('data/net_merit.csv') |>
  mutate_if(is.character, as.factor) |>
  glimpse()

# Response to selection ----

RESP_SEL <- NET_MERIT |>
  arrange(scenario, desc(grain_yield)) |>
  group_by(scenario) |>
  mutate(rank_grain_yield = rank(desc(grain_yield)),
         rank_net_merit = rank(desc(net_merit))) |>
  nest() |>
  # Mean gebv before selection for all traits
  mutate(mean0_grain.yield = map_dbl(data, ~mean(.x$grain_yield)),
         mean0_test.weight = map_dbl(data, ~mean(.x$test_weight)),
         mean0_heading.time = map_dbl(data, ~mean(.x$heading_time)),
         mean0_plant.height = map_dbl(data, ~mean(.x$plant_height)),
         mean0_net.merit  = map_dbl(data, ~mean(.x$net_merit))) |>
  # Mean gebv after selecting top 20 individuals for grain yield
  mutate(meanSelYld_grain.yield = map_dbl(data, ~mean(.x$grain_yield[.x$rank_grain_yield < 21])),
         meanSelYld_test.weight = map_dbl(data, ~mean(.x$test_weight[.x$rank_grain_yield < 21])),
         meanSelYld_heading.time = map_dbl(data, ~mean(.x$heading_time[.x$rank_grain_yield < 21])),
         meanSelYld_plant.height = map_dbl(data, ~mean(.x$plant_height[.x$rank_grain_yield < 21])),
         meanSelYld_net.merit = map_dbl(data, ~mean(.x$net_merit[.x$rank_grain_yield < 21]))) |>
  # Mean gebv after selecting top 20 individuals for net merit
  mutate(meanSelNM_grain.yield = map_dbl(data, ~mean(.x$grain_yield[.x$rank_net_merit < 21])),
         meanSelNM_test.weight = map_dbl(data, ~mean(.x$test_weight[.x$rank_net_merit < 21])),
         meanSelNM_heading.time = map_dbl(data, ~mean(.x$heading_time[.x$rank_net_merit < 21])),
         meanSelNM_plant.height = map_dbl(data, ~mean(.x$plant_height[.x$rank_net_merit < 21])),
         meanSelNM_net.merit = map_dbl(data, ~mean(.x$net_merit[.x$rank_net_merit < 21]))) |>
  select(-data) |>
  pivot_longer(cols = c(2:16),
               names_to = c('.value', 'trait'),
               names_pattern = '(.+)_(.+)') |>
  mutate(RespSelYld=meanSelYld-mean0,
         RespSelNM=meanSelNM-mean0) |>
  select(scenario, trait, RespSelYld, RespSelNM) |>
  pivot_longer(cols = c(RespSelYld, RespSelNM), names_to = 'criteria', values_to = 'RespSel') |>
  mutate(scenario = gsub('_GBLUP', '', scenario)) |>
  separate(scenario, into = c('model', 'dataset'), sep = '_', remove = F) |>
  unite('mod_crit', c(model,criteria), remove = F) |>
  mutate(dataset = gsub('22.23', 'Combined', dataset),
         dataset = gsub('22', '2022', dataset),
         dataset = gsub('23', '2023', dataset),
         model = gsub('MT', 'MT-GBLUP', model),
         model = gsub('ST', 'ST-GBLUP', model)) |>
  mutate(trait=fct_relevel(trait,c('net.merit', 'grain.yield', 'heading.time', 'test.weight', 'plant.height'))) |>
  glimpse()

## Plot ----

# colors
colors_full <- c('MT_RespSelNM' = '#13294B', 'ST_RespSelNM' = adjustcolor('#13294B', 0.5),
            'MT_RespSelYld' = '#FF552E', 'ST_RespSelYld' = adjustcolor('#FF552E', 0.5))

# Plot Full
RESP_SEL |>
  ggplot(aes(x=dataset, y=RespSel)) +
  geom_bar(aes(fill=mod_crit),
           stat = 'identity', position = 'dodge') +
  facet_wrap(~trait, scales='free', ncol = 2,
             labeller = labeller(trait = c('net.merit' = 'Net merit (USD)',
                                           'grain.yield' ='Grain yield (bu/ac)',
                                           'heading.time' = 'Heading time (days)',
                                           'test.weight' = 'Test weight (lb/bu)',
                                           'plant.height' = 'Plant height (cm)'))) +
  scale_fill_manual('Model/Selection criteria', values = colors_full,
                    label=c('MT-GBLUP/Net merit','MT-GBLUP/Grain yield',
                            'ST-GBLUP/Net merit','ST-GBLUP/Grain yield')) +
  xlab('Data set') + ylab('Response to selection') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.75,0.15))

ggsave('figures/RespSel_full.png', width = 6, height = 6, units = 'in', dpi=320)

# Plot Summary
colors_summary <- c('RespSelNM' = '#13294B', 'RespSelYld' = '#FF552E')

RESP_SEL |> filter(dataset=='Combined') |>
  ggplot(aes(x=model, y=RespSel)) +
  geom_bar(aes(fill=criteria), 
           stat = 'identity', position = 'dodge') +
  facet_wrap(~trait, scales='free', ncol = 2,
             labeller = labeller(trait = c('net.merit' = 'Net merit (USD)',
                                           'grain.yield' ='Grain yield (bu/ac)',
                                           'heading.time' = 'Heading time (days)',
                                           'test.weight' = 'Test weight (lb/bu)',
                                           'plant.height' = 'Plant height (cm)'))) +
  scale_fill_manual('Selection criteria', label=c('Net merit','Grain yield'),values = colors_summary) +
  xlab(NULL) + ylab('Response to selection') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.75,0.15))

# Save ----

save.image('data/step6-response_to_selection.RData')
