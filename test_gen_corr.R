# Multi-trait GBLUP ----

# Objective ----
# - Run Multi-trait GBLUP using the combination of trait and trial as groups
# - Get GEBVs

rm(list=ls()) # clean workspace

# Packages ----

library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package
library(doMC) # Parallel computing with doMC

# Load data ----
## BLUES & K2 ----
load('data/mod_MT_GBLUP_22.23_group.RData')

# Set up parallel processing
num_cores <- 40
registerDoMC(num_cores)

# MT-GBLUP ----
summary(mod_MT_GBLUP_22.23_group)$varcomp |>
  as.data.frame() |>
  rownames_to_column()

## Trial to trial genetic correlations
vc <- summary(mod_MT_GBLUP_22.23_group)$varcomp
R <- vc$component[1:29]
L1 <- vc$component[30:58]
L2 <- vc$component[59:87]
L3 <- vc$component[88:116]
L <- cbind(L1, L2, L3)

VG <- L%*%t(L) + diag(R)
rownames(VG) <- levels(bluesG_22.23$group)
colnames(VG) <- levels(bluesG_22.23$group)
CG <- cov2cor(VG)


CG |> glimpse()


# Define replacements
replacements <- c("grain_yield_YT_" = "Yld - ",
                  "heading_time_YT_" = "Hdt - ",
                  "plant_height_YT_" = "Pht - ",
                  "test_weight_YT_" = "Twt - ")

# Apply replacements to row names
for (pattern in names(replacements)) {
  rownames(CG) <- gsub(pattern, replacements[pattern], rownames(CG))
}

# Apply replacements to column names
for (pattern in names(replacements)) {
  colnames(CG) <- gsub(pattern, replacements[pattern], colnames(CG))
}

CG |> glimpse()

library(corrplot)


corrplot(CG, 
         tl.cex = 0.5, tl.col = "black", tl.srt = 45)

corrplot.mixed(CG, upper = 'pie', lower = 'pie',
               number.cex = 1.2, number.digits = 2,
               diag = NULL,
               tl.col = 'black', tl.cex = 0.75,
               tl.pos = '')

library(corrplot)

# Define color palette
my_colors <- colorRampPalette(c("#FF5F0F", "white", "#13294b"))(100)

# Create correlation plot with specified colors

png('figures/gen_corr.png', width = 8, height = 8, units = 'in', res = 320)
corrplot(CG, 
         method = 'circle', 
         type = 'lower', 
         col = my_colors,
         number.cex = 1.2, 
         number.digits = 2,
         diag = T,
         tl.col = 'black', 
         tl.cex = 0.75,
         tl.srt = 10)
dev.off()

# summarize ----

# Create a boxplot with the genetic correlations among traits

#Steps
## Rename rows and columns to keep only trait, pivot longer
#
CG |>
  as.data.frame() |>
  rownames_to_column('trait1') |>
  pivot_longer(cols = c(grain_yield_YT_Addie_22:test_weight_YT_Urb_23), 
               names_to = 'trait2', values_to = 'corr') |>
  mutate(trait1 = str_extract(trait1, "(.*?)_YT"),
         trait2 = str_extract(trait1, "(.*?)_YT")) |>
  mutate(trait=paste(trait1,trait2,sep='_')) |>
  View()
  ggplot(aes(x=trait, y=corr))+
  geom_boxplot()

