# wheat_selection_index

![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![](https://img.shields.io/badge/RStudio-4285F4?style=for-the-badge&logo=rstudio&logoColor=white)
<img src="uiuc.png" height="25">

## Optimizing winter wheat multi-trait selection for wheat-soybean double-crop systems using genomic selection index

### Authors
- Lucas Berger Munaro <img src="uiuc.png" height="25">
[![](https://img.shields.io/badge/LinkedIn-0077B5?style=for-the-badge&logo=linkedin&logoColor=white)](https://www.linkedin.com/in/lucas-berger-munaro/)
[![](https://img.shields.io/badge/GitHub-100000?style=for-the-badge&logo=github&logoColor=white)](https://github.com/lbmunaro)
[![](https://img.shields.io/badge/Google_Scholar-4285F4?style=for-the-badge&logo=google-scholar&logoColor=white)](https://scholar.google.com/citations?user=4awE-QsAAAAJ&hl=en)
lucasb4@illinois.edu

- Dr. Jessica Rutkoski <img src="uiuc.png" height="25">

### Description

This R project contain all scripts and data for the **"Optimizing winter wheat multi-trait selection for wheat-soybean double-crop systems using genomic selection index"** chapter of my PhD dissertation.

### Objectives

#### [![](https://img.shields.io/badge/RStudio-4285F4?style=for-the-badge&logo=rstudio&logoColor=white) wheat_selection_index.Rproj](https://github.com/lbmunaro/wheat_selection_index/blob/main/wheat_selection_index.Rproj)

- RStudio Project

#### [![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white) step1-single_trial_analysis.R](https://github.com/lbmunaro/wheat_selection_index/blob/main/step1-single_trial_analysis.R)

- Organize and check phenotypic data
- Run single trial BLUP models to check reliability
- Run single trial BLUE models to estimate genotype's blues

#### [![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white) step2-genotypic_data.R](https://github.com/lbmunaro/wheat_selection_index/blob/main/step2-genotypic_data.R)

- Subset phenotypic and genotypic data
- Create relationship matrices

#### [![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white) step3-ST-GBLUP.R](https://github.com/lbmunaro/wheat_selection_index/blob/main/step3-ST-GBLUP.R)
- Run Single-trait GBLUP for each scenario
- Get GEBVs

#### [![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white) step4-MT-GBLUP.R](https://github.com/lbmunaro/wheat_selection_index/blob/main/step4-MT-GBLUP.R)
- Run Multi-trait GBLUP for each scenario
- Get GEBVs

#### [![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)  step5-net_merit.R](https://github.com/lbmunaro/wheat_selection_index/blob/main/step5-net_merit.R)
- Calculate the net merit for each genotype

#### [![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white) step6-response_to_selection.R](https://github.com/lbmunaro/wheat_selection_index/blob/main/step6-response_to_selection.R)
- Calculate the response to selection for each scenario

#### [![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white) step7-correlation_among_gebvs.R](https://github.com/lbmunaro/wheat_selection_index/blob/main/step7-correlation_among_gebvs.R)
- Calculate the correlation among gebvs

#### [![](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)  step8-genetic_correlation.R]()
- Calculate genetic correlation among traits (available soon)













