#### Data description ----
# 1 strain (biofilm DAY286), 5 biological replicates, 2 compartments (EV & WCL)
# (WCL = whole cell lysate)
# shotgun proteomics data obtained on Thermo Q Exactive HF
# raw data searched against C. albicans SC5314 UniProt proteome with MaxQuant
# resulting proteinGroups.txt file used for this analysis, rows = protein groups

#### Load required packages and scripts ----
library(cluster)
library(ComplexHeatmap)
library(dplyr)
library(ggplot2)
library(limma)
library(stringr)
library(tibble)
library(tidyr)
source("R/util.R")
source("R/assert_functions.R")
source("R/DEP_plot_functions_freq.R")
source("R/DEP_plot_functions_qc.R")
source("R/plot_functions.R")
