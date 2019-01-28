#### Data description ----
# 1 strain (yeast DAY286), 4 biological replicates, 2 compartments (EV & WCL)
# (WCL = whole cell lysate)
# shotgun proteomics data obtained on Thermo Q Exactive HF
# raw data searched against C. albicans SC5314 UniProt proteome with MaxQuant
# resulting proteinGroups.txt file used for this analysis, rows = protein groups

#### Load required packages and scripts ----
library(dplyr)
library(ggplot2)
library(limma)
library(tibble)
library(tidyr)
source("R/util.R")
source("R/assert_functions.R")
source("R/DEP_plot_functions_freq.R")
source("R/DEP_plot_functions_qc.R")

#### Import data and reference tables ----
# load proteinGroups.txt
pg_orig <- read.table("data/raw/yeast-proteinGroups.txt", sep = "\t", header = TRUE,
                      stringsAsFactors = FALSE)

# fix headers containing extra `.`
pg_orig <- col_clean(pg_orig)

# load experimental design
expd <- read.table("data/raw/yeast-expdesign.txt", sep = "\t", header = TRUE,
                   stringsAsFactors = FALSE)

# load clean UniProt reference table
unip_table <- read.table(file = "data/clean/unip-table.txt", sep = "\t", header = TRUE,
                         stringsAsFactors = FALSE)

# load clean CGD reference table
cgd_table <- read.table(file = "data/clean/cgd-table.txt", sep = "\t", header = TRUE,
                        stringsAsFactors = FALSE)

#### Initial protein filtering ----
# remove reversed proteins, contaminant proteins
pg <- pg_orig %>%
  filter(Reverse != "+", Potential.contaminant != "+")

# keep only proteins identified by min 2 unique peptides
pg <- pg %>%
  filter(Unique.peptides >= 2)

#### Prepare LFQ data frame for initial analysis ----
lfq <- convert_lfq(pg, expd)

plot_frequency2(lfq) # most proteins identified in only 1 sample

plot_numbers2(lfq, expd) # WCL_2 has far fewer proteins than other WCL samples

lfq_summarise(lfq) # WCL_2 has more `NA` values than valid values

#### Initial filtering and normalisation ----
# filter for proteins identified in at least 3/4 replicates of EV or WCL
lfq_filt <- na_filter2(lfq, logic = "or",
                       pattern1 = "EV.*", value1 = 1, pattern2 = "W.*", value2 = 1)

plot_frequency2(lfq_filt) # now most proteins identified in 8/8 samples

plot_numbers2(lfq_filt, expd) # WCL_2 still has far fewer proteins than other WCL samples

plotMDS(lfq_filt) # MDS plot shows WCL_2, WCL_3, EV_1 not clustering with other samples

# normalise data
lfq_filt_norm <- normalizeCyclicLoess(lfq_filt)

plot_normalization2(expd, lfq_filt, lfq_filt_norm) # WCL_2 sample not centred well

plotMDS(lfq_filt_norm) # EV_1 sample clear outlier on MDS plot

plot_dendro(lfq_filt_norm) # EV_1 not clustering with other EV samples

#### Final filtering and normalisation ----
# drop EV_1 and WCL_2
lfq2 <- lfq[, !colnames(lfq) %in% c("EV_1", "WCL_2")]

# filter for proteins identified in at least 3/3 replicates of EV or WCL
lfq2_filt <- na_filter2(lfq2, "or", pattern1 = "EV.*", value1 = 0, pattern2 = "W.*", value2 = 0)

plot_frequency2(lfq2_filt) # mmost proteins identified in 3/6 or 6/6 samples

plot_numbers2(lfq2_filt, expd) # similar intragroup protein numbers

plotMDS(lfq2_filt) # WCL_3 outlier, try normalisation

# normalise data
lfq2_filt_norm <- normalizeCyclicLoess(lfq2_filt)

plot_normalization2(expd, lfq2_filt, lfq2_filt_norm)

plotMDS(lfq2_filt_norm) # looks ok

plot_dendro(lfq2_filt_norm) # looks ok

#### Imputation ----



