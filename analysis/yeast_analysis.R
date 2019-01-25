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

#### Import data and reference tables ----
# load proteinGroups.txt
pg_orig <- read.table("data/raw/yeast-proteinGroups.txt", sep = "\t", header = TRUE,
                      stringsAsFactors = FALSE)

# fix headers containing extra `.`
pg_orig <- col_clean(pg_orig)

# load experimental design
expd <- read.table("data/raw/yeast-expdesign2.txt", sep = "\t", header = TRUE,
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

# pg <- val_filter2(pg, pattern1 = "LFQ.*EV", value1 = 2, pattern2 = "LFQ.*W", value2 = 2)

#### Prepare LFQ data frame for analysis ----
# separate protein LFQ data from other data
lfq <- pg %>%
  select(Majority.protein.IDs, matches("LFQ.intensity.*EV"), matches("LFQ.intensity.*W"))

# convert to matrix
lfq <- lfq %>%
  remove_rownames() %>%
  column_to_rownames(var = "Majority.protein.IDs") %>%
  as.matrix()

colnames(lfq) <- expd$ID

# convert zero to NA and log2 transform LFQ data
lfq[lfq == 0] <- NA
lfq <- log2(lfq)

plot_numbers2(lfq, expd, plot = FALSE)

plot_frequency2(lfq, plot = FALSE)
