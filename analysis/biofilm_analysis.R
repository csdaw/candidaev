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
library(VennDiagram)
source("R/util.R")
source("R/assert_functions.R")
source("R/DEP_plot_functions_freq.R")
source("R/DEP_plot_functions_qc.R")
source("R/plot_functions.R")

### Import data and reference tables ----
# load proteinGroups.txt
b_pg_orig <- read.table("data/raw/biofilm-proteinGroups.txt", sep = "\t", header = TRUE,
                      stringsAsFactors = FALSE)

# fix headers containing extra `.`
b_pg_orig <- col_clean(b_pg_orig)

# load experimental design
b_expd <- read.table("data/raw/biofilm-expdesign.txt", sep = "\t", header = TRUE,
                   stringsAsFactors = FALSE)

# load clean UniProt reference table
unip_table <- read.table(file = "data/clean/unip-table.txt", sep = "\t", header = TRUE,
                         stringsAsFactors = FALSE)

# load clean CGD reference table
cgd_table <- read.table(file = "data/clean/cgd-table.txt", sep = "\t", header = TRUE,
                        stringsAsFactors = FALSE)

#### Initial protein filtering ----
# remove reversed proteins, contaminant proteins
b_pg <- b_pg_orig %>%
  filter(Reverse != "+", Potential.contaminant != "+")

# keep only proteins identified by min 2 unique peptides
b_pg <- b_pg %>%
  filter(Unique.peptides >= 2)

#### Prepare LFQ data frame for initial analysis ----
b_lfq <- convert_lfq(b_pg, b_expd)

plot_frequency2(b_lfq) # most proteins identified in only 1 sample

plot_numbers2(b_lfq, b_expd) # EV_5 has many more proteins than other EV samples

lfq_summarise(b_lfq) # EV_1 to EV_4 have more `NA` values than valid values

#### Filtering and normalisation ----
# filter for proteins identified in at least 4/5 replicates of EV or WCL
b_lfq_filt <- na_filter2(b_lfq, logic = "or",
                         pattern1 = "EV.*", value1 = 1, pattern2 = "W.*", value2 = 1)

plot_frequency2(b_lfq_filt) # now most proteins identified in 10/10 samples

plot_numbers2(b_lfq_filt, b_expd) # EV_5 now similar to other EV samples

b_ev <- na_filter(b_lfq_filt, "<=", "E.*", 4)
b_wcl <- na_filter(b_lfq_filt, "<=", "W.*", 4)

plot_venn(list(ev = b_ev, w = b_wcl), unip_table, plot = TRUE, main = "Overlap of proteins in at least 1/5 reps of WCL or EV")

lfq_summarise(b_lfq_filt) # Percent `NA` values reduced to ~30% for EVs and ~13% for WCL

plotMDS(b_lfq_filt) # Clustering is decent, WCL_3 and EV_5 a bit far from other samples

# normalise data
b_lfq_filt_norm <- normalizeCyclicLoess(b_lfq_filt)

plot_normalization2(b_expd, b_lfq_filt, b_lfq_filt_norm) # EV samples not centered well

plotMDS(b_lfq_filt_norm) # WCL samples cluster well but EV samples quite spread out

plot_dendro(b_lfq_filt_norm) # Dendrogram looks good though

#### Imputation ----
# explore percentage of missing values in each sample
lfq_summarise(b_lfq_filt_norm) # 11.3%-34.6% `NA`

# should not impute with so many missing values
# remove proteins with 4 or more missing values in EV or WCL
b_lfq_noimp <- na_filter4(b_lfq_filt_norm, "or", "EV.*", 4, "W.*", 4)
b_lfq_both <- na_filter2(b_lfq_filt_norm, "and", "EV.*", 3, "W.*", 3)

# explore percentage of missing values in lfq_both
lfq_summarise(b_lfq_both) # 2.2-17.3 percent

# explore pattern of missing values in data.
plot_missval2(b_lfq_both) # missing values more abundant in EV samples

# plot intensity distribution for proteins with and without missing values
plot_detect2(b_lfq_both) # protein with missing values tend to have lower intensity

# therefore proteins are MNAR, close to detection limit
# use left censored imputation method
b_lfq_imp <- QRILC_impute(b_lfq_both)

# observe effect of imputation on sample intensity distribution
plot_imputation2(b_expd, b_lfq_both, b_lfq_imp)

#### Differential expression analysis with limma ----
# stick imputed proteins and exclusive proteins back together
Reduce(intersect, list(rownames(b_lfq_imp), rownames(b_lfq_noimp))) # No duplicated rows
b_lfq_de <- rbind(b_lfq_noimp, b_lfq_imp)

# see limma user guide section 9.2 for more info
# create design matrix
b_samples <- data.frame(T = (rep(c("EV", "WCL"), each = 5)))
b_design <- model.matrix( ~ 0 + T, data = b_samples)
colnames(b_design) <- c("EV", "WCL")

# make all pair-wise comparisons between EV and WCL
b_cm <- makeContrasts(EV - WCL, levels = b_design)
b_fit_ev <- lmFit(b_lfq_de, design = b_design)
b_fit_ev_cm <- contrasts.fit(b_fit_ev, b_cm)
b_efit_ev <- eBayes(b_fit_ev_cm)

# extract DE results
b_tt <- topTable(b_efit_ev, sort.by = "none", number = Inf) %>%
  as.matrix()

#### Explore results ----
b_result <- combine_result(lfq_de = b_lfq_de, tt = b_tt)

b_sig <- b_result %>%
  as.data.frame() %>%
  rownames_to_column(var = "UP_accession") %>%
  mutate(name = match_uniprot(.[["UP_accession"]], unip_table, "CGD_gene_name", "UP_accession")) %>%
  filter(significant == 1)

plot_dendro(b_lfq_de)

plot_heatmap2(b_result, b_expd, unip_table, kmeans = TRUE, k = 6, clustering_distance = "euclidean", indicate = c("condition", "replicate"), na_col = "green")

b_accession_interest <- b_result %>%
  as.data.frame() %>%
  rownames_to_column(var = "accession") %>%
  filter(AveExpr > 30) %>%
  pull(accession)



b_prot_interest <- match_uniprot(b_accession_interest, unip_table, "CGD_gene_name", "UP_accession")

plot_volcano2(b_result, b_expd, unip_table, lab = b_prot_interest)


#### Output results table ----

b_result_df <- b_result %>%
  as.data.frame() %>%
  rownames_to_column(var = "UP_accession") %>%
  mutate(group = case_when(significant == 0 ~ "not sig",
                           significant == 1 & logFC > 0 ~ "ev up",
                           significant == 1 & logFC < 0 ~ "wcl up",
                           is.na(significant) == TRUE & UP_accession %in% rownames(b_ev) ~ "ev ex",
                           is.na(significant) == TRUE & UP_accession %in% rownames(b_wcl) ~ "wcl ex"),
         in_ev = ifelse(UP_accession %in% rownames(b_ev), TRUE, FALSE),
         in_wcl = ifelse(UP_accession %in% rownames(b_wcl), TRUE, FALSE),
         CGD_gene_name = match_uniprot(.[["UP_accession"]], unip_table, "CGD_gene_name", "UP_accession"),
         Protein_name = match_uniprot(.[["UP_accession"]], unip_table, "Protein_name", "UP_accession")) %>%
  select(UP_accession, CGD_gene_name, Protein_name, everything())

table(b_result_df$group)
table(b_result_df$in_ev)



