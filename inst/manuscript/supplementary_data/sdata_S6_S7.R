#### Setup ####
# need writexl package to write output to .xlsx
requireNamespace("writexl", quietly = TRUE)

# load packages
library(dplyr)
library(candidaev)
library(limma)

#### Supplementary Data S6 ####
# Table of proteins
# Compare the 690 DAY286 yeast EV proteins shown in Figure 2A venn to the
# 728 DAY286 biofilm EV proteins shown in Figure 2A
# Want to compare presence and abundance of EV proteins between EVs from
# yeast-form and biofilm C. albicans

#### DAY286 yeast EV proteins ####
# filter out reverse, contaminant proteins, proteins with <2 unique peptides
# extract LFQ intensity columns and UniProt accessions
# log2 transform LFQ intensities
yeast_lfq <- yeast %>%
  filter(Reverse != "+",
         Potential.contaminant != "+",
         Unique.peptides >= 2) %>%
  convert_lfq(., yeast_exp)

# drop EV_1 and WCL_1 samples
yeast_lfq2 <- yeast_lfq[, !colnames(yeast_lfq) %in% c("EV_0", "WCL_0")]

# filter for proteins quantified in min 2/3 reps of yeast EV or WCL
# then convert to a data.frame and extract protein acessions from row names
yeast_filt2 <- filter_na2(yeast_lfq2, logic = "or", op = "<=",
                          pat1 = "EV", val1 = 1,
                          pat2 = "W", val2 = 1) %>%
  as.data.frame() %>%
  mutate(id = stringr::str_extract(rownames(.), "[^;]+")) %>%
  select(id, everything())

#### DAY286 biofilm EV proteins ####
# filter out reverse, contaminant proteins, proteins with <2 unique peptides
# extract LFQ intensity columns and UniProt accessions
# log2 transform LFQ intensities
biofilm_lfq <- biofilm %>%
  filter(Reverse != "+",
         Potential.contaminant != "+",
         Unique.peptides >= 2) %>%
  convert_lfq(., biofilm_exp)

# filter for proteins quantified in min 4/5 reps of biofilm EV or WCL
# then convert to a data.frame and extract protein acessions from row names
biofilm_filt <- filter_na2(biofilm_lfq, logic = "or", op = "<=",
                           pat1 = "EV", val1 = 1,
                           pat2 = "W", val2 = 1) %>%
  as.data.frame() %>%
  mutate(id = stringr::str_extract(rownames(.), "[^;]+")) %>%
  select(id, everything())

#### Combine yeast EV and biofilm EV data ####
# Combine the yeast and biofilm datasets by matching Uniprot accessions
# Remove the WCL columns
# Filter for proteins with at least 1 valid LFQ value
# Convert to numeric matrix
ev_comb <- full_join(yeast_filt2, biofilm_filt, by = "id") %>%
  select(id, contains("EV")) %>%
  tibble::column_to_rownames(var = "id") %>%
  filter_na(op = "<=", pat = "EV", val = 7) %>%
  as.matrix()

# Rename the columns to something useful
colnames(ev_comb) <- c("Y_EV1", "Y_EV2", "Y_EV3",
                       "B_EV1", "B_EV2", "B_EV3", "B_EV4", "B_EV5")

# Check the yeast EV and biofilm EV proteins match the counts in Figure 2A
b_ev <- filter_na(ev_comb, op = "<=", pat = "B_EV", val = 4)
y_ev <- filter_na(ev_comb, op = "<=", pat = "Y_EV", val = 2)

check_venn <- plot_venn(list("Y_EV" = y_ev, "B_EV" = b_ev),
                        use_uniprot = TRUE, type = "plot",
                        fontfamily = "sans",
                        cex = 0.8,
                        cat.fontfamily = "sans",
                        cat.just = list(c(0.5,1.7), c(0.5,1.5)),
                        margin = 0.05)

# Import the yeast EV vs biofilm EV experimental design required for limma
ev_exp <- read.table("inst/manuscript/supplementary_data/Y_EV_vs_B_EV_expdesign",
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

#### Data processing ####
# Normalise log2 LFQ data
ev_norm <- normalizeCyclicLoess(ev_comb)

# Won't impute with so many missing values
# Separate EV/WCL exclusive proteins (don't impute) from
# EV/WCL common proteins (impute)
ev_excl <- filter_na2(ev_norm, logic = "or", op = "==",
                      pat1 = "Y_EV", val1 = 3,
                      pat2 = "B_EV", val2 = 5)

ev_both <- filter_na2(ev_norm, logic = "and", op = "<=",
                      pat1 = "Y_EV", val1 = 2,
                      pat2 = "B_EV", val2 = 4)

# Use bpca method from pcaMethods for missing value imputation
ev_imp <- pcaMethods::pca(ev_both, method = "bpca")
ev_imp <- pcaMethods::completeObs(ev_imp)


ev_de <- rbind(ev_excl, ev_imp)


# see limma user guide section 9.2 for more info about DE
# create design matrix
ev_samp <- data.frame(T = c(rep("Y_EV", 3), rep("B_EV", 5)))

ev_design <- stats::model.matrix(~ 0 + T, data = ev_samp)
colnames(ev_design) <- c("B_EV", "Y_EV")

# define sample comparisons of interest
ev_contrasts <- c("Y_EV - B_EV")

# make all pair-wise comparisons between EV and WCL
# and perform limma::eBayes()
ev_efit <- limma_eBayes(ev_de, design = ev_design, contrasts = ev_contrasts)

# extract DE results
# adj.p.val cutoff is 0.01, log2FC cutoff is 1
ev_res <- get_results(efit = ev_efit,
                      mat = ev_de,
                      p_val = 0.01,
                      lfc = 1,
                      type = "individual")[[1]]


s6 <- ev_res %>%
  mutate(group = case_when(group == "EV up" ~ "EV sig",
                           group == "WCL up" ~ "WCL sig",
                           TRUE ~ as.character(group)),
         CGDID = match_id(UP_accession, uniprot,
                          "UP_accession", "CGDID"),
         Function = match_id(UP_accession, uniprot,
                             "UP_accession", "CGD_description"),
         Feature_name = match_id(UP_accession, uniprot,
                                 "UP_accession", "CGD_feature_name")) %>%
  select(UP_accession, CGDID, Feature_name, CGD_gene_name, Function,
         everything(), -Protein_name)

colnames(s6) <- c("Accession",
                  "CGDID",
                  "Feature name",
                  "Protein name",
                  "Function",
                  "Log2(LFQ) y_EV1",
                  "Log2(LFQ) y_EV2",
                  "Log2(LFQ) y_EV3",
                  "Log2(LFQ) b_EV1",
                  "Log2(LFQ) b_EV2",
                  "Log2(LFQ) b_EV3",
                  "Log2(LFQ) b_EV4",
                  "Log2(LFQ) b_EV5",
                  "EV log2(LFQ) mean",
                  "WCL log2(LFQ) mean",
                  "log2(fold change)",
                  "t",
                  "p-value",
                  "Adjusted p-value",
                  "B",
                  "Significant",
                  "Group")

s6[657, 4] <- "EVP1"

sheets <- list("DAY286_EV_Yeast_vs_Biofilm" = s6)
writexl::write_xlsx(sheets,
                    "inst/manuscript/supplementary_data/Supplementary_data_S6.xlsx")

#### Supplementary Data S7 ####
# Copy protein names for pasting into FungiFun2 online GO enrichment tool
# FungiFun2 parameters:
# 1. Strain = Candida albicans (strain SC5314/ATCC MYA-2876)
# 2. Classification ontology = GO
# 3. Input IDs = paste protein names here
# 4. Advanced options = default except significance level = 0.01

# Yeast EV exclusive proteins
# Write to clipboard, paste into FungiFun2, and submit
yev_ex <- ev_res %>%
  filter(group == "Y_EV ex") %>%
  pull(CGD_gene_name)

# Yeast EV enriched proteins
# Write to clipboard, paste into FungiFun2, and submit
yev_up <- ev_res %>%
  filter(group == "Y_EV up") %>%
  pull(CGD_gene_name) %>%
  stringi::stri_replace_all_regex(.,
                                  c("MDH1"),
                                  c("CaO19.7481"),
                                  vectorize_all = FALSE)

# Biofilm EV exclusive proteins
# Write to clipboard, paste into FungiFun2, and submit
bev_ex <- ev_res %>%
  filter(group == "B_EV ex") %>%
  pull(CGD_gene_name) %>%
  stringi::stri_replace_all_regex(.,
                                  c("ALS3", "ARO2", "HOM3",
                                    "OSM1", "PLB3", "FBP1",
                                    "PST2", "STE23"),
                                  c("CaO19.1816", "CaO19.1986", "CaO19.1235",
                                    "CaO19.6882", "CaO19.6594", "CaO19.6178",
                                    "CaO19.3612", "CaO19.5561"),
                                  vectorize_all = FALSE)

# Biofilm EV enriched proteins
# Write to clipboard, paste into FungiFun2, and submit
bev_up <- ev_res %>%
  filter(group == "B_EV up") %>%
  pull(CGD_gene_name) %>%
  stringi::stri_replace_all_regex(.,
                                  c("PEP1"),
                                  c("CaO19.3767"),
                                  vectorize_all = FALSE)

# Define function to rename FungiFun2 columns
rnm_cols <- function(df) {
  df <- df %>%
    rename("GO ID" = GO.ID,
           "GO term" = GO.term,
           "GO domain" = GO.type,
           "Proteins found" = Prot.found,
           "p-values" = p.val,
           "Adjusted p-value" = adj.p.val,
           "N protein found" = N.prot.found,
           "N protein GO term" = N.prot.cat,
           "N protein input" = N.prot.input,
           "GO term coverage ratio" = Cat.ratio,
           "Input coverage ratio" = Input.ratio)
}

# Import FungiFun2 functional (GO) enrichment results

# Yeast EV exclusive and enriched proteins
yev_go <- process_fungifun("inst/manuscript/supplementary_data/GO_supplementary/yev_ex_up.csv") %>%
  mutate(Prot.found = stringi::stri_replace_all_regex(.$Prot.found,
                                                      c("CaO19.7481"),
                                                      c("MDH1"),
                                                      vectorize_all = FALSE))
  rnm_cols()

# Biofilm EV exclusive and enriched proteins
bev_go <- process_fungifun("inst/manuscript/supplementary_data/GO_supplementary/bev_ex_up.csv") %>%
  mutate(Prot.found = stringi::stri_replace_all_regex(.$Prot.found,
                                                      c("CaO19.1816", "CaO19.1986", "CaO19.1235",
                                                        "CaO19.6882", "CaO19.6594", "CaO19.6178",
                                                        "CaO19.3612", "CaO19.5561", "CaO19.3767"),
                                                      c("ALS3", "ARO2", "HOM3",
                                                        "OSM1", "PLB3", "FBP1",
                                                        "PST2", "STE23", "PEP1"),
                                                      vectorize_all = FALSE))
  rnm_cols()

sheets <- list("Fig S5 DAY286 yeast" = yev_go,
                  "Fig S6 DAY286 biofilm" = bev_go)
writexl::write_xlsx(sheets,
                    "inst/manuscript/supplementary_data/supplementary_data_S7.xlsx")
