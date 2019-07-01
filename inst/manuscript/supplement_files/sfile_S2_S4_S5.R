#### Setup ####
# need writexl package to write output to .xlsx
requireNamespace("writexl", quietly = TRUE)

# load packages
library(dplyr)
library(candidaev)
library(limma)

# set seed for rng
# ensures results output is identical to manuscript
set.seed(1)

#### DAY286 yeast ####
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

# filter for proteins quantified in min 2/3 reps of EV or WCL
yeast_filt2 <- filter_na2(yeast_lfq2, logic = "or", op = "<=",
                          pat1 = "EV", val1 = 1,
                          pat2 = "W", val2 = 1)

# normalise LFQ intensities
yeast_norm2 <- normalizeCyclicLoess(yeast_filt2)

# filter for proteins identified in min 1/3 reps of EV
# define 'EV proteins'
y_ev <- filter_na(yeast_norm2, op = "<=",
                  pat = "EV", val = 2)

# filter for proteins identified in min 1/3 reps of WCL
# define 'WCL proteins'
y_wcl <- filter_na(yeast_norm2, op = "<=",
                   pat = "W", val = 2)

# can't impute with so many missing values
# separate EV/WCL exclusive proteins (don't impute) from
# EV/WCL common proteins (impute)
yeast_excl <- filter_na2(yeast_norm2, logic = "or", op = "==",
                         pat1 = "EV", val1 = 3,
                         pat2 = "W", val2 = 3)

yeast_both <- filter_na2(yeast_norm2, logic = "and", op = "<=",
                         pat1 = "EV", val1 = 2,
                         pat2 = "W", val2 = 2)

# proteins with missing values tend to have lower intensity
# therefore proteins are MNAR, close to detection limit
# use left censored imputation method
yeast_imp <- impute_QRILC(yeast_both)

# recombine imputed proteins and non-imputed proteins in matrix
yeast_de <- rbind(yeast_excl, yeast_imp)

# see limma user guide section 9.2 for more info about DE
# create design matrix
y_samp <- data.frame(T = (rep(c("EV", "WCL"), each = 3)))

y_design <- stats::model.matrix(~ 0 + T, data = y_samp)
colnames(y_design) <- c("EV", "WCL")

# define sample comparisons of interest
y_contrasts <- c("EV - WCL")

# make all pair-wise comparisons between EV and WCL
# and perform limma::eBayes()
y_efit <- limma_eBayes(yeast_de, design = y_design, contrasts = y_contrasts)

# extract DE results
yeast_res <- get_results(efit = y_efit,
                         mat = yeast_de,
                         p_val = 0.01,
                         lfc = 0,
                         type = "individual")[[1]]

#### DAY286 biofilm ####
# filter out reverse, contaminant proteins, proteins with <2 unique peptides
# extract LFQ intensity columns and UniProt accessions
# log2 transform LFQ intensities
biofilm_lfq <- biofilm %>%
  filter(Reverse != "+",
         Potential.contaminant != "+",
         Unique.peptides >= 2) %>%
  convert_lfq(., biofilm_exp)

# filter for proteins quantified in min 4/5 reps of EV or WCL
biofilm_filt <- filter_na2(biofilm_lfq, logic = "or", op = "<=",
                           pat1 = "EV", val1 = 1,
                           pat2 = "W", val2 = 1)

# normalise LFQ intensities
biofilm_norm <- normalizeCyclicLoess(biofilm_filt)

# filter for proteins identified in min 1/5 reps of EV
# define 'EV proteins'
b_ev <- filter_na(biofilm_norm, op = "<=",
                  pat = "EV", val = 4)

# filter for proteins identified in min 1/5 reps of WCL
# define 'WCL proteins'
b_wcl <- filter_na(biofilm_norm, op = "<=",
                   pat = "W", val = 4)

# can't impute with so many missing values
# filter for proteins with 4-5 NA values in EV or WCL (don't impute)
biofilm_excl <- filter_na2(biofilm_norm, logic = "or", op = ">=",
                           pat1 = "EV", val1 = 4,
                           pat2 = "W", val2 = 4)

# filter for proteins with min 3 valid values in EV and WCL (impute)
biofilm_both <- filter_na2(biofilm_norm, logic = "and", op = "<=",
                           pat1 = "EV", val1 = 3,
                           pat2 = "W", val2 = 3)

# proteins with missing values tend to have lower intensity
# therefore proteins are MNAR, close to detection limit
# use left censored imputation method
biofilm_imp <- impute_QRILC(biofilm_both)

# recombine imputed proteins and non-imputed proteins in matrix
biofilm_de <- rbind(biofilm_excl, biofilm_imp)

# see limma user guide section 9.2 for more info about DE
# create design matrix
b_samp <- data.frame(T = (rep(c("EV", "WCL"), each = 5)))

b_design <- stats::model.matrix(~ 0 + T, data = b_samp)
colnames(b_design) <- c("EV", "WCL")

# define sample comparisons of interest
b_contrasts <- c("EV - WCL")

# make all pair-wise comparisons between EV and WCL
# and perform limma::eBayes()
b_efit <- limma_eBayes(biofilm_de, design = b_design, contrasts = b_contrasts)

# extract DE results
biofilm_res <- get_results(efit = b_efit,
                           mat = biofilm_de,
                           p_val = 0.01,
                           lfc = 0,
                           type = "individual")[[1]]

#### ATCC ####
# filter out reverse, contaminant proteins, proteins with <2 unique peptides
# extract LFQ intensity columns and UniProt accessions
# log2 transform LFQ intensities
atcc_lfq <- atcc %>%
  filter(Reverse != "+",
         Potential.contaminant != "+",
         Unique.peptides >= 2) %>%
  convert_lfq(., atcc_exp)

# filter for proteins quantified in min 2/3 reps of:
# A1 EV or A1 WCL or A9 EV or A9 WCL
atcc_filt <- filter_na4(atcc_lfq, "or", "<=",
                        pat1 = "A10231_EV", 1,
                        pat2 = "A10231_W", 1,
                        pat3 = "A90028_EV", 1,
                        pat4 = "A90028_W", 1)

# normalise LFQ intensities
atcc_norm <- normalizeCyclicLoess(atcc_filt)

#### ATCC10231 ####
# filter for proteins quantified in min 2/3 reps of EV or WCL
a1_norm <- filter_na2(atcc_norm, logic = "or", op = "<=",
                      pat1 = "A10231_EV", val1 = 1,
                      pat2 = "A10231_W", val2 = 1)
a1_norm <- a1_norm[, -c(7:12)]

# filter for proteins quantified in min 1/3 reps of EV
# define 'EV proteins'
a1_ev <- filter_na(a1_norm, op = "<=", pat = "EV", val = 2)

# filter for proteins quantified in min 1/3 reps of WCL
# define 'WCL proteins'
a1_wcl <- filter_na(a1_norm, op = "<=", pat = "W", val = 2)

# can't impute with so many missing values
# separate EV/WCL exclusive proteins (don't impute) from
# EV/WCL common proteins (impute)
a1_excl <- filter_na2(a1_norm, logic = "or", op = "==",
                      pat1 = "EV", val1 = 3,
                      pat2 = "W", val2 = 3)

a1_both <- filter_na2(a1_norm, logic = "and", op = "<=",
                      pat1 = "EV", val1 = 2,
                      pat2 = "W", val2 = 2)

# proteins with missing values tend to have lower intensity
# therefore proteins are MNAR, close to detection limit
# use left censored imputation method
a1_imp <- impute_QRILC(a1_both)

# recombine imputed proteins and non-imputed proteins in matrix
a1_de <- rbind(a1_excl, a1_imp)
colnames(a1_de) <- gsub("A10231", "A1", colnames(a1_de))

# see limma user guide section 9.2 for more info about DE
# create design matrix
a1_samp <- data.frame(T = (rep(c("EV", "WCL"), each = 3)))

a1_design <- stats::model.matrix(~ 0 + T, data = a1_samp)
colnames(a1_design) <- c("A1_EV", "A1_W")

# define sample comparisons of interest
a1_contrasts <- c("A1_EV - A1_W")

# make all pair-wise comparisons between EV and WCL
# and perform limma::eBayes()
a1_efit <- limma_eBayes(a1_de, design = a1_design, contrasts = a1_contrasts)

# extract DE results
a1_res <- get_results(efit = a1_efit,
                      mat = a1_de,
                      p_val = 0.01,
                      lfc = 0,
                      type = "individual")[[1]]

#### ATCC90028 ####
# filter for proteins quantified in min 2/3 reps of EV or WCL
a9_norm <- filter_na2(atcc_norm, logic = "or", op = "<=",
                      pat1 = "A90028_EV", val1 = 1,
                      pat2 = "A90028_W", val2 = 1)
a9_norm <- a9_norm[, -c(1:6)]

# filter for proteins quantified in min 1/3 reps of EV
# define 'EV proteins'
a9_ev <- filter_na(a9_norm, op = "<=", pat = "EV", val = 2)

# filter for proteins quantified in min 1/3 reps of WCL
# define 'WCL proteins'
a9_wcl <- filter_na(a9_norm, op = "<=", pat = "W", val = 2)

# can't impute with so many missing values
# separate EV/WCL exclusive proteins (don't impute) from
# EV/WCL common proteins (impute)
a9_excl <- filter_na2(a9_norm, logic = "or", op = "==",
                      pat1 = "EV", val1 = 3,
                      pat2 = "W", val2 = 3)

a9_both <- filter_na2(a9_norm, logic = "and", op = "<=",
                      pat1 = "EV", val1 = 2,
                      pat2 = "W", val2 = 2)

# proteins with missing values tend to have lower intensity
# therefore proteins are MNAR, close to detection limit
# use left censored imputation method
a9_imp <- impute_QRILC(a9_both)

# recombine imputed proteins and non-imputed proteins in matrix
a9_de <- rbind(a9_excl, a9_imp)
colnames(a9_de) <- gsub("A90028", "A9", colnames(a9_de))

# see limma user guide section 9.2 for more info about DE
# create design matrix
a9_samp <- data.frame(T = (rep(c("EV", "WCL"), each = 3)))

a9_design <- stats::model.matrix(~ 0 + T, data = a9_samp)
colnames(a9_design) <- c("A9_EV", "A9_W")

# define sample comparisons of interest
a9_contrasts <- c("A9_EV - A9_W")

# make all pair-wise comparisons between EV and WCL
# and perform limma::eBayes()
a9_efit <- limma_eBayes(a9_de, design = a9_design, contrasts = a9_contrasts)

# extract DE results
a9_res <- get_results(efit = a9_efit,
                      mat = a9_de,
                      p_val = 0.01,
                      lfc = 0,
                      type = "individual")[[1]]

#### Supplemental File S2 ####
# DAY286 yeast results
s2_a <- yeast_res %>%
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
colnames(s2_a) <- c("Accession",
                    "CGDID",
                    "Feature name",
                    "Protein name",
                    "Function",
                    "Log2(LFQ) EV1",
                    "Log2(LFQ) EV2",
                    "Log2(LFQ) EV3",
                    "Log2(LFQ) WCL1",
                    "Log2(LFQ) WCL2",
                    "Log2(LFQ) WCL3",
                    "EV log2(LFQ) mean",
                    "WCL log2(LFQ) mean",
                    "log2(fold change)",
                    "t",
                    "p-value",
                    "Adjusted p-value",
                    "B",
                    "Significant",
                    "Group")
s2_a[102, 4] <- "EVP1"

# ATCC90028 yeast results
s2_b <- a9_res %>%
  mutate(group = case_when(group == "A9_EV up" ~ "EV sig",
                           group == "A9_W up" ~ "WCL sig",
                           group == "A9_EV ex" ~ "EV ex",
                           group == "A9_W ex" ~ "WCL ex",
                           TRUE ~ as.character(group)),
         CGDID = match_id(UP_accession, uniprot,
                          "UP_accession", "CGDID"),
         Function = match_id(UP_accession, uniprot,
                             "UP_accession", "CGD_description"),
         Feature_name = match_id(UP_accession, uniprot,
                                 "UP_accession", "CGD_feature_name")) %>%
  select(UP_accession, CGDID, Feature_name, CGD_gene_name, Function,
         everything(), -Protein_name)
colnames(s2_b) <- c("Accession",
                    "CGDID",
                    "Feature name",
                    "Protein name",
                    "Function",
                    "Log2(LFQ) EV1",
                    "Log2(LFQ) EV2",
                    "Log2(LFQ) EV3",
                    "Log2(LFQ) WCL1",
                    "Log2(LFQ) WCL2",
                    "Log2(LFQ) WCL3",
                    "EV log2(LFQ) mean",
                    "WCL log2(LFQ) mean",
                    "log2(fold change)",
                    "t",
                    "p-value",
                    "Adjusted p-value",
                    "B",
                    "Significant",
                    "Group")
s2_b[131, 4] <- "EVP1"

# ATCC10231 yeast results
s2_c <- a1_res %>%
  mutate(group = case_when(group == "A1_EV up" ~ "EV sig",
                           group == "A1_W up" ~ "WCL sig",
                           group == "A1_EV ex" ~ "EV ex",
                           group == "A1_W ex" ~ "WCL ex",
                           TRUE ~ as.character(group)),
         CGDID = match_id(UP_accession, uniprot,
                          "UP_accession", "CGDID"),
         Function = match_id(UP_accession, uniprot,
                             "UP_accession", "CGD_description"),
         Feature_name = match_id(UP_accession, uniprot,
                                 "UP_accession", "CGD_feature_name")) %>%
  select(UP_accession, CGDID, Feature_name, CGD_gene_name, Function,
         everything(), -Protein_name)
colnames(s2_c) <- c("Accession",
                    "CGDID",
                    "Feature name",
                    "Protein name",
                    "Function",
                    "Log2(LFQ) EV1",
                    "Log2(LFQ) EV2",
                    "Log2(LFQ) EV3",
                    "Log2(LFQ) WCL1",
                    "Log2(LFQ) WCL2",
                    "Log2(LFQ) WCL3",
                    "EV log2(LFQ) mean",
                    "WCL log2(LFQ) mean",
                    "log2(fold change)",
                    "t",
                    "p-value",
                    "Adjusted p-value",
                    "B",
                    "Significant",
                    "Group")
s2_c[127, 4] <- "EVP1"

# DAY286 biofilm results
s2_d <- biofilm_res %>%
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
colnames(s2_d) <- c("Accession",
                    "CGDID",
                    "Feature name",
                    "Protein name",
                    "Function",
                    "Log2(LFQ) EV1",
                    "Log2(LFQ) EV2",
                    "Log2(LFQ) EV3",
                    "Log2(LFQ) EV4",
                    "Log2(LFQ) EV5",
                    "Log2(LFQ) WCL1",
                    "Log2(LFQ) WCL2",
                    "Log2(LFQ) WCL3",
                    "Log2(LFQ) WCL4",
                    "Log2(LFQ) WCL5",
                    "EV log2(LFQ) mean",
                    "WCL log2(LFQ) mean",
                    "log2(fold change)",
                    "t",
                    "p-value",
                    "Adjusted p-value",
                    "B",
                    "Significant",
                    "Group")
s2_d[67, 4] <- "EVP1"

#### export ####
s2_sheets <- list("DAY286 yeast" = s2_a,
                  "ATCC90028 yeast" = s2_b,
                  "ATCC10231 yeast" = s2_c,
                  "DAY286 biofilm" = s2_d)
writexl::write_xlsx(s2_sheets, "inst/manuscript/supplement_files/sfile_S2.xlsx")

#### Supplemental File S4 ####
# Venn diagram partition protein lists

#### Figure 4A ####
# figure 4a EV venn
s4_a_comp <- list("DAY Y" = rownames(y_ev),
                  "ATCC90028" = rownames(a9_ev),
                  "ATCC10231" = rownames(a1_ev),
                  "DAY B" = rownames(b_ev))

s4_a <- plot_venn(vlist = s4_a_comp, use_uniprot = FALSE, type = "df") %>%
  mutate(Proteins = lapply(..values.., function(x) match_id(x, uniprot,
                                                            "UP_accession",
                                                            "CGD_gene_name")),
         Proteins = unlist(lapply(Proteins,
                                  function(x) paste(x, collapse = " ")))) %>%
  select("DAY Y", ATCC90028, ATCC10231, "DAY B",
         ..set.., ..count.., Proteins, -..values..) %>%
  rename("Set" = ..set.., "Count" = ..count..)

#### Figure 4B ####
# figure 4b WCL venn
s4_b_comp <- list("DAY Y" = rownames(y_wcl),
                  "ATCC90028" = rownames(a9_wcl),
                  "ATCC10231" = rownames(a1_wcl),
                  "DAY B" = rownames(b_wcl))
s4_b <- plot_venn(vlist = s4_b_comp, use_uniprot = FALSE, type = "df") %>%
  mutate(Proteins = lapply(..values.., function(x) match_id(x, uniprot,
                                                            "UP_accession",
                                                            "CGD_gene_name")),
         Proteins = unlist(lapply(Proteins,
                                  function(x) paste(x, collapse = " ")))) %>%
  select("DAY Y", ATCC90028, ATCC10231, "DAY B",
         ..set.., ..count.., Proteins, -..values..) %>%
  rename("Set" = ..set.., "Count" = ..count..)

#### Figure 6A ####
# figure 6a EV marker venn
s4_c_comp <- list("DAY Y" = yeast_res %>%
                    filter(group == "EV up" | group == "EV ex") %>%
                    pull(CGD_gene_name),
                  "ATCC90028" = a9_res %>%
                    filter(group == "A9_EV up" | group == "A9_EV ex") %>%
                    pull(CGD_gene_name),
                  "ATCC10231" = a1_res %>%
                    filter(group == "A1_EV up" | group == "A1_EV ex") %>%
                    pull(CGD_gene_name),
                  "DAY B" = biofilm_res %>%
                    filter(group == "EV up" | group == "EV ex") %>%
                    pull(CGD_gene_name))

s4_c <- plot_venn(vlist = s4_c_comp, use_uniprot = FALSE, type = "df") %>%
  mutate(Proteins = unlist(lapply(..values..,
                                  function(x) paste(x, collapse = " ")))) %>%
  select("DAY Y", ATCC90028, ATCC10231, "DAY B",
         ..set.., ..count.., Proteins, -..values..) %>%
  rename("Set" = ..set.., "Count" = ..count..)

#### Figure 6B ####
# figure 6b WCL marker venn
s4_d_comp <- list("DAY Y" = yeast_res %>%
                    filter(group == "WCL up" | group == "WCL ex") %>%
                    pull(CGD_gene_name),
                  "ATCC90028" = a9_res %>%
                    filter(group == "A9_W up" | group == "A9_W ex") %>%
                    pull(CGD_gene_name),
                  "ATCC10231" = a1_res %>%
                    filter(group == "A1_W up" | group == "A1_W ex") %>%
                    pull(CGD_gene_name),
                  "DAY B" = biofilm_res %>%
                    filter(group == "WCL up" | group == "WCL ex") %>%
                    pull(CGD_gene_name))

s4_d <- plot_venn(vlist = s4_d_comp, use_uniprot = FALSE, type = "df") %>%
  mutate(Proteins = unlist(lapply(..values..,
                                  function(x) paste(x, collapse = " ")))) %>%
  select("DAY Y", ATCC90028, ATCC10231, "DAY B",
         ..set.., ..count.., Proteins, -..values..) %>%
  rename("Set" = ..set.., "Count" = ..count..)

#### export ####
s4_sheets <- list("Figure 4A" = s4_a,
                  "Figure 4B" = s4_b,
                  "Figure 6A" = s4_c,
                  "Figure 6B" = s4_d)
writexl::write_xlsx(s4_sheets, "inst/manuscript/supplement_files/sfile_S4.xlsx")

#### Supplemental File S5 ####
# Figure 5 protein clusters
s5_comp <- s4_a_comp

s5_overlap <- plot_venn(vlist = s5_comp, use_uniprot = FALSE, type = "df") %>%
  filter(..count.. == 396) %>%
  pull(..values..) %>%
  unlist() %>%
  match_id(., uniprot, "UP_accession", "CGD_gene_name")

s5_table <- a9_res %>%
  select(logFC, CGD_gene_name) %>%
  full_join(., select(a1_res, logFC, CGD_gene_name),
            by = "CGD_gene_name") %>%
  full_join(., select(yeast_res, logFC, CGD_gene_name),
            by = "CGD_gene_name") %>%
  full_join(., select(biofilm_res, logFC, CGD_gene_name),
            by = "CGD_gene_name") %>%
  rename(a9_lfc = logFC.x,
         a1_lfc = logFC.y,
         y_lfc = logFC.x.x,
         b_lfc = logFC.y.y) %>%
  select(CGD_gene_name, everything()) %>%
  filter(CGD_gene_name %in% s5_overlap)

# filter for rows with min 1/4 valid values
# otherwise clustering will not work
s5_table_filt <- s5_table %>%
  filter_at(.vars = vars(contains("lfc")),
            any_vars(!is.na(.))) %>%
  tibble::column_to_rownames(var = "CGD_gene_name") %>%
  as.matrix()

# excluded proteins (aka cluster 6)
s5_table_excl <- s5_table %>%
  filter_at(.vars = vars(contains("lfc")),
            all_vars(is.na(.))) %>%
  mutate(cluster = 6) %>%
  rename("id" = CGD_gene_name) %>%
  arrange(id)

# generate S5
s5 <- plot_heatmap(mt = s5_table_filt,
                   plot = FALSE,
                   df = TRUE,
                   data_type = "log2fc",
                   clust_fun = "gower",
                   split_type = "cutree",
                   k = 5,
                   cluster_split = FALSE) %>%
  mutate(cluster = recode(cluster,
                          "1" = 3, "2" = 1, "3" = 5, "4" = 4, "5" = 2)) %>%
  arrange(cluster) %>%
  select(-order) %>%
  bind_rows(s5_table_excl) %>%
  mutate(Accession = match_id(id, uniprot, "CGD_gene_name", "UP_accession"),
         CGDID = match_id(id, uniprot, "CGD_gene_name", "CGDID"),
         Function = match_id(id, uniprot,
                             "CGD_gene_name", "CGD_description"),
         Feature_name = match_id(id, uniprot,
                                 "CGD_gene_name", "CGD_feature_name")) %>%
  select(Accession, CGDID, Feature_name, id, Function,
         b_lfc, a1_lfc, a9_lfc, y_lfc, cluster) %>%
  rename("Protein name" = id,
         "Feature name" = Feature_name,
         "DAY286 biofilm log2FC" = b_lfc,
         "ATCC10231 log2FC" = a1_lfc,
         "ATCC90028 log2FC" = a9_lfc,
         "DAY286 yeast log2FC" = y_lfc,
         "Cluster" = cluster)
s5[390, 4] <- "EVP1"

#### export ####
writexl::write_xlsx(list("Fig. 5 Heatmap clusters" = s5),
                    "inst/manuscript/supplement_files/sfile_S5.xlsx")
