#### Supplemental File S1 ####
# table of proteins
# post-filtration but pre-log2 transformation
# pre-normalisation, pre-imputation etc.

# need writexl package to write output to .xlsx
requireNamespace("writexl", quietly = TRUE)

# load packages
library(dplyr)
library(candidaev)

make_s1 <- function(df) {
  result <- df %>%
    filter(Reverse != "+",
           Potential.contaminant != "+",
           Unique.peptides >= 2) %>%
    select(Majority.protein.IDs,
           Q.value,
           Score,
           matches("^Unique.peptides.EV"),
           matches("^Unique.peptides.W"),
           matches("Sequence.coverage.EV"),
           matches("Sequence.coverage.W"),
           matches("LFQ.intensity.EV"),
           matches("LFQ.intensity.W"),
           matches("MS.MS.count.EV"),
           matches("MS.MS.count.W")) %>%
    mutate(Protein.name = match_id(Majority.protein.IDs,
                                   uniprot,
                                   "UP_accession",
                                   "CGD_gene_name"),
           Feature.name = match_id(Majority.protein.IDs,
                                   uniprot,
                                   "UP_accession",
                                   "CGD_feature_name"),
           Function = match_id(Majority.protein.IDs,
                               uniprot,
                               "UP_accession",
                               "CGD_description"),
           CGDID = match_id(Majority.protein.IDs,
                            uniprot,
                            "UP_accession",
                            "CGDID"),
           First.protein = stringr::str_extract(Majority.protein.IDs, "[^;]+"),
           "Mass.(kDa)" = as.numeric(gsub(",", "",
                                         match_id(First.protein,
                                                  uniprot,
                                                  "UP_accession",
                                                  "Mass_(kDa)",
                                                  concat = FALSE)))) %>%
    select(Majority.protein.IDs,
           CGDID,
           Feature.name,
           Protein.name,
           Function,
           "Mass.(kDa)",
           Q.value,
           Score,
           everything(),
           -First.protein) %>%
    rename_all(list(~ stringr::str_replace_all(., "\\.", " ")))
}

#### DAY286 yeast ####
s1_a <- yeast %>%
  select(-contains("C3")) %>%
  filter_zero2(., logic = "or", "<=",
               pat1 = "LFQ.*EV", 1,
               pat2 = "LFQ.*W", 1) %>%
  rename_all(list(~ stringr::str_replace_all(., "C6.EV", "EV1"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "C7.EV", "EV2"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "C8.EV", "EV3"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "C6.WCL", "WCL1"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "C7.WCL", "WCL2"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "C8.WCL", "WCL3"))) %>%
  make_s1(.)
s1_a[215, 4] <- "EVP1"

#### ATCC ####
atcc_filt <- atcc %>%
  filter(Reverse != "+",
         Potential.contaminant != "+",
         Unique.peptides >= 2) %>%
  filter_zero4(., logic = "or", "<=",
               pat1 = "LFQ.*A1_EV", 1,
               pat2 = "LFQ.*A1_W", 1,
               pat3 = "LFQ.*A9_EV", 1,
               pat4 = "LFQ.*A9_W", 1)

#### ATCC90028 ####
s1_b <- atcc_filt %>%
  filter_zero2(., logic = "or", op = "<=",
               pat1 = "LFQ.*A9_EV", val1 = 1,
               pat2 = "LFQ.*A9_W", val2 = 1) %>%
  select(-contains("A1")) %>%
  rename_all(list(~ stringr::str_replace_all(., "A9_", ""))) %>%
  rename_all(list(~ stringr::str_replace_all(., "W", "WCL"))) %>%
  make_s1(.)
s1_b[352, 4] <- "EVP1"

#### ATCC10231 ####
s1_c <- atcc_filt %>%
  filter_zero2(., logic = "or", op = "<=",
               pat1 = "LFQ.*A1_EV", val1 = 1,
               pat2 = "LFQ.*A1_W", val2 = 1) %>%
  select(-contains("A9")) %>%
  rename_all(list(~ stringr::str_replace_all(., "A1_", ""))) %>%
  rename_all(list(~ stringr::str_replace_all(., "W", "WCL"))) %>%
  make_s1()
s1_c[280, 4] <- "EVP1"

#### DAY286 biofilm ####
s1_d <- biofilm %>%
  filter_zero2(., logic = "or", "<=",
               pat1 = "LFQ.*EV", 1,
               pat2 = "LFQ.*W", 1) %>%
  rename_all(list(~ stringr::str_replace_all(., ".b(?=EV|W)", "."))) %>%
  rename_all(list(~ stringr::str_replace_all(., "(?<=EV|W)3", "1"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "(?<=EV|W)4", "2"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "(?<=EV|W)5", "3"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "(?<=EV|W)6", "4"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "(?<=EV|W)7", "5"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "W", "WCL"))) %>%
  make_s1(.)
s1_d[200, 4] <- "EVP1"

#### export ####
s1_sheets <- list("DAY286 yeast" = s1_a,
                  "ATCC90028 yeast" = s1_b,
                  "ATCC10231 yeast" = s1_c,
                  "DAY286 biofilm" = s1_d)
writexl::write_xlsx(s1_sheets, "inst/manuscript/supplement_files/sfile_S1.xlsx")
