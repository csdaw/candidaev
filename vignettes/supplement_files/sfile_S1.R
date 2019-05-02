#### Supplemental File S1 ####
# table of proteins
# post-filtration but pre-log2 transformation
# pre-normalisation, pre-imputation etc.

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
           matches("LFQ.intensity.EV"),
           matches("LFQ.intensity.W"),
           matches("MS.MS.count.EV"),
           matches("MS.MS.count.W")) %>%
    mutate(Protein.name = match_id(Majority.protein.IDs,
                                   uniprot,
                                   "UP_accession",
                                   "CGD_gene_name"),
           Function = match_id(Majority.protein.IDs,
                               uniprot,
                               "UP_accession",
                               "CGD_description"),
           CGDID = match_id(Majority.protein.IDs,
                            uniprot,
                            "UP_accession",
                            "CGDID")) %>%
    select(Majority.protein.IDs,
           CGDID,
           Protein.name,
           Function,
           Q.value,
           Score,
           everything()) %>%
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
  rename_all(list(~ stringr::str_replace_all(., "C6.WCL", "W1"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "C7.WCL", "W2"))) %>%
  rename_all(list(~ stringr::str_replace_all(., "C8.WCL", "W3"))) %>%
  make_s1(.)

#### ATCC90028 ####
s1_b <- atcc %>%
  filter_zero4(., logic = "or", "<=",
               pat1 = "LFQ.*A1_EV", 1,
               pat2 = "LFQ.*A1_W", 1,
               pat3 = "LFQ.*A9_EV", 1,
               pat4 = "LFQ.*A9_W", 1) %>%
  select(-contains("A1")) %>%
  rename_all(list(~ stringr::str_replace_all(., "A9_", ""))) %>%
  make_s1(.) %>%
  filter_val(., op = ">=", pat = "LFQ", val = 1)

#### ATCC10231 ####
s1_c <- atcc %>%
  filter_zero4(., logic = "or", "<=",
              pat1 = "LFQ.*A1_EV", 1,
              pat2 = "LFQ.*A1_W", 1,
              pat3 = "LFQ.*A9_EV", 1,
              pat4 = "LFQ.*A9_W", 1) %>%
  select(-contains("A9")) %>%
  rename_all(list(~ stringr::str_replace_all(., "A1_", ""))) %>%
  make_s1() %>%
  filter_zero(., op = "<=", pat = "LFQ", val = 5)

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
  make_s1(.)

#### export ####
s1_sheets <- list("DAY286 yeast" = s1_a,
                  "ATCC90028" = s1_b,
                  "ATCC10231" = s1_c,
                  "DAY286 biofilm" = s1_d)
writexl::write_xlsx(s1_sheets, "vignettes/supplement_files/sfile_S1.xlsx")
