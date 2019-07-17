#### Supplementary Data S3 ####
# FungiFun2 functional enrichment results

# need writexl package to write output to .xlsx
requireNamespace("writexl", quietly = TRUE)

# load packages
library(dplyr)
library(candidaev)

# define function to rename FungiFun2 columns
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

#### Figure 4 ####
## import FungiFun2 results ##
s3_a <- process_fungifun("inst/manuscript/figures/f3_GO_DAY_Y.csv") %>%
  mutate(Prot.found = gsub("CaO19.4937", "CHS3", Prot.found),
         Prot.found = gsub("CaO19.3767", "PEP1", Prot.found)) %>%
    rnm_cols()

s3_b <- process_fungifun("inst/manuscript/figures/f3_GO_ATCC90028.csv") %>%
  mutate(Prot.found = stringi::stri_replace_all_regex(.$Prot.found,
                                                      c("CaO19.4937",
                                                        "CaO19.3767",
                                                        "CaO19.1816",
                                                        "CaO19.6594",
                                                        "CaO19.9017",
                                                        "CaO19.2347",
                                                        "CaO19.3803",
                                                        "CaO19.1995",
                                                        "CaO19.4255",
                                                        "CaO19.2651",
                                                        "CaO19.8937"),
                                                      c("CHS3", "PEP1",
                                                        "ALS3", "PLB3",
                                                        "PLB4.5", "MNN2",
                                                        "MNN22", "MNN24",
                                                        "ECM331", "CAM1-1",
                                                        "FCY21"),
                                                      vectorize_all = FALSE)) %>%
  rnm_cols()

s3_c <- process_fungifun("inst/manuscript/figures/f3_GO_ATCC10231.csv") %>%
  mutate(Prot.found = stringi::stri_replace_all_regex(.$Prot.found,
                                                      c("CaO19.4937",
                                                        "CaO19.3767",
                                                        "CaO19.1816",
                                                        "CaO19.6594",
                                                        "CaO19.9017",
                                                        "CaO19.2347",
                                                        "CaO19.3803",
                                                        "CaO19.1995",
                                                        "CaO19.4255",
                                                        "CaO19.2651"),
                                                      c("CHS3", "PEP1",
                                                        "ALS3", "PLB3",
                                                        "PLB4.5", "MNN2",
                                                        "MNN22", "MNN24",
                                                        "ECM331", "CAM1-1"),
                                                      vectorize_all = FALSE)) %>%
  rnm_cols()

s3_d <- process_fungifun("inst/manuscript/figures/f3_GO_DAY_B.csv") %>%
  mutate(Prot.found = stringi::stri_replace_all_regex(.$Prot.found,
                                                      c("CaO19.4937",
                                                        "CaO19.3767",
                                                        "CaO19.1816",
                                                        "CaO19.6594",
                                                        "CaO19.9017"),
                                                      c("CHS3", "PEP1",
                                                        "ALS3", "PLB3",
                                                        "PLB4.5"),
                                                      vectorize_all = FALSE)) %>%
  rnm_cols()

#### Figure 5 ####
## import FungiFun2 results
s3_e <- process_fungifun("inst/manuscript/figures/f5_GO_clust1_2.csv") %>%
  mutate(Prot.found = gsub("CaO19.3767", "PEP1", Prot.found)) %>%
  rnm_cols()

s3_f <- process_fungifun("inst/manuscript/figures/f5_GO_clust3.csv") %>%
  mutate(Prot.found = gsub("CaO19.4937", "CHS3", Prot.found)) %>%
  rnm_cols()

s3_g <- process_fungifun("inst/manuscript/figures/f5_GO_clust4.csv") %>%
  rnm_cols()

s3_h <- process_fungifun("inst/manuscript/figures/f5_GO_clust5.csv") %>%
  mutate(Prot.found = gsub("CaO19.3002", "RPS1", Prot.found)) %>%
  rnm_cols()

s3_i <- process_fungifun("inst/manuscript/figures/f5_GO_clust6.csv") %>%
  mutate(Prot.found = stringi::stri_replace_all_regex(.$Prot.found,
                                                      c("CaO19.5746",
                                                        "CaO19.7481",
                                                        "CaO19.7417"),
                                                      c("ALA1", "MDH1",
                                                        "TSA1"),
                                                      vectorize_all = FALSE)) %>%
  rnm_cols()

s3_j <- process_fungifun("inst/manuscript/figures/f5_GO_clust7.csv") %>%
  rnm_cols()

s3_k <- process_fungifun("inst/manuscript/figures/f5_GO_clust8.csv") %>%
  rnm_cols()


#### export ####
s3_sheets <- list("Fig 3a DAY286 y" = s3_a,
                  "Fig 3b ATCC90028 y" = s3_b,
                  "Fig 3c ATCC10231 y" = s3_c,
                  "Fig 3d DAY286 b" = s3_d,
                  "Fig 5 Cluster 1+2" = s3_e,
                  "Fig 5 Cluster 3" = s3_f,
                  "Fig 5 Cluster 4" = s3_g,
                  "Fig 5 Cluster 5" = s3_h,
                  "Fig 5 Cluster 6" = s3_i,
                  "Fig 5 Cluster 7" = s3_j,
                  "Fig 5 Cluster 8" = s3_k)
writexl::write_xlsx(s3_sheets,
                    "inst/manuscript/supplementary_data/supplementary_data_S3.xlsx")
