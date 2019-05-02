#### Supplemental File S3 ####
# FungiFun2 functional enrichment results

#### Figure 4 ####
## define function to process the .csv files output by FungiFun2
process_fungifun <- function(filename) {
  df <- read.csv(filename, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  df$N.prot.found <- as.numeric(gsub(" /.*", "", df$X..genes...category))
  df$N.prot.cat <- as.numeric(gsub(".* / ", "", df$X..genes...category))
  df$N.prot.input <- as.numeric(gsub(".* / ", "", df$X..genes...input))
  df$Cat.ratio <- df$N.prot.found / df$N.prot.cat
  df$Input.ratio <- df$N.prot.found / df$N.prot.input

  df <- df[, -c(7:8)]

  colnames(df)[1:6] <- c("GO.ID", "GO.term", "GO.type", "Prot.found", "p.val", "adj.p.val")

  df$Prot.found <- as.character(df$Prot.found)
  df$Prot.found <- gsub(" | ", " ", df$Prot.found, fixed = TRUE)

  colnames(df) <- c("GO ID",
                    "GO term",
                    "GO domain",
                    "Proteins_found",
                    "p-value",
                    "Adjusted p-value",
                    "N protein found",
                    "N protein GO term",
                    "N protein input",
                    "GO term coverage ratio",
                    "Input coverage ratio")

  return(df)
}

## import FungiFun2 results ##
s3_a <- process_fungifun("vignettes/figures/f4_GO_DAY_Y.csv") %>%
  mutate(Proteins_found = gsub("CaO19.4937", "CHS3", Proteins_found),
         Proteins_found = gsub("CaO19.3767", "PEP1", Proteins_found))

s3_b <- process_fungifun("vignettes/figures/f4_GO_ATCC90028.csv") %>%
  mutate(Proteins_found = stringi::stri_replace_all_regex(.$Proteins_found,
                                                          c("CaO19.4937", "CaO19.3767", "CaO19.1816",
                                                            "CaO19.6594", "CaO19.9017", "CaO19.2347",
                                                            "CaO19.3803", "CaO19.1995", "CaO19.4255",
                                                            "CaO19.2651", "CaO19.8937"),
                                                          c("CHS3", "PEP1", "ALS3",
                                                            "PLB3", "PLB4.5", "MNN2",
                                                            "MNN22", "MNN24", "ECM331",
                                                            "CAM1-1", "FCY21"),
                                                          vectorize_all = FALSE))

s3_c <- process_fungifun("vignettes/figures/f4_GO_ATCC10231.csv") %>%
  mutate(Proteins_found = stringi::stri_replace_all_regex(.$Proteins_found,
                                                          c("CaO19.4937", "CaO19.3767", "CaO19.1816",
                                                            "CaO19.6594", "CaO19.9017", "CaO19.2347",
                                                            "CaO19.3803", "CaO19.1995", "CaO19.4255",
                                                            "CaO19.2651"),
                                                          c("CHS3", "PEP1", "ALS3",
                                                            "PLB3", "PLB4.5", "MNN2",
                                                            "MNN22", "MNN24", "ECM331",
                                                            "CAM1-1"),
                                                          vectorize_all = FALSE))

s3_d <- process_fungifun("vignettes/figures/f4_GO_DAY_B.csv") %>%
  mutate(Proteins_found = stringi::stri_replace_all_regex(.$Proteins_found,
                                                          c("CaO19.4937", "CaO19.3767", "CaO19.1816",
                                                            "CaO19.6594", "CaO19.9017"),
                                                          c("CHS3", "PEP1", "ALS3",
                                                            "PLB3", "PLB4.5"),
                                                          vectorize_all = FALSE))

#### Figure 5 ####
## import FungiFun2 results
s3_e <- process_fungifun("vignettes/figures/f6_GO_clust1.csv") %>%
  mutate(Proteins_found = gsub("CaO19.4937", "CHS3", Proteins_found))

s3_f <- process_fungifun("vignettes/figures/f6_GO_clust2.csv") %>%
  mutate(Proteins_found = gsub("CaO19.3767", "PEP1", Proteins_found))

s3_g <- process_fungifun("vignettes/figures/f6_GO_clust3.csv")

s3_h <- process_fungifun("vignettes/figures/f6_GO_clust4.csv")

s3_i <- process_fungifun("vignettes/figures/f6_GO_clust5.csv") %>%
  mutate(Proteins_found = stringi::stri_replace_all_regex(.$Proteins_found,
                                                          c("CaO19.5746", "CaO19.7481",
                                                            "CaO19.3002", "CaO19.7417"),
                                                          c("ALA1", "MDH1",
                                                            "RPS1", "TSA1"),
                                                          vectorize_all = FALSE))

s3_j <- process_fungifun("vignettes/figures/f6_GO_clust6.csv")


#### export ####
s3_sheets <- list("DAY286 yeast" = s3_a,
                  "ATCC90028" = s3_b,
                  "ATCC10231" = s3_c,
                  "DAY286 biofilm" = s3_d,
                  "Cluster 1" = s3_e,
                  "Cluster 2" = s3_f,
                  "Cluster 3" = s3_g,
                  "Cluster 4" = s3_h,
                  "Cluster 5" = s3_i,
                  "Cluster 6" = s3_j)
writexl::write_xlsx(s3_sheets, "vignettes/supplement_files/sfile_S3.xlsx")
