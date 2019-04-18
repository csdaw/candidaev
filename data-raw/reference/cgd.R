# load packages
library(dplyr)

# source fill_blank function
source("R/fill_blank.R")
source("R/match_id_multi.R")

# read input .tab file
cgd <- read.delim("data-raw/reference/20181209_C_albicans_SC5314_A22_ccf.tab",
                  header = FALSE,
                  sep = "\t",
                  comment.char = "!",
                  stringsAsFactors = FALSE)

# add column names
colnames(cgd) <- c("Systematic_name",
                   "Gene_name",
                   "Aliases",
                   "Feature_type",
                   "Chromosome",
                   "Start_coord",
                   "Stop_coord",
                   "Strand",
                   "CGDID",
                   "Secondary_CGDID",
                   "Description",
                   "Date_created",
                   "Seq_coord_version",
                   "Blank",
                   "Blank2",
                   "Gene_name_res_date",
                   "Res_name_also_standard",
                   "Scerevisiae_orthologs")

# extract orf identifiers (Feature_name) from aliases column
cgd$Feature_name <- stringr::str_extract(cgd$Aliases,
                                         "(orf19.[0-9.]+)|(CaalfMp[0-9]{2})")

# replace blank gene names with orf identifiers
cgd <- fill_blank(cgd, "Gene_name", "Feature_name")

# replace NAs with blank
cgd <- cgd %>%
  mutate_at(.vars = vars(Gene_name, Feature_name),
            .funs = list(~ case_when(is.na(.) == TRUE ~ "",
                                     TRUE ~ .)))

# remove blank columns
cgd <- within(cgd, rm(Blank, Blank2))

# read GO annotations .txt file
go <- read.delim("data-raw/reference/20190418_C_albicans_SC3514_GO.txt",
                 header = FALSE,
                 stringsAsFactors = FALSE)

# make GO entries with no GO terms blank and rename columns
go <- go %>%
  mutate_at(.vars = vars(V5, V6, V7),
            .funs = list(~ case_when(. %in% c("biological_process",
                                              "cellular_component",
                                              "molecular_function") ~ "",
                                   TRUE ~ .))) %>%
  select(V2, V5, V6, V7) %>%
  rename("Systematic_name" = V2, BP_CGD = V5, CC_CGD = V6, MF_CGD = V7)

# add CGD GO terms to cgd table
cgd <- match_id_multi(df = cgd,
                      id = "Systematic_name",
                      ref = test,
                      match = "Systematic_name",
                      new = c("BP_CGD", "CC_CGD", "MF_CGD")) %>%
  mutate_at(.vars = vars(BP_CGD, CC_CGD, MF_CGD),
            .funs = list(~ case_when(. == "NA" ~ "",
                                     TRUE ~ .)))

# write cgd table to output .rda file
usethis::use_data(cgd, overwrite = TRUE)
