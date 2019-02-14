# if cgd.rda doesn't exist then stop
if(file.exists("data/cgd.rda") == FALSE) {
  stop("cgd.rda does not exist. Run ./data-raw/reference/cgd.R then try again.")
}

# load packages
library(dplyr)

# source fill_blank and fill_na function
source("R/fill_blank.R")
source("R/fill_na.R")
source("R/match_id.R")

# load cgd.rda
data(cgd)

# read input .tab file
uniprot <- read.delim(file = "data-raw/reference/20180616_uniprot-proteome_UP000000559.tab",
                         header = TRUE,
                         sep = "\t",
                         stringsAsFactors = FALSE)

# add column names
colnames(uniprot) <- c("UP_accession",
                       "CGDID",
                       "UP_gene_name",
                       "Gene_name_ol",
                       "Gene_name_orf",
                       "Protein_name",
                       "Function",
                       "Subcellular_location",
                       "Mass_(Da)",
                       "Length_(aa)",
                       "Sequence",
                       "Transmembrane",
                       "Topology",
                       "Signal_peptide",
                       "GO_all",
                       "GO_CC",
                       "GO_MF",
                       "GO_BP",
                       "GO_IDs",
                       "UP_status")

# define list of bad identifiers to replace
bad_ids <- c("CAALFM_CR",
             "CAALFM_C1",
             "CAALFM_C2",
             "CAALFM_C3",
             "CAALFM_C4",
             "CAALFM_C5",
             "CAALFM_C6",
             "CAALFM_C7",
             "CA$",
             "WA$",
             "CB$",
             "WB$")

# define list of good identifiers to replace bad_ids
good_ids <- c("CR_",
              "C1_",
              "C2_",
              "C3_",
              "C4_",
              "C5_",
              "C6_",
              "C7_",
              "C_A",
              "W_A",
              "C_B",
              "W_B")

# replace bad identifiers with good identifiers
uniprot$Gene_name_orf <- stringi::stri_replace_all_regex(uniprot$Gene_name_orf,
                                                            bad_ids,
                                                            good_ids,
                                                            FALSE)

uniprot$Gene_name_ol <- stringi::stri_replace_all_regex(uniprot$Gene_name_ol,
                                                           bad_ids,
                                                           good_ids,
                                                           FALSE)

# if orf column is blank, insert identifier from ol column
uniprot <- fill_blank(uniprot, "Gene_name_orf", "Gene_name_ol")

# create a new CGDID column using info from cgd.rda and Gene_name_ol
uniprot$CGDID2 <- as.character(cgd$CGDID[match(uniprot$Gene_name_ol,
                                                        cgd$Systematic_name)])

uniprot$CGDID3 <- as.character(cgd$CGDID[match(uniprot$Gene_name_orf,
                                                        cgd$Systematic_name)])

# fill in blanks in unip_table CGDID column
uniprot <- fill_blank(uniprot, "CGDID", "CGDID2")

uniprot <- fill_na(uniprot, "CGDID", "CGDID3")

# remove semicolons if there is only one CGDID in CGDID column
uniprot$CGDID <- gsub(";$", "", uniprot$CGDID)

# add some CGD columns (using UniProt CGDIDs for crossreference)
uniprot$CGD_gene_name <- match_id(uniprot$CGDID, cgd, "Gene_name", "CGDID")
uniprot$CGD_feature_name <- match_id(uniprot$CGDID, cgd, "Feature_name", "CGDID")
uniprot$CGD_systematic_name <- match_id(uniprot$CGDID, cgd, "Systematic_name", "CGDID")
uniprot$CGD_description <- match_id(uniprot$CGDID, cgd, "Description", "CGDID")

# remove CGDID2, CGDID3 ol, orf column
uniprot <- within(uniprot, rm(CGDID2, CGDID3, Gene_name_ol, Gene_name_orf))

# rearrange columns
uniprot <- uniprot %>%
  select(UP_accession, CGDID, UP_gene_name, CGD_gene_name, CGD_feature_name,
         CGD_systematic_name, Protein_name, CGD_description, everything())

# write output .rda file
usethis::use_data(uniprot, overwrite = TRUE)
