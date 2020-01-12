#### Setup ####
# need writexl package to write output to .xlsx
requireNamespace("writexl", quietly = TRUE)

# load packages
library(dplyr)
library(candidaev)

# define column rename function
rnm_cols <- function(df) {
  df <- df %>%
    rename("Accession" = UP_accession,
           "CGDID" = CGDID,
           "Gene name" = UP_gene_name,
           "CGD Gene name" = CGD_gene_name,
           "CGD Feature name" = CGD_feature_name,
           "CGD systematic name" = CGD_systematic_name,
           "Protein name" = Protein_name,
           "CGD description" = CGD_description,
           "Function" = Function,
           "Location" = Subcellular_location,
           "Mass (kDa)" = "Mass_(kDa)",
           "Length (aa)" = "Length_(aa)",
           "Sequence" = Sequence,
           "TM domain" = Transmembrane,
           "Topology" = Topology,
           "Signal peptide" = Signal_peptide,
           "GO all" = "GO_all",
           "GO CC" = GO_CC,
           "GO MF" = GO_MF,
           "GO BP" = GO_BP,
           "GO IDs" = GO_IDs,
           "UP status" = UP_status)
}

#### Supplementary Data S8 ####
# Table of 22 C. albicans EV marker candidates
# Metadata for these proteins

marker_22 <- c("ARF3",
               "CDC42",
               "FAA4",
               "FET34",
               "MTS1",
               "NCE102",
               "orf19.6741",
               "RAC1",
               "RHO1",
               "RHO3",
               "SSO2",
               "SUR7",
               "YCK2",
               "GPD2",
               "PHR1",
               "orf19.1054",
               "orf19.2168.3",
               "orf19.3799",
               "SEC4",
               "YKT6",
               "YPT31",
               "VAC8")

metadata <- candidaev::uniprot %>%
  filter(CGD_gene_name %in% marker_22) %>%
  rnm_cols

sheets <- list("22 markers" = metadata)
writexl::write_xlsx(sheets,
                    "inst/manuscript/supplementary_data/supplementary_data_S8.xlsx")
