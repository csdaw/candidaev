# if uniprot.rda doesn't exist then stop
if(file.exists("data/uniprot.rda") == FALSE) {
  stop("uniprot.rda does not exist. Run ./data-raw/reference/uniprot.R then try again.")
}

# load packages
library(dplyr)

# source package functions
source("R/match_id.R")

# load uniprot.rda
data(uniprot)

# load Wolf et al. 2015 Candida EV proteomics data
wolf <- read.csv(file = "data-raw/external/Wolf_et_al_2015.csv", header = TRUE,
                 fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# add uniprot accession column by matching Gene_name column in wolf
wolf$UP_accession <- match_id(wolf$Gene_name, uniprot, "CGD_gene_name", "UP_accession")

# write output .rda file
usethis::use_data(wolf, overwrite = TRUE)
