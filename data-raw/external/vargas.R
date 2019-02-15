# if uniprot.rda doesn't exist then stop
if(file.exists("data/uniprot.rda") == FALSE) {
  stop("uniprot.rda does not exist. Run ./data-raw/reference/uniprot.R then try again.")
}

# load packages
library(dplyr)

# source package functions
source("R/fill_blank.R")
source("R/match_id.R")

# load uniprot.rda
data(uniprot)

# load Vargas et al. 2015 Candida EV proteomics data
vargas <- read.csv(file = "data-raw/external/Vargas_et_al_2015.csv", header = TRUE,
                fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# clean vargas table
# Vargas table has several obsolete UniProt accessions and alternative gene names
# that are not in the UniProt lookup table. This makes comparing with the other
# papers difficult. This function replaces some alternative gene names with equivalent ones
# that also appear in the lookup table. Additionally it extracts UniProt accessions from
# the lookup table based on the gene names in the Vargas table. It also removes 1 protein
# from the table which has since been deleted from UniProt and CGD databases (orf19.10293)
clean_vargas <- function(df, badid, goodid, unip) {
  # convert CaO to orf identifiers, extract gene names, and replace obsolete/alternative gene names
  result <- df %>%
    mutate(Fasta_header = gsub("CaO", "orf", Fasta_header),
           var_gene_name = stringr::str_extract(Fasta_header, "(?<=GN=)(.+)(?= PE)")) %>%
    mutate(var_gene_name = stringi::stri_replace_all_regex(var_gene_name, badid, goodid, vectorize_all = FALSE))

  # add uniprot accessions and assign 1 protein per protein group (preferably one from the CANAL proteome)
  result <- result %>%
    mutate(proteome = stringr::str_extract(Fasta_header, "CANA."),
           UP_accession = match_id(var_gene_name, unip, "UP_accession", "UP_gene_name"),
           UP_accession2 = match_id(var_gene_name, unip, "UP_accession", "CGD_gene_name"),
           UP_accession = gsub("NA", "", UP_accession)) %>%
    filter(proteome != "CANAX",
           var_gene_name != "orf19.10293") %>%
    arrange(Group_number, proteome) %>%
    tibble::rownames_to_column(var = "num") %>%
    select(-num, -Group_number, everything()) %>%
    distinct(Group_number, .keep_all = TRUE)

  # fill in missing uniprot accessions
  result <- fill_blank(result, "UP_accession", "UP_accession2")
  result <- result %>%
    select(Group_number, Protein_number, Fasta_header, var_gene_name, proteome, UP_accession,
           everything(), -UP_accession2, -num)

  result$UP_accession <- as.character(result$UP_accession)

  return(result)
}

vargas <- clean_vargas(df = vargas,
                    goodid = c("orf19.2516", "orf19.31", "GLX3", "orf19.2168.3", "RBT4"),
                    badid = c("orf19.10052", "CIS302", "^orf19.251$", "CAWG_05907", "CAWG_00714"),
                    unip = uniprot)


# filter for proteins found in Strain 11 Candida EVs with min 1 peptide
vargas <- vargas %>%
  filter(S11_peptides > 0)

# write output .rda file
usethis::use_data(vargas)
