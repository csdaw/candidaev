#' Clean CGD chromosomal feature file
#'
#' Clean the current chromosomal feature file downloaded
#' from the Candida Genome Database website.
#'
#' @param file path for CGD ccf .tab file
#'
#' @return `cgd-table.txt` ready for use as lookup table
#' @export
#'
#' @examples
clean_cgd <- function(file) {
  if(!exists("fill blank", mode = "function")) {
    source("R/util.R")
  }

  # read input .tab file
  cgd_table <- read.delim(file,
                          header = FALSE,
                          sep = "\t",
                          comment.char = "!",
                          stringsAsFactors = FALSE)

  # add column names
  colnames(cgd_table) <- c("Systematic_name",
                           "Gene_name",
                           "Aliases",
                           "Feature_type",
                           "Chromosome",
                           "Start_coord",
                           "Stop_coord",
                           "Strand",
                           "CGDID",
                           "Secondary_CGDID",
                           "Description", "
                           Date_created",
                           "Seq_coord_version",
                           "Blank",
                           "Blank2",
                           "Gene_name_res_date",
                           "Res_name_also_standard",
                           "Scerevisiae_orthologs")

  # extract orf identifiers (Feature_name) from aliases column
  cgd_table$Feature_name <- stringr::str_extract(cgd_table$Aliases, "orf19.[0-9]+")

  # replace blank gene names with orf identifiers
  cgd_table <- fill_blank(cgd_table, "Gene_name", "Feature_name")

  # remove blank columns
  cgd_table <- within(cgd_table, rm(Blank, Blank2))

  # write output .txt file
  write.table(cgd_table,
              "data/clean/cgd-table.txt",
              sep = "\t",
              row.names = FALSE)
}


#' Clean UniProt proteome file
#'
#' Clean the list of C. albicans proteins downloaded
#' from Uniprot.
#'
#' @param file path for UniProt .tab file
#'
#' @return `unip-table.txt` ready for use as lookup table
#' @export
#'
#' @examples
clean_unip <- function(file) {

  # if cgd-table.txt doesn't exist then stop
  if(file.exists("data/clean/cgd-table.txt") == FALSE) {
    stop("cgd-table.txt does not exist. Run clean_cgd() then try again.")
  }

  if(!exists("fill blank", mode = "function")) {
    source("R/util.R")
  }

  # read input .tab file
  unip_table <- read.delim(file = file,
                           header = TRUE,
                           sep = "\t",
                           stringsAsFactors = FALSE)

  # add column names
  colnames(unip_table) <- c("UP_accession",
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
  unip_table$Gene_name_orf <- stringi::stri_replace_all_regex(unip_table$Gene_name_orf,
                                                              bad_ids,
                                                              good_ids,
                                                              FALSE)

  unip_table$Gene_name_ol <- stringi::stri_replace_all_regex(unip_table$Gene_name_ol,
                                                             bad_ids,
                                                             good_ids,
                                                             FALSE)

  # if orf column is blank, insert identifier from ol column
  unip_table <- fill_blank(unip_table, "Gene_name_orf", "Gene_name_ol")

  # create a new CGDID column using info from cgd-table.txt and Gene_name_ol
  cgd_table <- read.table(file = "data/clean/cgd-table.txt",
                          header = TRUE,
                          sep = "\t",
                          stringsAsFactors = FALSE)
  unip_table$CGDID2 <- as.character(cgd_table$CGDID[match(unip_table$Gene_name_ol,
                                                          cgd_table$Systematic_name)])

  unip_table$CGDID3 <- as.character(cgd_table$CGDID[match(unip_table$Gene_name_orf,
                                                          cgd_table$Systematic_name)])

  # fill in blanks in unip_table CGDID column
  unip_table <- fill_blank(unip_table, "CGDID", "CGDID2")

  unip_table$CGDID <- as.character(unip_table$CGDID)

  unip_table <- fill_na(unip_table, "CGDID", "CGDID3")

  # remove semicolons in CGDID column
  unip_table$CGDID <- gsub(";C", "; C", unip_table$CGDID)

  unip_table$CGDID <- gsub(";", "", unip_table$CGDID)

  # add CGD gene name column (using first UniProt CGDID for crossreference)
  unip_table$CGD_gene_name <- as.character(cgd_table$Gene_name[match(stringr::str_extract(unip_table$CGDID,
                                                                                          "[^ ]*"),
                                                                     cgd_table$CGDID)])

  # remove CGDID2, CGDID3 ol, orf column
  unip_table <- within(unip_table, rm(CGDID2, CGDID3, Gene_name_ol, Gene_name_orf))

  # rearrange columns
  unip_table <- unip_table %>%
    select(UP_accession, CGDID, UP_gene_name, CGD_gene_name, everything())

  # write output .txt file
  write.table(unip_table,
              "data/clean/unip-table.txt",
              sep = "\t",
              row.names = FALSE)
}
