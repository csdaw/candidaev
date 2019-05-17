#' Process FungiFun2 GO output
#'
#' @description \code{process_fungifun} is a function that loads the
#' \code{.csv} output file from the online tool
#' \href{https://sbi.hki-jena.de/fungifun/}{FungiFun2} and cleans up the
#' table for analysis.
#'
#' The settings that should be used for the FungiFun2 functional enrichment
#' analysis are:
#' \itemize{
#' \item Classification ontology = GO
#' \item 9/9 columns in the summary table should be shown.
#' \item Save as = CSV
#' }
#'
#' @param filename character: path to the .csv file output by FungiFun2
#'
#' @return Returns a data.frame with the results of the FungiFun2 GO term
#' enrichment analysis.
#'
#' @examples
#' # write some gene names to the clipboard
#' # only works with Windows
#' my_genes <- c("GSC1", "XOG1", "BGL2", "HYR1", "ALS3",
#'               "PLB4.5", "SAP9")
#'
#' writeClipboard(my_genes)
#'
#' # paste into FungiFun2 input
#' # perform GO analysis and save output as CSV
#' # import GO enrichment analysis results
#' GO_result <- process_fungifun("./path/to/file")
#'
#' @export
process_fungifun <- function(filename) {
  df <- read.csv(filename, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  df$N.prot.found <- as.numeric(gsub(" /.*", "", df$X..genes...category))
  df$N.prot.cat <- as.numeric(gsub(".* / ", "", df$X..genes...category))
  df$N.prot.input <- as.numeric(gsub(".* / ", "", df$X..genes...input))
  df$Cat.ratio <- df$N.prot.found / df$N.prot.cat
  df$Input.ratio <- df$N.prot.found / df$N.prot.input

  df <- df[, -c(7:8)]

  colnames(df)[1:6] <- c("GO.ID", "GO.term", "GO.type",
                         "Prot.found", "p.val", "adj.p.val")

  df$Prot.found <- as.character(df$Prot.found)
  df$Prot.found <- gsub(" | ", ";", df$Prot.found, fixed = TRUE)

  return(df)
}
