#' Candida Genome Database reference table
#'
#' @description This \code{data.frame} contains information about chromosomal features for
#' \emph{Candida albicans} SC5314 (Assembly 22) including:
#'
#' \itemize{
#' \item Various identifiers and aliases (see the \href{http://www.candidagenome.org/Nomenclature.shtml}{CGD website}
#' for nomenclature explanantions)
#' \item Gene names
#' \item Gene/protein descriptions
#' \item \emph{Saccharomyces cerevisiae} orthologs
#' \item GO terms (biological process, cellular component, molecular function)
#' }
#'
#' It was constructed using \href{https://github.com/csdaw/candidaev/tree/master/data-raw/reference/cgd.R}{this script}
#' and combines the \emph{C. albicans}
#' SC3514 Assembly 22 chromosomal feature file available \href{http://www.candidagenome.org/download/chromosomal_feature_files/}{here}
#' (downloaded 2018-12-09),
#' and the orf6 plus GO term file available \href{http://www.candidagenome.org/download/misc/}{here} (downloaded 2019-04-18).
#'
#' @format An object of class \code{data.frame} with 13281 observations (rows) and 20 variables (columns).
#'
#' @details Each row represents 1 chromosomal feature. The columns are as follows:
#'
#' \describe{
#'   \item{\strong{Systematic_name}}{Unique identifier for each chromosomal feature,
#'    introduced with Assembly 22.}
#'   \item{\strong{Gene_name}}{Standard gene name. Not all genes have standard gene names.}
#'   \item{\strong{Aliases}}{Non-standard gene names and alternative or old identifiers.}
#'   \item{\strong{Feature_type}}{Type of chromosomal feature e.g. open reading frame, tRNA, etc.}
#'   \item{\strong{Chromosome}}{Which chromosome the feature is on.}
#'   \item{\strong{Start_coord}}{Where on the chromosome the feature starts.}
#'   \item{\strong{Stop_coord}}{Where on the chromosome the feature ends.}
#'   \item{\strong{Strand}}{Which strand the feature is on: Watson or Crick.}
#'   \item{\strong{CGDID}}{Identifier for the feature which is specific to CGD. These are useful for cross-referencing
#'   with UniProt as \emph{C. albicans} proteins in UniProtKB are typically annotated with CGDIDs.}
#'   \item{\strong{Secondary_CGDID}}{Alternative CGDID.}
#'   \item{\strong{Description}}{Description of protein function for orfs, or description of the feature itself.}
#'   \item{\strong{Date_created}}{Date feature was created.}
#'   \item{\strong{Seq_coord_version}}{Sequence coordinate version date (if any).}
#'   \item{\strong{Gene_name_res_date}}{Gene name reservation date (if any).}
#'   \item{\strong{Res_name_also_standard}}{Has the reserved gene name become the standard name (yes or no).}
#'   \item{\strong{Scerevisiae_orthologs}}{\emph{Saccharomyces cerevisiae orthologous genes.}}
#'   \item{\strong{Feature_name}}{Unique identifier for each chromosomal feature used for Assembly 20
#'   and 21. Still persist in literaure.}
#'   \item{\strong{BP_CGD}}{Biological process GO terms from CGD}
#'   \item{\strong{CC_CGD}}{Cellular component GO terms from CGD}
#'   \item{\strong{MF_CGD}}{Molecular function GO terms from CGD}
#' }
#' @source Arnaud, M.B., Costanzo, M. C., Skrzypek, M. S., Binkley, G., Lane, C., Miyasato, S. R., & Sherlock, G.
#' (2005). The Candida Genome Database (CGD), a community resource for \emph{Candida albicans} gene and
#' protein information. \emph{Nucleic Acids Research, 33}(suppl_1), D358-D363.
#' \href{https://doi.org/10.1093/nar/gki003}{doi:10.1093/nar/gki003}
#'
#' Skrzypek, M. S., Binkley, J., Binkley, G., Miyasato, S. R., Simison, M., & Sherlock, G. (2017).
#' The Candida Genome Database (CGD): incorporation of Assembly 22, systematic identifiers and visualization of
#' high throughput sequencing data. \emph{Nucleic Acids Research, 45}(D1), D592-D596.
#' \href{https://doi.org/10.1093/nar/gkw924}{doi:10.1093/nar/gkw924}
"cgd"
