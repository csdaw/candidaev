#' UniProt \emph{C. albicans} reference table
#'
#' @description This \code{data.frame} contains information about the proteins
#' in the \emph{Candida albicans} SC5314 reference proteome
#' (UP000000559, downloaded 2018-05-29, 6035 entries) including:
#'
#' \itemize{
#' \item Various identifiers and aliases, including Candida Genome Database
#' identifiers for cross referencing with \code{\link{cgd}}.
#' \item Gene names
#' \item Gene/protein descriptions
#' \item Protein physical properties
#' \item GO terms (biological process, cellular component, molecular function)
#' }
#'
#' \code{uniprot} was constructed using
#' \href{https://github.com/csdaw/candidaev/tree/master/data-raw/reference/uniprot.R}{this script}
#' and incorporates data from the \emph{C. albicans}
#' SC3514 Assembly 22 chromosomal feature file available
#' \href{http://www.candidagenome.org/download/chromosomal_feature_files/}{here}
#' (downloaded 2018-12-09), and the \emph{C. albicans} SC5314 UniProt
#' reference proteome available
#' \href{https://www.uniprot.org/proteomes/UP000000559}{here}
#' (downloaded 2018-05-29).
#'
#' @format An object of class \code{data.frame} with 6035 observations
#' (rows) and 22 variables (columns).
#'
#' @details Each row represents 1 protein. The columns are as follows:
#'
#' \describe{
#'   \item{\strong{UP_accession}}{Accession number for the protein in UniProt.}
#'   \item{\strong{CGDID}}{Identifier for the chromosomal feature associated
#'   with the protein. Specific to the CGD.}
#'   \item{\strong{UP_gene_name}}{Gene name annotated in UniProt.}
#'   \item{\strong{CGD_gene_name}}{Standard gene name annotated in the CGD.}
#'   \item{\strong{CGD_feature_name}}{Unique identifier for the chromosomal
#'   feature associated with the protein,
#'   used for Assembly 20 and 21. Still persist in literaure.}
#'   \item{\strong{CGD_systematic_name}}{Unique identifier for the chromosomal
#'   feature associated with the protein, used for Assembly 22.}
#'   \item{\strong{Protein_name}}{Protein name annotated in UniProt.}
#'   \item{\strong{CGD_description}}{Description of protein function annotated
#'   in the CGD.}
#'   \item{\strong{Function}}{Protein function annotated in UniProt.}
#'   \item{\strong{Subcellular_location}}{Subcellular location annotated in
#'   UniProt.}
#'   \item{\strong{Mass_(kDa)}}{Mass of protein annotated in UniProt.}
#'   \item{\strong{Length_(aa)}}{Length of the protein in amino acids
#'   annotated in UniProt.}
#'   \item{\strong{Sequence}}{Amino acid protein sequence annotated in
#'   UniProt.}
#'   \item{\strong{Transmembrane}}{Number, type, and location of protein
#'   transmembrane domains annotated in UniProt.}
#'   \item{\strong{Topology}}{Information on the topology of the protein
#'   annotated in UniProt.}
#'   \item{\strong{Signal_peptide}}{Presence and location of protein signal
#'   peptide annotated in UniProt.}
#'   \item{\strong{GO_all}}{Names and GO IDs of all the GO terms annotated
#'   for the protein in UniProt.}
#'   \item{\strong{GO_CC}}{Names and GO IDs of all the Cellular Component GO
#'   terms annotated for the protein in UniProt.}
#'   \item{\strong{GO_MF}}{Names and GO IDs of all the Molecular Function GO
#'   terms annotated for the protein in UniProt.}
#'   \item{\strong{GO_BP}}{Names and GO IDs of all the Biological Process GO
#'   terms annotated for the protein in UniProt.}
#'   \item{\strong{GO_IDs}}{GO IDs for all the GO terms annotated for the
#'   protein in UniProt.}
#'   \item{\strong{UP_status}}{Status of the UniProt entry, has it been
#'   reviewed or not.}
#' }
#'
#' @source The UniProt Consortium (2019). UniProt: a worldwide hub of protein
#' knowledge. \emph{Nucleic Acids Research, 47}(D1), D506-D515.
#' \href{https://doi.org/10.1093/nar/gky1049}{doi:10.1093/nar/gky1049}
#'
#' Arnaud, M.B., Costanzo, M. C., Skrzypek, M. S., Binkley, G., Lane, C.,
#' Miyasato, S. R., & Sherlock, G. (2005). The Candida Genome Database (CGD),
#' a community resource for \emph{Candida albicans} gene and
#' protein information. \emph{Nucleic Acids Research} \strong{33}(suppl_1), D358-D363.
#' \href{https://doi.org/10.1093/nar/gki003}{doi:10.1093/nar/gki003}
#'
#' Skrzypek, M. S., Binkley, J., Binkley, G., Miyasato, S. R., Simison, M.,
#' & Sherlock, G. (2017). The Candida Genome Database (CGD): incorporation of
#' Assembly 22, systematic identifiers and visualization of
#' high throughput sequencing data.
#' \emph{Nucleic Acids Research} \strong{45}(D1), D592-D596.
#' \href{https://doi.org/10.1093/nar/gkw924}{doi:10.1093/nar/gkw924}
"uniprot"
