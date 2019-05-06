#' \emph{C. albicans} (ATCC10231, ATCC90028) experimental design
#' for LC-MS/MS data.
#'
#' @description Certain plot functions in this package which are adapted from the
#' \href{http://doi.org/10.18129/B9.bioc.DEP}{DEP}
#' package require an experimental design to define data groups and labels.
#' This data.frame contains the experimental design for the 12 samples in the
#' \code{\link{atcc}} data set.
#'
#' @format An object of class \code{data.frame} with 12 observations (rows) and 4 variables (columns).
#'
#' @details Each row represents 1 LC-MS/MS sample. The columns are as follows:
#'
#' \describe{
#'   \item{\strong{ID}}{The name to be used for axis labels and plot legends.}
#'   \item{\strong{label}}{The exact LFQ intensity column name for this sample in the \code{\link{atcc}} data set.}
#'   \item{\strong{condition}}{The type of sample: EV or WCL and which strain.}
#'   \item{\strong{replicate}}{Biological replicate.}
#' }
#'
#' @source Dawson, C. S., Bleackley, M. R., Garcia-Ceron, D., & Anderson, M. A. (2019).
#' Title. \emph{Journal vol}(issue), 000-000. doi:
#'
"atcc_exp"
