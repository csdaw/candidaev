% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/biofilm_exp.R
\docType{data}
\name{biofilm_exp}
\alias{biofilm_exp}
\title{\emph{C. albicans} (DAY286 biofilm) experimental design
for LC-MS/MS data.}
\format{
An object of class \code{data.frame} with 12 observations (rows) and 4 variables (columns).
}
\source{
Dawson, C. S., Garcia-Ceron, D., Rajapaksha, H., Faou, P., Anderson,
M. A., Bleackley, M. R. (2020) Protein markers for \emph{Candida albicans}
EVs include claudin-like Sur7 family proteins.
\emph{Journal of Extracellular Vesicles}, 9(1): 1750810. DOI:
\href{https://doi.org/10.1080/20013078.2020.1750810}{10.1080/20013078.2020.1750810}
}
\usage{
biofilm_exp
}
\description{
Certain plot functions in this package which are adapted from the
\href{http://doi.org/10.18129/B9.bioc.DEP}{DEP}
package require an experimental design to define data groups and labels.
This data.frame contains the experimental design for the 10 samples in the
\code{\link{biofilm}} data set.
}
\details{
Each row represents 1 LC-MS/MS sample. The columns are as follows:

\describe{
  \item{\strong{ID}}{The name to be used for axis labels and plot legends.}
  \item{\strong{label}}{The exact LFQ intensity column name for this sample in the \code{\link{atcc}} data set.}
  \item{\strong{condition}}{The type of sample: EV or WCL.}
  \item{\strong{replicate}}{Biological replicate.}
}
}
\keyword{datasets}
