% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_lfq.R
\name{convert_lfq}
\alias{convert_lfq}
\title{Convert proteinGroups table into LFQ intensity matrix}
\usage{
convert_lfq(df, exd, log_2 = TRUE)
}
\arguments{
\item{df}{data.frame: this should at a minimum contain a \code{Majority.protein.IDs}
column as well as the \code{LFQ.intensity} columns defined in the experimental design.}

\item{exd}{data.frame: see the examples section of \code{\link{assert_exd}} or
\code{\link{atcc_exp}} for examples of appropriately structured experimental designs.}

\item{log_2}{logical: if \code{TRUE} then the numeric matrix will be log2
transformed before being returned. If \code{FALSE} the output numeric
matrix will not be log2 transformed before being returned.}
}
\value{
Returns a numeric \code{matrix} with the same number of rows as \code{df}
and the \code{LFQ.intensity} columns defined in \code{exd}. The row names are the
\code{Majority.protein.IDs} column in \code{df}.
}
\description{
This function extracts the \code{LFQ.intensity} columns defined in
an experimental design from a MaxQuant proteinGroups data.frame, and converts
these into a numeric matrix.

It also changes the row names of the numeric matrix to the UniProt
accession(s) found in the \code{Majority.protein.IDs} column of the
proteinGroups data.frame.

A numeric matrix generally makes it easier to perform
normalisation, imputation, and statistical analyses in later steps.
}
\examples{
# load dplyr
library(dplyr)

# load a proteinGroups data.frame supplied with this package
my_proteinGroups <- atcc

# load its corresponding experimental design
my_expDesign <- atcc_exp

# filter for proteins identified with minimum 3 unique peptides
# and convert to numeric matrix
my_lfq <- my_proteinGroups \%>\%
  filter(Unique.peptides >= 3) \%>\%
  convert_lfq(., my_expDesign)

}
