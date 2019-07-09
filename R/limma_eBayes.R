#' Perform differential protein abundance analysis with empirical Bayes
#' method from \emph{limma}
#'
#' @description This function performs differential abundance analysis of proteins
#' using a linear model fit and the empirical Bayes method from the
#' \pkg{limma}.
#'
#' The code for this function is slighlty modified from a section of the \code{test_diff} function
#' from the Bioconductor package \href{http://doi.org/10.18129/B9.bioc.DEP}{DEP}.
#' \href{http://doi.org/10.18129/B9.bioc.DEP}{DEP} is distributed with the Artistic-2.0 license
#' which requires any source code modifications to be clearly stated.
#'
#' \code{limma_eBayes} includes the following modifications of \code{test_diff}:
#'
#' \itemize{
#' \item Names of some objects have been changed.
#' \item Order of \code{lmFit()} and \code{makeContrasts()} has been swapped.
#' \item The rest of \code{test_diff} source code has been removed.
#' }
#'
#' The results of this function can be extracted using \code{\link{get_results}}.
#'
#' @param mat numeric matrix: typically this will include log2 LFQ intensity data.
#' \code{NA} values are allowed, however the \code{\link[limma]{eBayes}} function may
#' produce a warning
#'
#' @param design design matrix: created by \code{stats::model.matrix()}. See the
#' \pkg{limma} user guide for instructions on creating design matrices.
#'
#' @param contrasts character vector: names of sample groups you want to contrast separated
#' by a \code{-} sign. For example: \code{c("A1_EV - A1_W", "A9_EV - A9_W")}. These \strong{must}
#' refer to column names present in the design matrix.
#'
#' @param block \code{NULL} or numeric vector: blocking for LmFit function. Must
#' be the same length as the number of columns in \code{mat}.
#'
#' @return Returns an object of class \code{MArrayLM}. See \code{\link[limma]{eBayes}} for
#' more information. This object is required for \code{\link{get_results}}.
#'
#' @references Zhang, X., Smits, A., van Tilburg, G., Ovaa, H., Huber, W., Vermeulen,
#' M. (2018). Proteome-wide identification of ubiquitin interactions using UbIA-MS.
#' \emph{Nature Protocols}, \strong{13}, 530-550.
#' \href{https://doi.org/10.1038/nprot.2017.147}{doi:10.1038/nprot.2017.147}
#'
#' @examples
#' # load dplyr
#' library(dplyr)
#'
#' # load a proteinGroups data.frame supplied with this package
#' my_proteinGroups <- atcc
#'
#' # load its corresponding experimental design
#' my_expDesign <- atcc_exp
#'
#' # filter for proteins identified with minimum 3 unique peptides
#' # and convert to numeric matrix
#' my_lfq <- my_proteinGroups %>%
#'   filter(Unique.peptides >= 3) %>%
#'   convert_lfq(., my_expDesign)
#'
#' # filter for proteins quantified in min 2/3 reps of
#' # at least 1 sample group
#' my_filt <- my_lfq %>%
#'   filter_na4(., logic = "or", op = "<=",
#'              pat1 = "A10231_EV", val1 = 1,
#'              pat2 = "A10231_W", val2 = 1,
#'              pat3 = "A90028_EV", val3 = 1,
#'              pat4 = "A90028_W", val4 = 1)
#'
#' # normalise LFQ intensities
#' my_norm <- limma::normalizeCyclicLoess(my_filt)
#'
#' # impute missing values
#' my_imp <- impute_QRILC(my_norm)
#'
#' # rename columns
#' colnames(my_imp) <- stringi::stri_replace_all_regex(colnames(my_imp),
#'                                                     c("A10231_", "A90028_"),
#'                                                     c("A1_", "A9_"),
#'                                                     vectorize_all = FALSE)
#'
#' # see limmma user guide section 9.2 for more info about DE
#' # create design matrix
#' my_samples <- data.frame(T = rep(c("A1_EV", "A1_W", "A9_EV", "A9_W"), each = 3))
#'
#' my_design <- stats::model.matrix(~ 0 + T, data = my_samples)
#' colnames(my_design) <- c("A1_EV", "A1_W", "A9_EV", "A9_W")
#'
#' # define sample contrasts of interest
#' my_contrasts <- c("A1_EV - A1_W", "A9_EV - A9_W", "A1_EV - A9_EV", "A1_W - A9_W")
#'
#' # make linear model fit and perform limma::eBayes()
#' my_efit <- limma_eBayes(mat = my_imp, design = my_design, contrasts = my_contrasts)
#'
#' # extract overall results table
#' result_overall <- get_results(my_efit, my_imp,
#'                               p_val = 0.001, lfc = 0, type = "overall")
#'
#' # extract list of results tables for each individual contrast
#' result_list <- get_results(my_efit, my_imp,
#'                            p_val = 0.001, lfc = 0, type = "individual")
#'
#' # see results table for A9_EV versus A9_W contrast
#' result_a9 <- result_list[[2]]
#'
#' @export
limma_eBayes <- function(mat, design, contrasts, block = NULL) {
  cm <- limma::makeContrasts(contrasts = contrasts, levels = design)
  fit <- limma::lmFit(mat, design = design, block = block)

  result <- limma::contrasts.fit(fit = fit, contrasts = cm)

  if(any(is.na(mat))) {
    for(i in contrasts) {
      covariates = strsplit(i, " - ") %>% unlist()
      single_contrast <- limma::makeContrasts(contrasts = i,
                                              levels = design[, covariates])
      single_contrast_fit <- limma::contrasts.fit(fit[, covariates],
                                                  single_contrast)
      result$coefficients[, i] <- single_contrast_fit$coefficients[, 1]
      result$stdev.unscaled[, i] <- single_contrast_fit$stdev.unscaled[, 1]
    }
  }
  result <- limma::eBayes(result)
  result
}
