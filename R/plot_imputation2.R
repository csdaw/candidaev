#' Visualise effect of imputation on protein intensities
#'
#' @description \code{plot_imputation2} generates density plots of protein
#' LFQ intensities showing the distribution of intensities
#' before and after imputation.
#'
#' This function has been taken directly from the \code{plot_imputation}
#' function in the \href{http://doi.org/10.18129/B9.bioc.DEP}{DEP} package and
#' then modified for use in this package.
#'
#' \href{http://doi.org/10.18129/B9.bioc.DEP}{DEP} is distributed with the
#' Artistic-2.0 license
#' which requires any source code modifications to be clearly stated.
#'
#' \code{plot_imputation2} includes the following modifications
#' of \code{plot_imputation}:
#'
#' \itemize{
#' \item Dependance on a "SummarizedExperiment" object as input has been
#' removed. Instead the input has been changed to a numeric matrix.
#' \item Use of \code{theme_DEP1()} has been replaced with \code{theme_bw()}
#' \item Parsing of function arguments has been slightly modified.
#' }
#'
#' @param exd data.frame: contains information for plot labels. See the
#' examples section of \code{\link{assert_exd}} or \code{\link{atcc_exp}} for
#' examples of appropriately structured experimental designs.
#'
#' @param facet_labels \code{FALSE} or a named vector: if\code{FALSE} then
#' the matrix names will be used for the facet labels (aka. strip text). Use a
#' named vector to customise the facet labels. For example if you are plotting
#' the following: \code{plot_imputation2(exp, facet_labels = x, mat1, mat2)}
#' then \code{x} should be \code{c(mat1 = "Matrix 1", mat2 = "Matrix 2")}.
#'
#' @param mat numeric matrix: LFQ intensity data before imputation
#'
#' @param ... numeric matrix/matrices: LFQ intensity data after imputation.
#' Multiple matrices can be input for plotting.
#'
#' @return Returns density plots of all input matrices, e.g. before and
#' after imputation (generated by \code{\link[ggplot2]{ggplot}}).
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
#' my_imp <- pcaMethods::pca(my_norm, method = "svdImpute")
#' my_imp <- pcaMethods::completeObs(my_imp)
#'
#' # impute missing values with another method
#' my_imp2 <- pcaMethods::pca(my_norm, method = "ppca")
#' my_imp2 <- pcaMethods::completeObs(my_imp2)
#'
#' # density plot before and after imputations
#' plot_imputation2(exd = atcc_exp,
#'                  facet_labels = FALSE,
#'                  my_norm,
#'                  my_imp,
#'                  my_imp2)
#'
#'  # density plot before and after imputations
#'  # with custom labels
#'  plot_imputation2(exd = atcc_exp,
#'                   facet_labels = c(my_norm = "A",
#'                                    my_imp = "B",
#'                                    my_imp2 = "C"),
#'                   my_norm,
#'                   my_imp,
#'                   my_imp2)
#' @export
plot_imputation2 <- function(exd, facet_labels = FALSE, mat, ...) {
  # Get arguments from call
  call <- match.call()
  arglist <- lapply(call[-1], function(x) x)
  to_plot <- arglist[c(-1,-2)]
  var.names <- vapply(to_plot, deparse, character(1))
  to_plot <- lapply(to_plot, eval.parent, n = length(to_plot))
  names(to_plot) <- var.names

  # Show error if inputs are not the required classes
  assertthat::assert_that(is.data.frame(exd),
                          assert_exd(exd))
  lapply(to_plot, function(x) {
    assertthat::assert_that(is.matrix(x))
  })

  # Internal function to get a long data.frame of the assay data
  # annotated with sample info
  gather_join2 <- function(mat) {
    as.data.frame(mat) %>%
      tidyr::gather(ID, val) %>%
      left_join(., as.data.frame(exd), by = "ID")
  }

  df <- purrr::map_df(to_plot, gather_join2, .id = "var") %>%
    mutate(var = factor(var, levels = names(to_plot)))

  # Density plots for different conditions with facet_wrap
  # for original and imputed samles
  if(is.null(facet_labels)) {
    ggplot(df, aes(val, col = condition)) +
      geom_density(na.rm = TRUE) +
      facet_wrap(~var, ncol = 1) +
      labs(x = expression(log[2]~"LFQ intensity"), y = "Density") +
      theme_bw(base_size = 16)
  } else {
    ggplot(df, aes(val, col = condition)) +
      geom_density(na.rm = TRUE) +
      facet_wrap(~var, ncol = 1,
                 labeller = labeller(var = facet_labels)) +
      labs(x = expression(log[2]~"LFQ intensity"), y = "Density") +
      theme_bw(base_size = 16)
  }

}
