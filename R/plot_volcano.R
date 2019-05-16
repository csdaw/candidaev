#' Visualise log2 fold change data with volcano plot
#'
#' @description \code{plot_volcano} generates a volcano plot
#' from a data.frame containing the following data:
#' \itemize{
#' \item p-values or some other values used for statistical hypothesis
#' testing.
#' \item log2 fold change values or some other fold change measure.
#' \item a grouping column for sorting each row, for example significant or not
#' significant. This is used for giving each point on the plot a certain
#' colour.
#' }
#'
#' @param data data.frame: contains p-values and fold changes for proteins,
#' genes, etc. (rows).
#'
#' @param p_val expression: name of a numeric column in \code{data}. Written as
#' \code{column_name} instead of \code{"column_name"}. Data to be plotted on
#' the y axis. Should not be log10 transformed as the p-values will be
#' transformed by this function.
#'
#' @param log2fc expression: name of a numeric column in \code{data}.
#' Written as \code{column_name} instead of \code{"column_name"}.
#' Data to be plotted on the x axis. Can be log2 transformed fold changes or
#' untransformed fold changes.
#'
#' @param group expression: name of a character column in \code{data}. Written
#' as \code{column_name} instead of \code{"column_name"}. Indicates which
#' group each protein, gene, etc. belongs to.
#' Used for colouring points on the plot.
#'
#' @param use_labels logical: if \code{FALSE} points will not be labelled. If
#' \code{TRUE} points will be labelled according to \code{label_fun}.
#'
#' @param label_fun character or function: use a single character if you want
#' to label all points with the same statement e.g. \code{"Hello"}, or
#' an \code{ifelse} statement e.g. \code{ifelse(p_val < 0.05)} to label
#' points conditionally.
#'
#' @param point_size integer: size of points on plot.
#'
#' @param label_size integer: size of label text
#'
#' @param x_lim numeric vector (length = 2): specifies the lower and upper
#' limits of the x axis.
#'
#' @param y_lim numeric vector (length = 2): specifies the lower and upper
#' limits of the y axis.
#'
#' @param legend_params mixed vector (length = 3): specifies the location,
#' title text size, and key text size of the legend. For example
#' \code{c("right", 11, 8)}.
#'
#' @param axis_params mixed vector (length = 2): specifies the colour and text
#' size of the both axes text. For example \code{c("black", 10)}.
#'
#' @return Returns a volcano plot with the \code{log2fc} column data on the
#' x axis and the \code{p_val} data on the y axis and with the points coloured
#' according to \code{group} and labels added if \code{use_labels = TRUE} and
#' \code{label_fun != " "}.
#'
#' @examples
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
#'   filter_na4(my_lfq, logic = "or", op = "<=",
#'              pat1 = "A10231_EV", val1 = 1,
#'              pat2 = "A10231_W", val2 = 1,
#'              pat3 = "A90028_EV", val3 = 1,
#'              pat4 = "A90028_W", val4 = 1)
#'
#' # normalise LFQ intensities
#' my_norm <- normalizeCyclicLoess(my_filt)
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
#' my_samples <- data.frame(T = rep(c("A1_EV", "A1_W",
#'                                    "A9_EV", "A9_W"), each = 3))
#'
#' my_design <- stats::model.matrix(~ 0 + T, data = my_samples)
#' colnames(my_design) <- c("A1_EV", "A1_W", "A9_EV", "A9_W")
#'
#' # define sample contrasts of interest
#' my_contrasts <- c("A1_EV - A1_W", "A9_EV - A9_W",
#'                   "A1_EV - A9_EV", "A1_W - A9_W")
#'
#' # make linear model fit and perform limma::eBayes()
#' my_efit <- limma_eBayes(mat = my_imp,
#'                         design = my_design,
#'                         contrasts = my_contrasts)
#'
#' # extract overall results table
#' result_overall <- get_results(efit = my_efit,
#'                               mat = my_imp,
#'                               p_val = 0.001,
#'                               lfc = 0,
#'                               type = "overall")
#'
#' # extract list of results tables for each individual contrast
#' result_list <- get_results(efit = my_efit,
#'                            mat = my_imp,
#'                            p_val = 0.001,
#'                            lfc = 0,
#'                            type = "individual")
#'
#' # see results table for A9_EV versus A9_W contrast
#' result_a9 <- result_list[[2]]
#'
#' # generate volcano plot
#' my_plot <- plot_volcano(data = result_a9,
#'                         p_val = adj.P.Val,
#'                         log2fc = logFC,
#'                         group = group,
#'                         label_fun = ifelse(logFC >= 6,
#'                                            CGD_gene_name,
#'                                            ""),
#'                         point_size = 0.6,
#'                         label_size = 2,
#'                         x_lim = c(-10, 12),
#'                         y_lim = c(0, 8),
#'                         legend_params = c("left", 11, 8),
#'                         axis_params = c("black", 10)) +
#'   scale_colour_manual(labels = c("EV enriched",
#'                                  "WCL enriched",
#'                                  "Not significant"),
#'                       values = c("red",
#'                                  "blue",
#'                                  "grey")
#'
#' @export
plot_volcano <- function(data, p_val, log2fc, group,
                         use_labels = FALSE,
                         label_fun = " ",
                         point_size = 1, label_size = 2,
                         x_lim = c(-10, 10), y_lim = c(0, 10),
                         legend_params = c("left", 11, 10),
                         axis_params = c("black", 10)) {

  # enquo data columns and label function
  p_val <- rlang::enquo(p_val)
  log2fc <- rlang::enquo(log2fc)
  group <- rlang::enquo(group)

  # plot all data that has a valid log2fc value
  p <- ggplot(data = dplyr::filter(data, !is.na(!!log2fc)),
              mapping = aes(x = !!log2fc, y = -log10(!!p_val))) +
    geom_point(mapping = aes(colour = !!group), size = point_size) +
    theme_classic(base_size = 11)

  # adjust legend position, text sizes, text colours
  p <- p +
    theme(legend.position = legend_params[1],
          legend.title = element_text(size = legend_params[2],
                                      face = "bold"),
          legend.text = element_text(size = legend_params[3]),
          axis.text = element_text(colour = axis_params[1],
                                   size = axis_params[2]))

  # adjust size of dots in legend and
  # add axis limits and axis labels
  p <- p +
    guides(colour = guide_legend(override.aes = list(size = 3)))+
    scale_x_continuous(limits = x_lim, expand = c(0,0)) +
    xlab(expression(log[2]("FC"))) +
    scale_y_continuous(limits = y_lim, expand = c(0,0)) +
    ylab(expression(-log[10]("adj. p-val")))

  # add text annotations to certain points on the plot
  # or just return the plot
  if(use_labels == TRUE) {
    label_fun <- rlang::enquo(label_fun)

    p <- p +
      ggrepel::geom_text_repel(data = dplyr::filter(data, !is.na(!!log2fc)),
                               mapping = aes(label = !!label_fun),
                               show.legend = FALSE,
                               size = label_size)
    p

  } else {p}
}
