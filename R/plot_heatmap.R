#' Plot a protein heatmap
#'
#' @description \code{plot_heatmap} generates a heatmap of proteins
#' based on either their log2 LFQ intensities across in each sample or
#' their log2FC between sample groups.
#'
#' This function uses the \code{\link[ComplexHeatmap]{Heatmap}} function from
#' \pkg{ComplexHeatmap} to generate the heatmap.
#' If the numeric matrix input does not have missing values, then the clustering
#' methods built into \code{\link[ComplexHeatmap]{Heatmap}} should be used for
#' row and column clustering. If there are \code{NA} values in the numeric matrix
#' input, then \code{clust_fun = "gower"} should be specified, which uses
#' Gower's formula via the \code{\link[cluster]{daisy}} function from the
#' package \pkg{cluster} for row and column clustering.
#'
#' @param mt numeric matrix: contains either log2 intensities or log2 fold change data.
#' If \code{NA} values are present, use \code{clust_fun = "gower"}.
#'
#' @param plot logical: if \code{TRUE} the heatmap will be plotted using
#' \code{\link[ComplexHeatmap]{draw}}. If \code{FALSE}, either a
#' \code{\link[ComplexHeatmap]{Heatmap-class}} object or a \code{data.frame} will be
#' generated, depending on the value of \code{df}.
#'
#' @param df logical: if \code{TRUE} a \code{data.frame} with the data underlying the
#' heatmap (include the cluster id) is generated, with the proteins in order of
#' appearance in the heatmap. If \code{FALSE}, a
#' \code{\link[ComplexHeatmap]{Heatmap-class}} object is generated.
#'
#' @param data_type character: can be either \code{"log2intensity"} or
#' \code{"log2fc"} depending on the data in the numeric matrix.
#'
#' @param clust_fun character: one of c("gower", "euclidean", "maximum",
#' "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman",
#' "kendall").
#'
#' @param split_type character: method for splitting rows into slices, one of c("kmeans", "cutree").
#' Use \code{"kmeans"} if \code{mt} does not contain \code{NA} values, or \code{"cutree"}
#' if \code{mt} does contain \code{NA} values.
#'
#' @param k integer: number of slices to split heatmap into.
#'
#' @param cluster_split logical: if rows are split, \code{TRUE} indicates that clustering
#' should be performed on the slice means. If \code{mt} contains \code{NA} values, use
#' \code{cluster_split = FALSE}.
#'
#' @param split_order integer vector: must be sequencial integers in a vector with the
#' same length as \code{k}. Use to manually specify the order of row slices.
#'
#' @param colour_lims numeric vector (length = 2): specify the limits of the divergent
#' legend colours.
#'
#' @param use_cbrewer logical: if \code{TRUE} then \code{\link[RColorBrewer]{brewer.pal}} is
#' used to define the legend colours.
#'
#' @param cbrewer_pal character: name of a divergent \pkg{RColorBrewer} palette. One of
#' c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral").
#'
#' @param colour_vals character vector (length = 3): use with \code{use_cbrewer = FALSE} if
#' the legend colours should be defined manually.
#'
#' @param legend_pos character: position of the legend, one of
#' c("left", "right", "top", "bottom").
#'
#' @param row_title_fontsize numeric: text size of the cluster titles.
#'
#' @param col_name_fontsize numeric: text size of the column titles.
#'
#' @param ... Other arguments passed to \code{\link[ComplexHeatmap]{Heatmap}}. Most often this
#' will include \code{heatmap_legend_param} for further customisation of the heatmap appearance.
#'
#' @return If \code{plot = TRUE}, plots a heatmap using \code{\link[ComplexHeatmap]{draw}}.
#' If \code{plot = FALSE} and \code{df = FALSE}, returns a
#' \code{\link[ComplexHeatmap]{Heatmap-class}} object. If \code{plot = FALSE} and \code{df = TRUE},
#' returns a \code{data.frame} with the proteins listed in order of appearance in the heatmap.
#'
#' @examples
#' ## log2 LFQ intensity type heatmap
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
#' result_overall <- get_results(efit = my_efit, mat = my_imp, p_val = 0.001, lfc = 0, type = "overall")
#'
#' # plot a log2 LFQ intensity heatmap
#' htmp_data <- result_overall %>%
#'   select(1, 4:15, 24) %>%
#'   filter(significant == TRUE) %>%
#'   tibble::column_to_rownames(var = "UP_accession") %>%
#'   select(-significant) %>%
#'   as.matrix()
#'
#' plot_heatmap(htmp_data,
#'              plot = TRUE,
#'              df = FALSE,
#'              data_type = "log2intensity",
#'              clust_fun = "euclidean",
#'              split_type = "kmeans",
#'              k = 5,
#'              legend_pos = "right",
#'              heatmap_legend_param = list(title = "legend title",
#'                                          color_bar = "continuous",
#'                                          direction = "vertical",
#'                                          legend_width = ggplot2::unit(5, "cm"),
#'                                          title_position = "lefttop-rot",
#'                                          at = seq(-10, 10, 2)))
#'
#'
#' ## log2FC type heatmap
#' # plot a log2FC heatmap
#' htmp_data2 <- result_overall %>%
#'   select(1, matches("EV.vs.*W"), 24) %>%
#'   filter(significant == TRUE) %>%
#'   tibble::column_to_rownames(var = "UP_accession") %>%
#'   select(-significant) %>%
#'   as.matrix()
#'
#' plot_heatmap(htmp_data2,
#'              plot = TRUE,
#'              df = FALSE,
#'              data_type = "log2fc",
#'              clust_fun = "canberra",
#'              split_type = "kmeans",
#'              k = 5,
#'              legend_pos = "right",
#'              split_order = c(2, 5, 1, 4, 3),
#'              heatmap_legend_param = list(title = expression(bold(log[2]("FC"))),
#'              color_bar = "continuous",
#'              direction = "vertical",
#'              legend_width = unit(5, "cm"),
#'              title_position = "lefttop-rot",
#'              title_gp = grid::gpar(colour = "black", fontsize = 8),
#'              labels_gp = grid::gpar(colour = "black", fontsize = 8),
#'              at = seq(-10, 10, 2)))
#'
#' @export
plot_heatmap <- function(mt,
                         plot = TRUE,
                         df = FALSE,
                         data_type = c("log2fc", "log2intensity"),
                         clust_fun = c("gower", "euclidean", "maximum", "manhattan",
                                       "canberra", "binary", "minkowski", "pearson",
                                       "spearman", "kendall"),
                         split_type = c("kmeans", "cutree"),
                         k = 5,
                         cluster_split = TRUE,
                         split_order = seq(1, k, 1),
                         colour_lims = c(-8, 8),
                         use_cbrewer = FALSE,
                         cbrewer_pal = NULL,
                         colour_vals = c("blue", "white", "red"),
                         legend_pos = "top",
                         row_title_fontsize = 6,
                         col_name_fontsize = 10,
                         ...) {

  # centre values depending on data type
  if(data_type == "log2intensity") {
    mt_mean <- mt %>%
      as.data.frame() %>%
      mutate(mean = rowMeans(., na.rm = TRUE))

    mt2 <- mt - mt_mean$mean
  } else if(data_type == "log2fc") {
    mt2 <- mt
  }

  # define clustering function
  clust_fun <- match.arg(clust_fun)

  if(clust_fun == "gower") {
    # used in DEP package heatmap function
    clust_dist <- function(x) {
      dist <- cluster::daisy(x, metric = "gower")
      dist[is.na(dist)] <- max(dist, na.rm = TRUE)
      return(dist)
    }

    # cluster rows
    row_clust <- stats::hclust(clust_dist(mt2))

  } else {
    # define clustering method
    clust_dist <- clust_fun

    # cluster rows
    row_clust <- stats::hclust(dist(mt2, method = clust_fun))

  }

  if(split_type == "cutree") {
    # extract row clusters
    row_cutree <- stats::cutree(row_clust,
                                k = k)

    # get a cluster ID for each row for splitting heatmap
    htmp_split <- row_cutree %>%
      factor(levels = split_order)
  } else if(split_type == "kmeans") {
    set.seed(1)
    row_kmeans <- kmeans(mt2, k)

    htmp_split <- row_kmeans$cluster %>%
      factor(levels = split_order)
  }

  # define breaks for colour palette
  legend_breaks <- seq(colour_lims[1], colour_lims[2],
                       (Mod(colour_lims[1] - colour_lims[2])/2)/5)

  # use colorbrewer colour palette or specific colours
  if(use_cbrewer == TRUE) {
    legend_cols <- RColorBrewer::brewer.pal(11, cbrewer_pal)

    colour_fun <- circlize::colorRamp2(legend_breaks, legend_cols)
  } else {
    legend_cols <- colorRampPalette(colour_vals)

    colour_fun <- circlize::colorRamp2(legend_breaks, legend_cols(11))
  }

  # define legend title
  if(data_type == "log2fc") {
    legend_title <- expression(bold(log[2]("FC")))
  } else if(data_type == "log2intensity") {
      legend_title <- expression("centred"~log[2]("intensity"))}



  # construct the heatmap
  htmp <- ComplexHeatmap::Heatmap(mt2,
                                  cluster_rows = TRUE,
                                  cluster_columns = TRUE,
                                  clustering_distance_rows = clust_dist,
                                  clustering_distance_columns = clust_dist,
                                  na_col = "black",
                                  col = colour_fun,
                                  show_row_names = FALSE,
                                  split = htmp_split,
                                  cluster_row_slices = cluster_split,
                                  row_title_gp = grid::gpar(fontsize = row_title_fontsize),
                                  column_names_gp = grid::gpar(fontsize = col_name_fontsize),
                                  ...)

  if(plot) {
    # draw the heatmap
    ComplexHeatmap::draw(htmp, heatmap_legend_side = legend_pos)
  } else {
    if(df == TRUE) {
      # return data table
      result <- mt2[, unlist(ComplexHeatmap::column_order(htmp))]

      if(split_type == "kmeans") {
        result <- cbind(result, cluster = row_kmeans$cluster)
      } else if(split_type == "cutree") {
        result <- cbind(result, cluster = row_cutree)
      }

      result <- result[unlist(ComplexHeatmap::row_order(htmp)), ] %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "id") %>%
        mutate(order = row_number())

      return(result)
    } else if(df == FALSE) {
      # return the heatmap but do not plot it
      return(htmp)
    }

  }

}
