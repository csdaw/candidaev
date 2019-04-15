#' Title
#'
#' Description.
#'
#' @param mt Description.
#' @param plot Description.
#' @param data_type Description.
#' @param clust_fun Description.
#' @param split_type Description.
#' @param k Description.
#' @param cluster_split Description.
#' @param split_order Description.
#' @param colour_lims Description.
#' @param use_cbrewer Description.
#' @param cbrewer_pal Description.
#' @param colour_vals Description.
#' @param legend_pos Description.
#' @param row_font_size Description.
#' @param col_font_size Description.
#' @param ... Description.
#'
#' @return Description.
#'
#' @examples
#'
#' # example
#'
#' @export
plot_heatmap <- function(mt,
                         plot = TRUE,
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
                         row_font_size = 6,
                         col_font_size = 10,
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
                                  row_names_gp = grid::gpar(fontsize = row_font_size),
                                  column_names_gp = grid::gpar(fontsize = col_font_size),
                                  ...)

  if(plot) {
    # draw the heatmap
    ComplexHeatmap::draw(htmp, heatmap_legend_side = legend_pos)
  } else {
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

  }

}
