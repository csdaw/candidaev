#' Title
#'
#' Internal function to get ComplexHeatmap::HeatmapAnnotation object.
#'
#' @param mat Description
#' @param exd Description
#' @param indicate Description
#'
#' @return Description
#'
#' @examples
#' # example
#'
get_annotation2 <- function(mat, exd, indicate) {
  assertthat::assert_that(
    is.matrix(mat),
    is.data.frame(exd),
    assert_exd(exd),
    is.character(indicate))

  # Check indicate columns
  col_data <- exd %>%
    filter(ID %in% colnames(mat))
  columns <- colnames(col_data)
  if(all(!indicate %in% columns)) {
    stop("'",
         paste0(indicate, collapse = "' and/or '"),
         "' column(s) is/are not present in ",
         deparse(substitute(exd)),
         ".\nValid columns are: '",
         paste(columns, collapse = "', '"),
         "'.",
         call. = FALSE)
  }
  if(any(!indicate %in% columns)) {
    indicate <- indicate[indicate %in% columns]
    warning("Only used the following indicate column(s): '",
            paste0(indicate, collapse = "', '"),
            "'")
  }

  # Get annotation
  anno <- select(col_data, indicate)

  # Annotation color
  names <- colnames(anno)
  anno_col <- vector(mode="list", length=length(names))
  names(anno_col) <- names
  for(i in names) {
    var = anno[[i]] %>% unique() %>% sort()
    if(length(var) == 1)
      cols <- c("black")
    if(length(var) == 2)
      cols <- c("orangered", "cornflowerblue")
    if(length(var) < 7 & length(var) > 2)
      cols <- RColorBrewer::brewer.pal(length(var), "Pastel1")
    if(length(var) > 7)
      cols <- RColorBrewer::brewer.pal(length(var), "Set3")
    names(cols) <- var
    anno_col[[i]] <-  cols
  }

  # HeatmapAnnotation object
  ComplexHeatmap::HeatmapAnnotation(df = anno,
                                    col = anno_col,
                                    show_annotation_name = TRUE)
}
