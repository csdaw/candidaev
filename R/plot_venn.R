#' Plot venn diagram
#'
#' @description \code{plot_venn} is a wrapper function around
#' \code{\link[VennDiagram]{venn.diagram}} and
#' \code{\link[VennDiagram]{get.venn.partitions}}
#' from the package \pkg{VennDiagram}.
#'
#' This function  can generate a 2-way to 5-way venn diagram and output the
#' result as:
#' \itemize{
#' \item a tiff (lzw compressed), png, or svg image file.
#' \item a list of GRID objects that can be plotted
#' using \code{grid::grid.draw()}.
#' \item a data.frame containing the data underlying the Venn diagram
#' including lists of the members of each partition.
#' }
#'
#'
#' @param vlist list of vectors: can be integers or characters. Contains the
#' sets to be compared.
#'
#' @param use_uniprot logical: typically should be \code{FALSE}. If \code{TRUE}
#' then vlist should be a list of data.frames with row names corresponding
#' to UniProt accessions found in \code{\link{uniprot}}. The accessions will
#' be converted to gene names and then used for the comparison.
#'
#' @param type character: one of \code{"df"}, \code{"plot"}, or \code{"save"}.
#' Indicates which output format to use.
#'
#' @param output character: one of \code{"tiff"}, \code{"png"},
#' or \code{"svg"}. Used when \code{type = "save"} and indicates which image
#' format to save the Venn diagram to.
#'
#' @param output_file character: file path, where to save the image.
#' For example: \code{"./figures/my_venn.tiff"}.
#'
#' @param output_dim integer vector (length = 2): width and height of image to
#' be saved. Units of the dimensions for tiff and png files can be specified
#' using the \code{output_units} argument. For svg files the dimensions must
#' be in inches (e.g. \code(c(7, 7))).
#'
#' @param output_units character: one of \code{"px"}, \code{"in"},
#' \code{"cm"}, or \code{"mm"}. Indicates the units of the dimensions for
#' saving tiff and png files. Units for svg files are always in inches.
#'
#' @param output_res integer: resolution of the image (DPI) to be saved.
#'
#' @param output_pts integer: overall font size of labels
#'
#' @param ... Other arguments to be passed to
#' \code{\link[VennDiagram]{venn.diagram}}
#'
#' @return If \code{type = "plot"} returns a list of GRID objects to plot with
#' \code{grid::grid.draw()}. If \code{type = "df"} returns a data.frame with
#' the data underlying the Venn. If \code{type = "save"} saves an image
#' (tiff, png, or svg) of the Venn diagram to a specified file path.
#'
#' @examples
#' # make two data frames
#' v1 <- data.frame(colour = c("green", "red", "blue"),
#'                  level = c(60, 50, 45))
#'
#' v2 <- data.frame(colour = c("purple", "green", "pink"),
#'                  level = c(10, 13, 22))
#'
#' # make list of vectors to compare
#' comp <- list(c1 = v1$colour, c2 = v2$colour)
#'
#'
#' # plot Venn diagram with grid
#' my_venn <- plot_venn(vlist = comp,
#'                      type = "plot")
#' grid::grid.draw(my_venn)
#'
#' # get the data underlying the Venn
#' my_venn_data <- plot_venn(vlist = comp,
#'                           type = "df")
#'
#' # plot Venn diagram and save as png
#' plot_venn(vlist = comp,
#'           type = "save",
#'           output = "png",
#'           output_file = "path/to/save/to/my_venn.png")
#'
#' # plot Venn diagram and save as svg
#' plot_venn(vlist = comp,
#'           type = "save",
#'           output = "svg",
#'           output_file = "path/to/save/to/my_venn.svg",
#'           output_dim = c(6, 6))
#'
#' @importFrom grDevices dev.off png tiff svg
#' @export
plot_venn <- function(vlist, use_uniprot = FALSE,
                      type = c("df", "plot", "save"),
                      output = c("tiff", "png", "svg"),
                      output_file = NULL,
                      output_dim = c(1000, 1000),
                      output_units = "px",
                      output_res = 300,
                      output_pts = 14, ...) {
  # supress futile logger message from VennDiagram package
  futile.logger::flog.threshold(futile.logger::ERROR,
                                name = "VennDiagramLogger")

  if(use_uniprot == TRUE) {
    # extract rownames (UniProt acessions) for comparison
    comparison <- mapply(rownames, vlist)
    # convert UniProt accessions to gene names
    comparison <- lapply(comparison, function(x) match_id(x,
                                                          candidaev::uniprot,
                                                          "UP_accession",
                                                          "CGD_gene_name"))
  } else {comparison <- vlist}

  # plot venn and save file or
  # plot venn and save list of GRID objects or
  # get venn partitions
  if(type == "save") {
    venn_plot <- VennDiagram::venn.diagram(x = comparison,
                                           filename = NULL, ...)
    if(output == "tiff") {
      tiff(filename = output_file,
           width = output_dim[1],
           height = output_dim[2],
           res = output_res,
           units = output_units,
           pointsize = output_pts,
           family = "sans",
           type = "cairo",
           compression = "lzw")
      grid::grid.draw(venn_plot)
      dev.off()
    } else if (output == "png") {
      png(filename = output_file,
          width = output_dim[1],
          height = output_dim[2],
          res = output_res,
          units = output_units,
          pointsize = output_pts,
          family = "sans",
          type = "cairo")
      grid::grid.draw(venn_plot)
      dev.off()
    } else if (output == "svg") {
      svg(filename = output_file,
          width = output_dim[1],
          height = output_dim[2],
          pointsize = output_pts)
      grid::grid.draw(venn_plot)
      dev.off()
    }
  } else if(type == "plot") {
    venn_plot <- VennDiagram::venn.diagram(x = comparison,
                                           filename = NULL, ...)
    return(venn_plot)
  } else if(type == "df") {
    venn <- VennDiagram::get.venn.partitions(comparison)
    return(venn)
  }
}
