plot_dendro <- function(lfq_mat) {
  dis <- cluster::daisy(t(lfq_mat), metric = "gower")
  hc <- hclust(dis)
  plot(hc)
}

plot_venn <- function(mats, plot = TRUE, file = NULL, output = c("tiff", "png"), ...) {
  # Supress futile logger message from VennDiagram package
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

  # use matrix rownames for comparison
  comparison <- mapply(rownames, mats)

  # plot venn or get venn partition lists
  if(plot == FALSE) {
    venn <- get.venn.partitions(comparison)
    return(venn)
  } else if(plot == TRUE & is.null(file) == TRUE) {
    venn_plot <- venn.diagram(x = comparison,
                              filename = NULL,
                              height = 800,
                              width = 800,
                              resolution = 300,
                              lty = "solid",
                              lwd = 2)
    grid.draw(venn_plot)
  } else if(plot == TRUE & is.null(file) == FALSE) {
    venn_plot <- venn.diagram(x = comparison,
                              filename = NULL,
                              imagetype = "tiff",
                              compression = "lzw",
                              height = 800,
                              width = 800,
                              resolution = 300,
                              lty = "solid",
                              lwd = 2)
    if(output == "tiff") {
      tiff(filename = file,
           width = 800,
           height = 800,
           res = 300,
           units = "px",
           pointsize = 12,
           type = "cairo",
           compression = "lzw")
      grid.draw(venn_plot)
      dev.off()
    } else if (output == "png") {
      png(filename = file,
          width = 800,
          height = 800,
          res = 300,
          units = "px",
          type = "cairo")
      grid.draw(venn_plot)
      dev.off()
    }
  }
}
