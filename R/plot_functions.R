plot_dendro <- function(lfq_mat) {
  dis <- cluster::daisy(t(lfq_mat), metric = "gower")
  hc <- hclust(dis)
  plot(hc)
}

plot_venn <- function(vlist, use_rownames = c(TRUE, FALSE),
                      unip = NULL, plot = TRUE, file = NULL, output = c("tiff", "png"), ...) {
  # supress futile logger message from VennDiagram package
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

  if(use_rownames == TRUE & is.null(unip) == FALSE) {
    # extract rownames (UniProt acessions) for comparison
    comparison <- mapply(rownames, vlist)
    # convert UniProt accessions to gene names
    comparison <- lapply(comparison, function(x) match_uniprot(x, unip,
                                                               "CGD_gene_name", "UP_accession"))
  } else {comparison <- vlist}

  # plot venn or get venn partition lists
  if(plot == FALSE) {
    venn <- get.venn.partitions(comparison)
    return(venn)
  } else if(plot == TRUE & is.null(file) == TRUE) {
    venn_plot <- venn.diagram(x = comparison,
                              filename = NULL,
                              ...)
    grid.newpage()
    grid.draw(venn_plot)
  } else if(plot == TRUE & is.null(file) == FALSE) {
    venn_plot <- venn.diagram(x = comparison,
                              filename = NULL,
                              ...)
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
