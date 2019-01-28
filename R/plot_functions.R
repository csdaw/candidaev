plot_dendro <- function(lfq_mat) {
  dis <- cluster::daisy(t(lfq_mat), metric = "gower")
  hc <- hclust(dis)
  plot(hc)
}
