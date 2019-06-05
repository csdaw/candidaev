# devtools::check() generates a note with a list of
# 'undefined global variables'
# This occurs due to non-standard evaluation used in dplyr, tidyr, and
# ggplot2 among other packages
# see this https://github.com/STAT545-UBC/Discussion/issues/451 and
# this https://bookdown.org/rdpeng/RProgDA/non-standard-evaluation.html for
# more information, and for other methods for avoiding the note.

# This script is a somewhat hacky way (but also a commonly used way) to
# avoid generating an undefined global variables note during
# devtools::check()

.onLoad <- function(libname = find.package("candidaev"), pkgname = "candidaev") {
  # CRAN note avoidance
  if(getRversion() >= "2.15.1")
    utils::globalVariables(
      c(# convert_lfq.R
        "Majority.protein.IDs",

        # get_results.R
        "UP_accession", "uniprot", "CGD_gene_name", "Protein_name", ".",
        "AveExpr",

        # plot_detect2.R
        "ID", "val", "rowname", "missval", "num", "cs", "cs_frac",

        # plot_frequency2.R
        "bin", "Var1", "Freq",

        # plot_go_bar.R
        "adj.p.val", "GO.term", "Cat.ratio", "GO.type",

        # plot_imputation2.R
        "var", "condition",

        # plot_mds
        "dim1", "dim2", "label",

        # tukey_hsd.R
        "comparison", "comparison2", "p.adj", "term", "group1", "group2",
        "adj.p.value")
      )
  invisible()
}

