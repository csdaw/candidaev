#' Extract differential abundance analysis results table
#'
#' @description This function is a wrapper around the \code{\link[limma]{topTable}} function from
#' \pkg{limma}. It combines the output of \code{\link[limma]{topTable}} with the numeric matrix
#' used in the \code{\link{limma_eBayes}} function to produce a differential protein abundance
#' results table.
#'
#' This results table contains the following:
#'
#' \itemize{
#' \item Protein names, identifiers, and functions
#' \item \code{log2} LFQ intensity data for each protein in each sample
#' \item Mean \code{log2} LFQ intensities for each protein in each sample group
#' \item \code{log2} fold change (\code{log2FC}) values for each protein
#' for the sample group comparison (contrast) of interest
#' \item \pkg{limma} moderated t- and B-statistics
#' \item p-values and Benjamini-Hochberg (BH) adjusted p-values (q-values)
#' \item An indication of whether the protein is significantly different between the sample
#' groups contrasted according to the adjusted p-value threshold selected.
#' \item An indication of if the protein is exclusive or enriched in one group or
#' another (only if \code{type = "individual"}
#' }
#'
#' See the \strong{Examples} section for an example of how to use this function.
#'
#' @param efit an object of class \code{MArrayLM} produced by \code{\link{limma_eBayes}}
#'
#' @param mat a numeric matrix used in \code{\link{limma_eBayes}} to produce \code{efit}
#'
#' @param p_val the BH adjusted p-value below which proteins are considered significantly different
#'
#' @param lfc the minimum \code{log2FC} required for significance
#'
#' @param type results table output type: can be "overall" to get a single combined result
#' (i.e. only 1 adjusted p-value) for all the contrasts
#' specified in \code{\link{limma_eBayes}} or "individual" to get results
#' (i.e. multiple adjusted p-values) for each individual
#' contrast specified in \code{\link{limma_eBayes}}. Usually "individual" is more informative.
#'
#' @return an object of class \code{data.frame} if \code{type = "overall"}, or a \code{list} of
#' \code{data.frames} if \code{type = "individual"}, 1 for each contrast specified in
#' \code{\link{limma_eBayes}}. Access the \code{data.frames} in the \code{list} by subsetting
#' the list, for example:
#'
#' \code{result_1 <- get_results(my_efit, my_mat, type = "individual")[[1]]}
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
#' result_overall <- get_results(my_efit, my_imp,
#'                               p_val = 0.001, lfc = 0, type = "overall")
#'
#' # extract list of results tables for each individual contrast
#' result_list <- get_results(my_efit, my_imp,
#'                            p_val = 0.001, lfc = 0, type = "individual")
#'
#' # see results table for A9_EV versus A9_W contrast
#' result_a9 <- result_list[[2]]
#'
#' @export
get_results <- function(efit, mat, p_val = 0.01, lfc = 0, type = c("individual", "overall")) {
  if(type == "overall") {
    df <- as.data.frame(mat) %>%
      tibble::rownames_to_column(var = "UP_accession")

    tt <- limma::topTable(efit, number = Inf, sort.by = "none") %>%
      tibble::rownames_to_column(var = "UP_accession")

    result <- left_join(df, tt, by = "UP_accession") %>%
      mutate(significant = case_when(adj.P.Val < p_val ~ TRUE,
                                     adj.P.Val > p_val ~ FALSE),
             CGD_gene_name = match_id(UP_accession, uniprot, "UP_accession", "CGD_gene_name"),
             Protein_name = match_id(UP_accession, uniprot, "UP_accession", "Protein_name")) %>%
      select(UP_accession, CGD_gene_name, Protein_name, everything())

    colnames(result) <- gsub("\\.{3}", ".vs.", colnames(result))
  }

  if(type == "individual") {
    result <- list()

    contrasts <- colnames(stats::coef(efit))

    df <- as.data.frame(mat) %>%
      tibble::rownames_to_column(var = "UP_accession")

    for(i in 1:length(contrasts)) {
      tt <- limma::topTable(efit, number = Inf, coef = i) %>%
        tibble::rownames_to_column(var = "UP_accession")

      c1 <- unlist(strsplit(contrasts[i], " - "))[1]
      c1_labels <- c(paste0(c1, "_mean"), paste(c1, "up"), paste(c1, "ex"))

      c2 <- unlist(strsplit(contrasts[i], " - "))[2]
      c2_labels <- c(paste0(c2, "_mean"), paste(c2, "up"), paste(c2, "ex"))

      lfq_df <- df %>%
        select(UP_accession, starts_with(c1), starts_with(c2)) %>%
        mutate(!!c1_labels[1] := rowMeans(select(., starts_with(c1)), na.rm = TRUE),
               !!c2_labels[1] := rowMeans(select(., starts_with(c2)), na.rm = TRUE))
      lfq_df[is.na(lfq_df)] <- NA

      lfq_cols <- unlist(strsplit(contrasts[i], " - ")) %>%
        paste(., collapse = "|")

      result[[i]] <- tt %>%
        left_join(lfq_df, ., by = "UP_accession") %>%
        mutate(significant = case_when(adj.P.Val < p_val & logFC > lfc ~ TRUE,
                                       adj.P.Val < p_val & logFC < -lfc ~ TRUE,
                                       adj.P.Val < p_val & logFC < lfc & logFC > -lfc ~ FALSE,
                                       adj.P.Val > p_val ~ FALSE),
               group = case_when(significant == FALSE ~ "not sig",
                                 significant == TRUE & logFC > lfc ~ paste(c1, "up"),
                                 significant == TRUE & logFC < -lfc ~ paste(c2, "up"),
                                 is.na(significant) == TRUE & is.na(.[[c1_labels[1]]]) == TRUE ~ paste(c2, "ex"),
                                 is.na(significant) == TRUE & is.na(.[[c2_labels[1]]]) == TRUE ~ paste(c1, "ex")),
               CGD_gene_name = match_id(UP_accession, uniprot, "UP_accession", "CGD_gene_name"),
               Protein_name = match_id(UP_accession, uniprot, "UP_accession", "Protein_name")) %>%
        filter(is.na(.[[c1_labels[1]]]) == FALSE | is.na(.[[c2_labels[1]]]) == FALSE) %>%
        select(UP_accession, CGD_gene_name, Protein_name, everything(), -AveExpr)
    }

    names(result) <- gsub(" - ", ".vs.", contrasts)
  }


  return(result)
}
