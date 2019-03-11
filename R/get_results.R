#' Title
#'
#' Description.
#'
#' @param efit Description
#' @param mat Description
#' @param p_val Description
#' @param lfc Description
#' @param type Description
#'
#' @return Description
#'
#' @examples
#' # example
#'
#' @export
get_results <- function(efit, mat, p_val = 0.01, lfc = 0, type = c("overall", "individual")) {
  if(type == "overall") {
    df <- as.data.frame(mat) %>%
      tibble::rownames_to_column(var = "UP_accession")

    tt <- topTable(efit, number = Inf, sort.by = "none") %>%
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

    contrasts <- colnames(coef(efit))

    df <- as.data.frame(mat) %>%
      tibble::rownames_to_column(var = "UP_accession")

    for(i in 1:length(contrasts)) {
      tt <- topTable(efit, number = Inf, coef = i) %>%
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
