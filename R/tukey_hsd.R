#' Compute Tukey Honest Significant Differences
#'
#' @description \code{tukey_hsd} is a wrapper function around
#' \code{\link[stats]{TukeyHSD}} from the \pkg{stats} package.
#'
#' @param x fitted model object: usually an \code{\link[stats]{aov}} fit.
#'
#' @param ... Other arguments passed to \code{\link[stats]{TukeyHSD}}.
#'
#' @return Returns a data.frame with each sample comparison and
#' adjusted p-values for the comparison of sample means.
#'
#' @examples
#' # construct data.frame with nta experiment info
#' nta_metadata <- data.frame(id = c("ATCC10231_1", "ATCC10231_2",
#'                                   "ATCC10231_3", "ATCC90028_1",
#'                                   "ATCC90028_2", "ATCC90028_3",
#'                                   "Biofilm_1", "Biofilm_2",
#'                                   "Biofilm_3", "Biofilm_4",
#'                                   "Biofilm_5", "DAY286_1",
#'                                   "DAY286_2", "DAY286_3"),
#'                            protein_conc = c(0.192, 0.282,
#'                                             0.287, 0.424,
#'                                             0.716, 0.584,
#'                                             0.204, 0.198,
#'                                             0.297, 0.193,
#'                                             0.416, 0.437,
#'                                             0.360, 0.447),
#'                            stringsAsFactors = FALSE)
#'
#' # average the nta technical replicate data
#' nta_techavg <- nta %>%
#'   nanolyze(particle_size, Strain, Bio_rep, Dilution,
#'            name = "Tech_rep",
#'            param_var = True_count)
#' # average the nta biological replicate data
#' nta_bioavg <- nta_techavg %>%
#'   nanolyze(particle_size, Strain,
#'            name = "Bio_rep",
#'            param_var = Tech_rep_mean) %>%
#'   mutate(facet = case_when(Strain == "Biofilm" ~ "second",
#'                            TRUE ~ "first"))
#'
#' # determine the total particle concentration for each
#' # biological replicate
#' nta_total <- nta_techavg %>%
#'   nanocount(Strain, Bio_rep, Dilution,
#'             param_var = Tech_rep_mean) %>%
#'   tidyr::unite("id", Strain, Bio_rep) %>%
#'   mutate(protein_conc = as.numeric(match_id(id,
#'                                             nta_metadata,
#'                                             "id",
#'                                             "protein_conc")),
#'          od = match_id(id, nta_metadata, "id", "OD600"),
#'          part_per_prot = Particle_count / protein_conc / 1000) %>%
#'   tidyr::separate(col = id,
#'                   into = c("Strain", "Bio_rep"),
#'                   sep = "_",
#'                   remove = TRUE)
#'
#' # use ANOVA with Tukey HSD to compare mean particle per ug protein
#' # ratio across the strains
#' nta_total_tukey <- aov(part_per_prot ~ Strain, data = nta_total) %>%
#'   tukey_hsd()
#'
#' @export
tukey_hsd <- function(x, ...) {
  res <- stats::TukeyHSD(x, ...) %>%
    broom::tidy() %>%
    mutate(comparison2 = comparison) %>%
    tidyr::separate(comparison2, into= c("group2", "group1"), sep = "-") %>%
    rename(p.adj = adj.p.value) %>%
    mutate(p.adj = signif(p.adj, 2)) %>%
    select(term, group1, group2, everything(), -term)
  res
}
