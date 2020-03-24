#### Load libraries ####
library(dplyr)

#### Define custom functions ####
filter_zero <- function(data, op = c("==", "<=", ">="), pat, val) {
  # refer to operator by name
  op <- as.name(op)

  # get sum of values in column group
  col_sum <- apply(data[, c(grep(pat, colnames(data)))], 1, function(x) sum(x == 0, na.rm = TRUE))

  # subset data based on sum of values in column group
  subset(data, sapply(col_sum, op, val))
}

#### Load in MaxQuant peptides.txt outputs ####
y_pep <- readr::read_tsv("data-raw/lcms/yeast_peptides.txt") %>%
  rename_all(list(~make.names(.))) %>%
  select(Sequence,
         Length:End.position,
         MS.MS.Count,
         matches("Intensity"))

b_pep <- readr::read_tsv("data-raw/lcms/biofilm_peptides.txt") %>%
  rename_all(list(~make.names(.))) %>%
  select(Sequence,
         Length:End.position,
         MS.MS.Count,
         matches("Intensity"))

a_pep <- readr::read_tsv("data-raw/lcms/atcc_peptides.txt") %>%
  rename_all(list(~make.names(.))) %>%
  select(Sequence,
         Length:End.position,
         MS.MS.Count,
         matches("Intensity"))

#### Define UniProt accessions for proteins of interest ####
sur7 <- "Q5A4M8"

evp1 <- "A0A1D8PKU6"


#### Find Sur7 peptides ####
# Filter for peptides with:
# Less than 2 missed cleavages
# A spectral count of at least 5 MS/MS spectra
# Peptide intensities values in min 2/3 or 4/5 EV reps

# DAY286 yeast EVs
sur7_y_pep <- y_pep %>%
  filter(Proteins %in% sur7,
         Missed.cleavages < 2,
         MS.MS.Count >= 5) %>%
  filter_zero(., op = "<=", pat = "EV", val = 1)

# DAY286 biofilm EVs
sur7_b_pep <- b_pep %>%
  filter(Proteins %in% sur7,
         Missed.cleavages < 2,
         MS.MS.Count >= 5) %>%
  filter_zero(., op = "<=", pat = "EV", val = 1)

# ATCC90028 and ATCC10231 EVs
sur7_a_pep <- a_pep %>%
  filter(Proteins %in% sur7,
         Missed.cleavages < 2,
         MS.MS.Count >= 5) %>%
  filter_zero(., op = "<=", pat = "90028_EV", val = 1) %>%
  filter_zero(., op = "<=", pat = "10231_EV", val = 1)

# Common peptides to all data sets
sur7_pep_common <- Reduce(intersect, list(sur7_y_pep$Sequence,
                                          sur7_b_pep$Sequence,
                                          sur7_a_pep$Sequence))

#### Find Evp1 peptides ####
# Filter for peptides with:
# Less than 2 missed cleavages
# A spectral count of at least 5 MS/MS spectra
# Peptide intensities values in min 2/3 or 4/5 EV reps

# DAY286 yeast EVs
evp1_y_pep <- y_pep %>%
  filter(Proteins %in% evp1,
         Missed.cleavages < 2,
         MS.MS.Count >= 5) %>%
  filter_zero(., op = "<=", pat = "EV", val = 1)

# DAY286 biofilm EVs
evp1_b_pep <- b_pep %>%
  filter(Proteins %in% evp1,
         Missed.cleavages < 2,
         MS.MS.Count >= 5) %>%
  filter_zero(., op = "<=", pat = "EV", val = 1)

# ATCC90028 and ATCC10231 EVs
evp1_a_pep <- a_pep %>%
  filter(Proteins %in% evp1,
         Missed.cleavages < 2,
         MS.MS.Count >= 5) %>%
  filter_zero(., op = "<=", pat = "90028_EV", val = 1) %>%
  filter_zero(., op = "<=", pat = "10231_EV", val = 1)

# Common peptides to all data sets
evp1_pep_common <- Reduce(intersect, list(evp1_y_pep$Sequence,
                                          evp1_b_pep$Sequence,
                                          evp1_a_pep$Sequence))

#### Summary table ####
result <- tibble::tibble(Protein = c("Sur7", "Evp1"),
                         Accession = c(sur7, evp1),
                         yeast.filt.pep = c(paste(sur7_y_pep$Sequence, collapse = " "),
                                            paste(evp1_y_pep$Sequence, collapse = " ")),
                         biofilm.filt.pep = c(paste(sur7_b_pep$Sequence, collapse = " "),
                                              paste(evp1_b_pep$Sequence, collapse = " ")),
                         a9.filt.pep = c(paste(sur7_a_pep$Sequence, collapse = " "),
                                         paste(evp1_a_pep$Sequence, collapse = " ")),
                         a1.filt.pep = c(paste(sur7_a_pep$Sequence, collapse = " "),
                                         paste(evp1_a_pep$Sequence, collapse = " ")),
                         common.pep = c(paste(sur7_pep_common, collapse = " "),
                                        paste(evp1_pep_common, collapse = " ")))



#### Full Sur7 and Evp1 peptide data ####

y_pep_se <- y_pep %>%
  filter(Proteins %in% c(sur7, evp1))

b_pep_se <- b_pep %>%
  filter(Proteins %in% c(sur7, evp1))

a_pep_se <- a_pep %>%
  filter(Proteins %in% c(sur7, evp1))

#### Export data ####
sheets <- list("Summary" = result,
               "DAY286 yeast" = y_pep_se,
               "DAY286 biofilm" = b_pep_se,
               "ATCC" = a_pep_se)

writexl::write_xlsx(sheets,
                    path = "inst/ev_peptides.xlsx")
