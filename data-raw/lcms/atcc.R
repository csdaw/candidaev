# source remove_dotdot function
source("R/remove_dotdot.R")

# load ATCC10231 and ATCC90028 yeast LC-MS/MS data
atcc <- read.table("data-raw/lcms/atcc-proteinGroups.txt", sep = "\t", header = TRUE,
                    stringsAsFactors = FALSE)

# remove duplicate periods in column names
atcc <- remove_dotdot(atcc)

# rename columns so sample names don't start with a number
colnames(atcc) <- stringi::stri_replace_all_regex(str = colnames(atcc),
                                                  pattern = c("10231_EV", "10231_W", "90028_EV", "90028_W"),
                                                  replacement = c("A1_EV", "A1_W", "A9_EV", "A9_W"),
                                                  vectorize_all = FALSE)

# write output .rda file
usethis::use_data(atcc, overwrite = TRUE)
