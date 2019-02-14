# source remove_dotdot function
source("R/remove_dotdot.R")

# load DAY286 biofilm LC-MS/MS data
biofilm <- read.table("data-raw/lcms/biofilm-proteinGroups.txt", sep = "\t", header = TRUE,
                    stringsAsFactors = FALSE)

# remove duplicate periods in column names
biofilm <- remove_dotdot(biofilm)

# write output .rda file
usethis::use_data(biofilm, overwrite = TRUE)
