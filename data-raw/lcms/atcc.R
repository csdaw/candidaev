# source remove_dotdot function
source("R/remove_dotdot.R")

# load ATCC10231 and ATCC90028 yeast LC-MS/MS data
atcc <- read.table("data-raw/lcms/atcc-proteinGroups.txt", sep = "\t", header = TRUE,
                    stringsAsFactors = FALSE)

# remove duplicate periods in column names
atcc <- remove_dotdot(atcc)

# write output .rda file
usethis::use_data(atcc, overwrite = TRUE)
