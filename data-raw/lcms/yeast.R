# source remove_dotdot function
source("R/remove_dotdot.R")

# load DAY286 yeast LC-MS/MS data
yeast <- read.table("data-raw/lcms/yeast-proteinGroups.txt", sep = "\t", header = TRUE,
                    stringsAsFactors = FALSE)

# remove duplicate periods in column names
yeast <- remove_dotdot(yeast)

# write output .rda file
usethis::use_data(yeast, overwrite = TRUE)
