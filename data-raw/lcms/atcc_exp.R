# load ATCC10231 and ATCC90028 yeast LC-MS/MS experimental design
atcc_exp <- read.table("data-raw/lcms/atcc-expdesign.txt", sep = "\t",
                          header = TRUE, stringsAsFactors = FALSE)

# write output .rda file
usethis::use_data(atcc_exp, overwrite = TRUE)
