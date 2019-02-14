# load DAY286 biofilm LC-MS/MS experimental design
biofilm_exp <- read.table("data-raw/lcms/biofilm-expdesign.txt", sep = "\t",
                          header = TRUE, stringsAsFactors = FALSE)

# write output .rda file
usethis::use_data(biofilm_exp, overwrite = TRUE)
