# load DAY286 yeast LC-MS/MS experimental design
yeast_exp <- read.table("data-raw/lcms/yeast-expdesign.txt", sep = "\t",
                        header = TRUE, stringsAsFactors = FALSE)

# write output .rda file
usethis::use_data(yeast_exp, overwrite = TRUE)
