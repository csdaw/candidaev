# load tidyNano package
library(tidyNano)

# combine nta data and extract metadata from filenames in [Size Data] section
nta <- nanocombine(dir = "data-raw/nta/") %>%
  nanotidy(sep_var = c("Strain", "Bio_rep", "Dilution", "Tech_rep"))

# write output to .rda file
usethis::use_data(nta, overwrite = TRUE)

