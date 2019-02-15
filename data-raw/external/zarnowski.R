# if uniprot.rda doesn't exist then stop
if(file.exists("data/uniprot.rda") == FALSE) {
  stop("uniprot.rda does not exist. Run ./data-raw/reference/uniprot.R then try again.")
}

# load packages
library(dplyr)

# source package functions
source("R/match_id.R")

# load uniprot.rda
data(uniprot)

# load Zarnowski et al. 2018 Candida EV proteomics data
zarnowski <- read.csv(file = "data-raw/external/Zarnowski_et_al_2018.csv", header = TRUE,
                      fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# filter for proteins found in planktonic SN152 Candida EVs
zarnowski <- zarnowski %>%
  filter(Planktonic_NTS > 0)

# write output .rda file
usethis::use_data(zarnowski)
