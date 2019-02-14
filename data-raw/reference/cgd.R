# source fill_blank function
source("R/fill_blank.R")

# read input .tab file
cgd <- read.delim("data-raw/reference/20181209_C_albicans_SC5314_A22_ccf.tab",
                  header = FALSE,
                  sep = "\t",
                  comment.char = "!",
                  stringsAsFactors = FALSE)

# add column names
colnames(cgd) <- c("Systematic_name",
                         "Gene_name",
                         "Aliases",
                         "Feature_type",
                         "Chromosome",
                         "Start_coord",
                         "Stop_coord",
                         "Strand",
                         "CGDID",
                         "Secondary_CGDID",
                         "Description", "
                           Date_created",
                         "Seq_coord_version",
                         "Blank",
                         "Blank2",
                         "Gene_name_res_date",
                         "Res_name_also_standard",
                         "Scerevisiae_orthologs")

# extract orf identifiers (Feature_name) from aliases column
cgd$Feature_name <- stringr::str_extract(cgd$Aliases, "(orf19.[0-9.]+)|(CaalfMp[0-9]{2})")

# replace blank gene names with orf identifiers
cgd <- fill_blank(cgd, "Gene_name", "Feature_name")

# remove blank columns
cgd <- within(cgd, rm(Blank, Blank2))

# write output .rda file
usethis::use_data(cgd, overwrite = TRUE)
