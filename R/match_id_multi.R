#' Create data.frame or list based on matching values with a reference table
#'
#' @description \code{match_id_multi} matches a character column in a \code{data.frame}
#' with values (which may or may not be
#' separated by semicolons) to a reference table with a character column containing matching
#' values (which may or may not be separated by semicolons) and then produces a list of
#' vectors with corresponding information from other selected columns in the reference
#' table.
#'
#' \code{match_id_multi} is  useful for adding multiple columns to a \code{data.frame}
#' based on matching with a single reference table.
#'
#' If you want to add multiple columns to a \code{data.frame} based on matching to multiple
#' reference tables, see \code{\link{match_id}}.
#'
#' @param df data.frame: contains the \code{id} column
#'
#' @param id string: name of a character column with values to be matched
#'
#' @param ref data.frame: reference table with values to be matched against
#'
#' @param match string: name of character column in reference table with
#' values to be matched against
#'
#' @param new vector: name(s) of character column(s) in reference table with
#' corresponding information/values of interest to append to \code{df}
#'
#' @param as_list logical: if \code{TRUE} output is a list of vectors and if
#' \code{FALSE} output is a data.frame
#'
#' @param ... Other arguments passed to \code{\link{match_id}}. Used for specifying
#' \code{concat = FALSE} or \code{str_split = FALSE} where required. See
#' \code{\link{match_id}} for examples.
#'
#' @return If \code{as_list = TRUE}, returns a list of vectors with the values
#' from the columns specified by \code{new}. If \code{as_list = FALSE} returns
#' a \code{data.frame}
#' with the column specified by \code{new} added to the end.
#'
#' If there is a value in \code{id} which is not present in \code{match},
#' the output for that particular value will be the value \code{NA} or
#' the string \code{"NA"}.
#'
#' @examples
#' # make a reference table
#' my_ref <- data.frame(accession = c("A12345", "B45678", "C09876", "D20398"),
#'                      gene_name = c("AAA1", "BBB56", "CCC9", "DDD-110"),
#'                      protein_name = c("Alcohol dehydrogenase",
#'                                       "Butylase",
#'                                       "Chitinase",
#'                                       "Uncharacterized protein"))
#' # make a data.frame with an id column with some
#' # identifiers we want to match
#' my_data <- data.frame(id = c("A12345", "C09876", "E55566", "D20398;B45678"),
#'                       sample_A1 = c(NA, NA, 30, NA),
#'                       sample_A2 = c(NA, 15, 31, 23),
#'                       sample_A3 = c(NA, NA, 32, 24),
#'                       sample_B1 = c(23, NA, 29, 22),
#'                       sample_B2 = c(24, NA, 30, NA),
#'                       sample_B3 = c(21, 14, 31, 24))
#'
#' # add the gene name and protein name columns to the data table
#' my_data2 <- match_id_multi(df = my_data,
#'                            id = "id",
#'                            ref = my_ref,
#'                            match = "accession",
#'                            new = c("protein_name", "gene_name"))
#'
#' # or get a list of gene names and protein names
#' my_list <- match_id_multi(df = my_data,
#'                           id = "id",
#'                           ref = my_ref,
#'                           match = "accession",
#'                           new = c("protein_name", "gene_name",
#'                           as_list = TRUE))
#'
#' # use with dplyr
#' library(dplyr)
#'
#' my_data3 <- my_data %>%
#'   select(-sample_B3) %>%
#'   match_id_multi(.,
#'                  id = "id",
#'                  ref = my_ref,
#'                  match = "accession",
#'                  new = c("protien_name", "gene_name"))
#'
#' @export
match_id_multi <- function(df, id, ref, match, new, as_list = FALSE, ...) {
  new_columns <- lapply(new, function(x) match_id(id = df[[id]],
                                                  ref = ref,
                                                  match = match,
                                                  new = x,
                                                  ...))

  names(new_columns) <- new

  if(as_list == FALSE) {
    new_df <- bind_cols(df, new_columns)

    new_df
  } else {new_columns}
}
