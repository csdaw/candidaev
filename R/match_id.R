#' Replace values based on match
#'
#' \code{match_id} matches a character vector with values (which may or may not be
#' separated by semicolons) to a reference table with a character column containing
#' values (which must not be separated by semicolons) and then replaces those
#' values with corresponding information from another character column in the
#' reference table.
#'
#' @param id character vector: the values to be matched
#' @param ref data frame: reference table with values to be matched against
#' @param match string: column name of character column in reference table with
#' values to be matched against
#' @param new string: column name of character column in reference table with new
#' information to replace matches
#' @param concat Description.
#' @param str_split Description.
#'
#' @return A vector with the same length as \code{id} but with the values replaced
#' with corresponding information from the reference table.
#'
#' If there is a value to be matched which is not present in the reference table,
#' the value will be replaced with the string "NA"
#'
#' @examples
#' # load dplyr
#' library(dplyr)
#'
#' # make a reference table
#' ref_df <- data.frame(id = c("a1", "a2", "a3"), last_name = c("Smith", "Brown", "Doe"))
#'
#' # make a data frame
#' my_names <- data.frame(id = c("a1;a3", "a10", "a2"), first = c("Jane", "Mary", "John"))
#'
#' # use match_id to make new column in my_names
#' my_names$last <- match_id(my_names$id, ref_df, "id", "last_name")
#'
#' @export
match_id <- function(id, ref, match, new, concat = TRUE, str_split = TRUE) {
  if(concat == TRUE & str_split == TRUE) {
    stringr::str_extract_all(id, "[^;]+") %>%
      lapply(., function(x) match(x, ref[[match]])) %>%
      Map("[", list(as.character(ref[[new]])), .) %>%
      lapply(., function(x) paste(x, collapse = ";")) %>%
      unlist()
  } else if(concat == FALSE & str_split == TRUE) {
    stringr::str_extract_all(id, "[^;]+") %>%
      lapply(., function(x) match(x, ref[[match]])) %>%
      Map("[", list(as.character(ref[[new]])), .) %>%
      unlist()
  } else if (concat == TRUE & str_split == FALSE) {
    lapply(id, function(x) match(x, ref[[match]])) %>%
      Map("[", list(as.character(ref[[new]])), .) %>%
      lapply(., function(x) paste(x, collapse = ";")) %>%
      unlist()
  }
}
