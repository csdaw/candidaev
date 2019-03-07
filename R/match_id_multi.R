#' Title
#'
#' Description.
#'
#' @param df Description
#' @param id Description
#' @param ref Description
#' @param match Description
#' @param new Description
#' @param as_list Description
#'
#' @return Description
#'
#' @examples
#' # make a reference table
#' ref_df <- data.frame(id = c("a1", "a2", "a3"),
#'                      last_name = c("Smith", "Brown", "Doe"),
#'                      atr1 = c("Blue", "Brown", "Yellow"),
#'                      atr2 = c("Big", "Medium", "Small"),
#'                      stringsAsFactors = FALSE)
#'
#' # make a dataset to which you want to append columns
#' # note that row 3 has an id which is NOT in the reference table
#' my_names <- data.frame(employee_id = c("a1;a3", "a2", "a5"),
#'                        first_name = c("Jane", "Mary", "John"),
#'                        stringsAsFactors = FALSE)
#'
#' # append selected columns to my_names based on matching ids
#' # note that row 3 has NA in all appended columns because
#' # the employee_id = a5 is not in the reference table
#' my_names <- match_id_multi(df = my_names,
#'                            id = "employee_id",
#'                            ref = ref_df,
#'                            match = "id",
#'                            new = c("last_name", "atr2", "atr1"))
#'
#' # alternatively, return a list of character vectors for the
#' columns selected in by the 'new' argument
#' my_list <- match_id_multi(df = my_names,
#'                           id = "employee_id",
#'                           ref = ref_df,
#'                           match = "id",
#'                           new = c("atr1", "atr2"))
#'
#' @export
#'
match_id_multi <- function(df, id, ref, match, new, as_list = FALSE) {
  if(as_list == FALSE) {
    new_cols <- list()

    for(i in new) {
    col <- stringr::str_extract_all(df[[id]], "[^;]+") %>%
      lapply(., function(x) match(x, ref[[match]])) %>%
      Map("[", list(as.character(ref[[i]])), .) %>%
      lapply(., function(x) paste(x, collapse = ";")) %>%
      unlist()
    new_cols[[i]] <- col
    }

    result <- as.data.frame(do.call(cbind, c(df, new_cols)), stringsAsFactors = FALSE)
    } else if(as_list == TRUE) {
      result <- list()

      for(i in new) {
        col <- stringr::str_extract_all(df[[id]], "[^;]+") %>%
          lapply(., function(x) match(x, ref[[match]])) %>%
          Map("[", list(as.character(ref[[i]])), .) %>%
          unlist()
        result[[i]] <- col
        }
      }
  result
}
