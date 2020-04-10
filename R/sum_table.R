#' Summarise data table
#'
#' @description \code{sum_table} generates a summary table (a tibble) of
#' descriptive statistics for each column of the input data table,
#' which can be a matrix or a data.frame.
#'
#' @param data data.frame or matrix: must only contain numeric data.
#'
#' @return Returns a tibble with the following columns:
#' \itemize{
#' \item \code{label}, name of the column being summarised
#' \item \code{mean}
#' \item \code{median}
#' \item \code{std.dev}, standard deviation
#' \item \code{coeff.var}, coefficient of variation
#' \item \code{min}
#' \item \code{max}
#' \item \code{range}
#' \item \code{n.valid.val}, number of non-\code{NA} values
#' \item \code{n.na.val}, number of \code{NA} values
#' \item \code{prcnt.na}, percentage of total values that are \code{NA}
#' }
#'
#' @examples
#' # make a matrix
#' my_mat <- matrix(c(2, 7, NA, NA,
#'                    3, 6, NA, NA,
#'                    1, NA, 8, NA,
#'                    432, 564, 789, 555),
#'                    nrow = 4,
#'                    ncol = 4)
#' colnames(my_mat) <- c("sample....1",
#'                       "sample..2",
#'                       "sample...3",
#'                       "total")
#'
#' # summarise the matrix
#' sum_table(my_mat)
#'
#' @export
sum_table <- function(data) {
  # ensure data is data.frame or matrix
  assertthat::assert_that(is.matrix(data) | is.data.frame(data))

  # define custom summary function
  summarise_na <- function(data) {
    result <- list(mean = mean(data, na.rm = TRUE),
                   median = stats::median(data, na.rm = TRUE),
                   std.dev = stats::sd(data, na.rm = TRUE),
                   coeff.var = stats::sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE)*100,
                   min = min(data, na.rm = TRUE),
                   max = max(data, na.rm = TRUE),
                   range = max(data, na.rm = TRUE) - min(data, na.rm = TRUE),
                   n.valid.val = length(stats::na.omit(data)),
                   n.na.val = sum(is.na(data)),
                   prcnt.na = sum(is.na(data))/length(data)*100)
  }

  # convert data to tibble and apply summarise_na to each column
  sum_list <- data %>%
    tibble::as_tibble() %>%
    lapply(., summarise_na)

  # bind the summary for each column together in a tibble
  result <- tibble::as_tibble(do.call(rbind, sum_list), rownames = "label")

  # unnest the tibble columns
  tidyr::unnest(result, cols = c(mean, median, std.dev, coeff.var, min, max,
                                 range, n.valid.val, n.na.val, prcnt.na))
}
