#' Title
#'
#' @param data Description
#'
#' @return Returns x
#'
#' @examples
#' # example
#'
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
