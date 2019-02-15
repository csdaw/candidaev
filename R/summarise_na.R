#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
summarise_na <- function(data) {
  result <- list(mean = mean(data, na.rm = TRUE),
                 median = median(data, na.rm = TRUE),
                 std.dev = sd(data, na.rm = TRUE),
                 coeff.var = sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE)*100,
                 min = min(data, na.rm = TRUE),
                 max = max(data, na.rm = TRUE),
                 range = max(data, na.rm = TRUE) - min(data, na.rm = TRUE),
                 n.valid.val = length(na.omit(data)),
                 n.na.val = sum(is.na(data)),
                 prcnt.na = sum(is.na(data))/length(data)*100)
}
