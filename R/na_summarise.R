#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
na_summarise <- function(data) {
  result <- list(Mean = mean(data, na.rm = TRUE),
                 Median = median(data, na.rm = TRUE),
                 Std.dev = sd(data, na.rm = TRUE),
                 Coeff.var = sd(data, na.rm = TRUE)/mean(data, na.rm = TRUE)*100,
                 Min = min(data, na.rm = TRUE),
                 Max = max(data, na.rm = TRUE),
                 Range = max(data, na.rm = TRUE) - min(data, na.rm = TRUE),
                 N.valid.val = length(na.omit(data)),
                 N.na.val = sum(is.na(data)),
                 Prcnt.na = sum(is.na(data))/length(data)*100)
}
