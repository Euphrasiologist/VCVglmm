#' Specify the number of decimal places
#' Please see (https://stackoverflow.com/questions/3443687/formatting-decimal-places-in-r)
#' for the function conceptions
#' 
#' @param x decimal to convert
#' @param k number of decimal places
#' @keywords decimal places, decimal
#' @export
#' @examples
#' x <- 0.324153245
#' specify_decimal(x, 3)

specify_decimal <- function(x, k){
  format <- trimws(format(round(x, k), nsmall=k))
  format <- as.numeric(format)
  format
}