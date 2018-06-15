#' na to zero
#'
#' convenience function converts na's to zero in a vector
#' @param x is a vector
#' @keywords Convenience
#' @export
#' @examples
#' natozero(c(1,NA))
natozero=function(x) {
  x[is.na(x)]=0
  x
}
