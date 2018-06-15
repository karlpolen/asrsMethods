#' tvpi
#'
#' calcultes TVPI (total value as percent of paid in)
#' @param cf is the cash flow
#' @keywords Performance
#' @export
#' @examples
#' tvpi(c(-1,1.1))
tvpi <- function(cf) {
  if(any(is.na(cf))) return(NA)
  if(0==sum(cf[cf<0])) {return(NA)}
  -sum(cf[cf>0])/sum(cf[cf<0])
}
#' payback
#'
#' payback period (how long before you get your money back)
#' @param cf is the cash flow with even periodicity
#' @keywords Performance
#' @export
#' @examples
#' tvpi(c(-1,1.1))

payback <- function(cf) {
  cumcf=cumsum(cf)
  cumcfneg=rev(cumcf<0)
  pb=sum(cummax(cumcfneg))
  ans=pb
  if (pb==length(cf)) {ans=NA}
  ans
}
