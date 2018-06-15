#' last elecment in vector
#' 
#' convenience function to grab the last element in a vector
#' @param x is the vector
#' @param naremove changes trailing NA to zero if set to true
#' @keywords convenience
#' @export
#' @examples 
#' lastinvec(1:3)
lastinvec=function(x,naremove=FALSE) {
  ans=tail(x,1)
  if(length(ans)> 0){
    if(naremove & is.na(ans)) ans=0
  }
  return(ans)
}
#' first in vector
#' 
#' convenience function to return first element in a vector
#' @param x is the vector
#' @keywords convenience
#' @export
#' @examples 
#' firstinvec(1:3)
firstinvec=function(x) {x[1]}
