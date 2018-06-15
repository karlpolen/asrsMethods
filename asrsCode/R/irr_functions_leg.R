
#' IRR
#'
#' A polyroot based IRR function (included for curiosity, don't use, not reliable with real data)
#' @param cf are evenyly spaced cash flows delivered as a vector
#' @param rejectlm1 rejects values less than -100% if TRUE (imaginary values automatically rejected)
#' @param frequ is the periodicity of the cash flow (1 for annual, 12 for monthly, etc)
#' @keywords IRR
#' @export
#' @examples
#' irr(c(-1,1.1))
irr <- function(cf,rejectlm1=TRUE,frequ=1) {
  res=polyroot(cf)
  res=Re(res[abs(Im(res))< 1e-6])
  res=1/res-1
  if(rejectlm1) res=res[res>=-1]
  if(0==length(res)){
    res=NA
  } else {
    res=sort(res)
    res=((1+res)^frequ)-1
  }
  return(res)
}

#' IRR daily data
#'
#' calculates an IRR using the irr function with sparse daily data
#' @param cf.z is the daily cash flow zoo object
#' @keywords IRR
#' @export
#' @examples
#' irr.z365(zoo(c(-1,1.1),as.Date("2018-1-1")+c(0,180)))
irr.z365=function(cf.z) {
  beg=min(index(cf.z))
  end=max(index(cf.z))
  cf.i=zooreg(rep(0,as.integer(1+end-beg)),start=beg,end=end)
  cf.mat=merge(cf.z,cf.i)
  cf=as.numeric(natozero(cf.mat$cf.z))
  res=irr(cf,frequ=365)
  return(res)
}
