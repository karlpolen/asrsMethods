#' Future Value Function
#' 
#' Calculates future value at end of cash flow or date "now"
#'
#' @param i is the periodic interest rate
#' @param cf is the cash flow, a zoo object or vector
#' @param apr if TRUE means the periodic/annual interest rate will be adjusted to the payment periodicity by division, otherwise geometric
#' @param now is the date for valuation
#' @param drop.after.now if TRUE drops cash flows after now
#' @param adjdisc is there for a forgotten reason
#' @export
#' @examples 
#' fv.z(.1,zoo(1:10,11:20))
fv.z=function(i,cf,freq=1,apr=FALSE,now=NULL,drop.after.now=TRUE,adjdisc=NULL) {
  if(!is.null(adjdisc)) apr=!adjdisc
  if(!is.zoo(cf)) {
    timeline=(1:length(cf))/freq
  } else {
    timeline=time(cf)
  }
  iszooreg=("Date"!=class(timeline))
  if(!iszooreg) {
    freq=365
  } else if ("yearmon" == class(timeline)) {
    freq=12
  } else if ("yearqtr" == class(timeline)) {
    freq=4
  }
  if (iszooreg&apr) {
    i=-1+(1+i/freq)^freq
  } else if (iszooreg&(!apr)) {
    i=i
  } else if ((!iszooreg)&apr) {
    i=i/freq
  } else if((!iszooreg)&(!apr)) {
    i=-1+((1+i)^(1/freq))
  }
  if (is.null(now)) now=timeline[length(timeline)] 
  if (drop.after.now) {
    cf=cf[timeline<=now]
    timeline=timeline[timeline<=now]
  }
  ind=as.numeric(now-timeline)
  d=(1+i)^ind
  sum(cf*d)
}

#' A NPV function
#' 
#' calculates NPV of cash flow submitted as a zoo object
#' @param i is the interest rate
#' @param cf is the cash flow, a zoo object may be zooreg or daily cash flows
#' @param freq is the frequency of payments per period for the stated interest rate
#' @param apr is converted frequency by division if apr is TRUE, geometrically otherwise
#' @param now is the date for valuation, otherwise valued at beginning of cash flow
#' @param drop.bef.now drops cash flows before now if TRUE
#' @param adjdisc interacts with apr, forget why it is here
#' @export
#' @examples 
#' npv.z(.1,zooreg(1:10,11:20))
npv.z=function(i,cf,freq=1,apr=FALSE,now=NULL,drop.bef.now=TRUE,adjdisc=NULL) {
  if(!is.null(adjdisc)) apr=!adjdisc
  if(!is.zoo(cf)) {
    timeline=(1:length(cf))/freq
  } else {
    timeline=time(cf)
  }
  iszooreg=("Date"!=class(timeline))
  if(!iszooreg) {
    freq=365
  } else if ("yearmon" == class(timeline)) {
    freq=12
  } else if ("yearqtr" == class(timeline)) {
    freq=4
  }
  if (iszooreg&apr) {
    i=-1+(1+i/freq)^freq
  } else if (iszooreg&(!apr)) {
    i=i
  } else if ((!iszooreg)&apr) {
    i=i/freq
  } else if((!iszooreg)&(!apr)) {
    i=-1+((1+i)^(1/freq))
  }
  if (is.null(now)) now=timeline[1] 
  if (drop.bef.now) {
    cf=cf[timeline>=now]
    timeline=timeline[timeline>=now]
  }
  ind=as.numeric(timeline-now)
  d=(1+i)^ind
  sum(cf/d)
}
