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

#' FV with time varying interest rates
#' 
#' calculates FV from cash flow and a total return index
#' @param CF is the cash flow zoo object
#' @param TR is the total return time series
#' @param ValDate is the valuation date defaults to last date in time series
#' @keywords present_value
#' @export
#' @examples 
#' cf=zoo(c(1,2),as.Date("2018-1-1")+c(0,30))
#' tr=zoo(c(101,102),as.Date("2018-1-1")+c(0,30))
#' fv.tr(cf,tr)
fv.tr=function(CF,TR,ValDate=NULL) {  
  #
  # DESCRIPTION:
  # a function that calculates future value with time varying interest rates
  # 
  # ARGUMENTS:
  # CF -- a zoo object of cash flows
  # TR -- a zoo object which is a total return index reflecting the variable interest rate, 
  #       there must be an entry in TR for every date in CF and for the ValDate
  # ValDate -- the valuation date or dates, defaults to last date in cash flow if none provided,
  #             if earlier than last date in cash flows, then cash flows after this date are ignored,
  #             if earlier than first date in cash flows, returns NA
  #             defaults to the last date in the cash flow,
  #             must be in the same date format as CF and TR
  #
  # RESULT:
  # a zoo object of the same length as ValDate which is the value of the cash flows as if invested in the index
  # as of each valuation date
  #
  if(is.null(ValDate)) ValDate=tail(time(CF),1)
  if(!is.zoo(CF)) stop("CF not a zoo object")
  if(!is.zoo(TR)) stop("TR not a zoo object")
  if(any(!(ValDate%in%time(TR)))) stop("No TR for ValDate")
  if(any(!(time(CF)%in%time(TR)))) stop("No TR for CF")
  value=vector()
  for (i in 1:length(ValDate)) {
    if(ValDate[i]<min(time(CF))) value[i]=NA
    CF.i=CF[time(CF)<=ValDate[i]]
    TR.i=TR[time(TR)<=ValDate[i]]
    TR.ind=(as.numeric(tail(TR.i,1)))/TR.i
    value[i]=-sum(TR.ind[time(CF.i)]*CF.i)
  }
  return(zoo(value,ValDate))
}

