#' IRR gap
#'
#' How much more (or less) terminal value to achieve target IRR
#' @param i is the target IRR
#' @param cf is the cash flow, a zoo object with date (i.e. daily) class time
#' @param end is the valuation date, defaults to last day in cash flow
#' @keywords IRR
#' @export
#' @examples
#' irrgap.z(.1,zoo(c(-1,1.05),as.Date("2018-1-1")+c(0,365)))
irrgap.z=function(i,cf.z,end=NA) {
  freq=365
  if(!is.zoo(cf.z)) {warning("cash flow must be zoo object"); return(NA)}
  if("Date"!=class(time(cf.z))) {warning("need Date class for zoo index"); return(NA)}
  i=freq*(((1+i)^(1/freq))-1)
  if(!is.na(end)) cf.z=mergesum.z(cf.z,zoo(0,end))
  tdif=as.numeric(index(cf.z)-(index(cf.z)[1]))
  d=(1+(i/freq))^tdif
  v=sum(cf.z/d)
  -v*tail(d,1)
}
#' IRR
#'
#' Calculate an IRR from a zoo object with GIPS compliance option
#' @param cf is the cash flow, a zoo object with date (i.e. daily) class time
#' @param gips calculations is GIPS compliant if TRUE
#' @keywords IRR
#' @export
#' @examples
#' irr.z(zoo(c(-1,1.05),as.Date("2018-1-1")+c(0,365)))
irr.z=function(cf.z,gips=FALSE) {
  irr.freq=1
  if(any(is.na(cf.z))) return(NA)
  if(sum(cf.z)==0) return (0)
  if(length(cf.z)<=1) return(NA)
  if(all(cf.z<=0)) return(NA)
  if(all(cf.z>=0)) return(NA)
  if(!is.zoo(cf.z)) {
    timediff=-1+1:length(cf.z)
  } else {
    timeline=time(cf.z)
    timediff=as.numeric(timeline-timeline[1])
    if ("Date"== class(timeline)) irr.freq=365
  }
  failirr=FALSE
  samesign=TRUE
  if (sum(cf.z)<0) {
    rangehi=.01
    rangelo=0
    i=0
    # low range on search for negative IRR is -100%
    while(i<1002&samesign&!failirr) {
      rangehi=rangelo
      rangelo=max(-10,rangelo-.01)
      hinpv=npv.znoadjust(rangehi,cf.z,irr.freq,timediff)
      lownpv=npv.znoadjust(rangelo,cf.z,irr.freq,timediff)
      samesign=sign(hinpv)==sign(lownpv)
      failirr=(is.nan(hinpv)|is.nan(lownpv)|is.na(samesign))
      i=i+1
    }} else {
      rangehi=0
      rangelo=-.01
      i=0
      # while hi range on search for positive IRR is 100,000%
      while(i<100000&samesign&!failirr) {
        rangelo=rangehi
        rangehi=rangehi+.01
        hinpv=npv.znoadjust(rangehi,cf.z,irr.freq,timediff)
        lownpv=npv.znoadjust(rangelo,cf.z,irr.freq,timediff)
        samesign=sign(hinpv)==sign(lownpv)
        failirr=(is.nan(hinpv)|is.nan(lownpv)|is.na(samesign))
        i=i+1
      }}
  if(failirr) return(NA)
  npv1=lownpv
  npv2=hinpv
  if (sign(npv1)==sign(npv2)) return(NA)
  cf.n=as.numeric(cf.z)
  #calculate with uniroot if cash flow starts negative and ends positive otherwise do your own search
  if((cf.n[1]<0)&(cf.n[length(cf.n)]>0)) {
    ans=uniroot(npv.znoadjust,c(rangelo,rangehi),cf=cf.z,freq=irr.freq,tdiff=timediff)
    apr=ans$root } else {
      int1=rangelo
      int2=rangehi
      for (i in 1:40) {
        inta=mean(c(int1,int2))
        npva=npv.znoadjust(inta,cf.z,irr.freq,timediff)
        if(sign(npva)==sign(npv1)) {
          int1=inta
          npv1=npva
        } else {
          int2=inta
          npv2=npva
        }}
      apr=mean(int1,int2)
    }
  # convert IRR to compounding at irr.freq interval
  ans=((1+(apr/irr.freq))^irr.freq)-1
  # convert IRR to GIPS compliant if requested
  if (gips) {
    if(cf.z[1]==0)  cf.z=cf.z[-1]
    dur=index(cf.z)[length(cf.z)]-index(cf.z)[1]
    if(dur<irr.freq) ans=(1+ans)^((as.numeric(dur))/irr.freq)-1
  }
  return (ans)
}

#' NPV for irr.z function
#'
#' a concise NPV function used by irr.z (everything extra stripped out since function called a bazillion times)
#' @param i is the annual interest rate
#' @param cf.z is a zoo object cash flow
#' @param freq is the compounding period (e.g. 365 for daily, 12 for monthly)
#' @param tdiff is the previously calculated time difference across cf.z
npv.znoadjust=function(i,cf.z,freq,tdiff) {
  d=(1+(i/freq))^tdiff
  sum(cf.z/d)
}
