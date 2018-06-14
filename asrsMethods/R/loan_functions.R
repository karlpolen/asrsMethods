#' Loan balance
#' 
#' This function returns loan balance after the nth payment
#' @param bal is the initial loan balance
#' @param i is the interest rate
#' @param pmt is the periodic payment
#' @param n is the period for evaluation (can be a vector)
#' @keywords loan
#' @export
#' @examples 
#' loanbal(1000,.1,106.08,c(9,10))
loanbal=function(bal,i,pmt,n) {
  (bal*((1+i)^n))-((pmt/i)*(((1+i)^n)-1))
}
#' Loan payment
#' 
#' Returns level payment amount given initial balance, interest rate and number of payments
#' @param bal is initial balance
#' @param i is interest rate
#' @param n is number of periods
#' @keywords loan
#' @export
#' @examples 
#' lpmt(1000,.1,30)
lpmt=function(bal,i,n) {
  (i*bal)/(1-((1+i)^-n))
}
#' Interest portion
#' 
#' Returns the interest portion of the nth payment
#' @param bal is the initial balance
#' @param i is the interest rate
#' @param pmt is the period payment
#' @param n is the period for evaluation (can be a vector)
#' @keywords loan
#' @export
#' @examples 
#' intportion(1000,.1,106.08,c(9,10))
intportion=function(bal,i,pmt,n) {
  i*loanbal(bal,i,pmt,n-1)
}
#' Principal portion
#' 
#' Returns the principal portion of the nth payment
#' @param bal is the initial balance
#' @param i is the interest rate
#' @param pmt is the period payment
#' @param n is the period for evaluation(can be a vector)
#' @keywords loan
#' @export
#' @examples 
#' intportion(1000,.1,106.08,c(9,10))
prinportion=function(bal,i,pmt,n) {
  pmt-intportion(bal,i,pmt,n)
}
#' Loan amortization function
#' 
#' Returns value for the missing parameter and full amortization schedule for the loan as zooreg objects
#' @param r is the interest rate
#' @param n is the nuber of periods
#' @param pmt is the periodic payment
#' @param bal0 is the balance at time 0 (can be a zooreg object)
#' @param freq is the frequency of payments - 1 means the frequency is the same as the stated interest rate
#' @param apr if TRUE means the conversion of rate in r to frequency is done by arithmetic division, otherwise geometric
#' @param start is optional starting date
#' @keywords loan
#' @export
#' @examples 
#' loanamort(r=.1, bal0=1000, pmt=NULL, n=30, start=2018, freq=12)
loanamort=function(r=NULL,bal0=NULL,pmt=NULL,n=NULL,apr=FALSE,start=NULL,freq=1) {
  ans=list()
  risnull=is.null(r)
  bal0isnull=is.null(bal0)
  if(!bal0isnull) {
    if(is.zoo(bal0)) start=time(bal0)
    bal0=as.numeric(bal0)
  }
  pmtisnull=is.null(pmt)
  nisnull=is.null(n)
  if(1<sum(c(risnull,bal0isnull,pmtisnull,nisnull))) stop('loanamort error -- need to provide at least three parameters')
  n.f=n
  if(apr) n.f=n*freq
  if(!risnull) {
    if(apr) {
      r.f=r/freq
    } else {
      r.f=-1+(1+r)^(1/freq)
    }
  } else {
    cf=c(-bal0,rep(pmt,n.f))
    if(0<=sum(cf)) {
      rootrange=c(0,1.01) } else {
        rootrange=c(1,1000)
      }
    d=(uniroot(function(d) {sum(cf*d^(0:n.f))},rootrange))$root
    r.f=(1/d)-1
  }
  d=1/(1+r.f)
  f=1+r.f
  if(pmtisnull) pmt=(bal0*r.f)/(1-d^n.f)
  perp=pmt/r.f
  if(bal0isnull) bal0=perp-perp*(d^n)
  if(pmt<=(r.f*bal0)) stop(paste(pmt,r.f*bal0,'payment must be greater than interest'))
  if(nisnull) n.f= ceiling(log((1-(bal0*r.f)/pmt))/log(d))
  i=1:n.f
  bal=pmax(0,((bal0*f^i)-(((pmt*f^i)-pmt)/r.f)))
  balall=c(bal0,bal)
  int=balall[i]*r.f
  prin=-diff(balall)
  if(!is.null(start)) {
    bal=zooreg(bal,start=start+1/freq,freq=freq)
    int=zooreg(int,start=start+1/freq,freq=freq)
    prin=zooreg(prin,start=start+1/freq,freq=freq)
  }
  if(apr) {
    ans$r=r.f*freq
    ans$n=n.f/freq
  } else {
    ans$r=-1+((1+r.f)^freq)
    ans$n=n.f
  }
  ans$pmt=pmt
  ans$bal0=bal0
  ans$freq=freq
  ans$start=start
  ans$apr=apr
  ans$bal=bal
  ans$prin=prin
  ans$int=int
  return(ans)
}
