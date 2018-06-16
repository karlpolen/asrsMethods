#' incentive fee reverse engineer
#'
#' reverse engineer incentive fee from known performance data
#' @param nav is a time series of navs
#' @param cf is a time series of cash flows
#' @param cu is the catchup parameter
#' @param pref is the preferred return defaults to 8
#' @param cry is the carry defaults to 20
#' @param fre is the compounding period defaults to quarterly
#' @keywords private_equity
#' @export
#' @examples
#' cf=zoo(c(-100,-100),as.Date("2018-1-1")+c(1,90))
#' nav=zoo(c(300)as.Date("2018-6-30"))
#' incentfee.rev=function(nav,cf,1)
incentfee.rev=function(nav,cf,cu,pref=.08,cry=.2,fre=4) {
  #
  # reverse engineer an incentive fee from following info
  # nav-- a one element time series of the investor net asset value
  # cf -- a time series of calls (negative) and distributions (positive),
  #       values after date of nav are ignored
  # cu -- the catchup percent as a decimal
  # pref -- the preferred return rate as a decimal
  # cry -- the carry rate as a decimal
  # fre -- the periodicity of calculation, 4 for quarterly (the default and industry convention)
  #
  # returns a zoo data.frame object which is the incentive fee earned cumulatively through date of nav
  # only works for a one level structure with catchup
  # calculated on accrual and cash basis, both with european waterfall style calculations
  #
  # data validation
  if (!is.zoo(cf)) stop ("cf must be a zoo object")
  if (!is.zoo(nav)) stop ("nav must be a zoo object")
  if (cu<0|cu>1) stop ("cu must be 0<=cu<=1")
  if (pref<0|pref>1) stop ("pref must be 0<=pref<=1")
  if (cry<0|cry>1) stop ("cry must be 0<=cry<=1")
  if (cu!=0&cu<=cry) stop ("cu must be zero or > cry")
  if (!fre%in%c(1,4,12)) stop("freq must be 1, 4 or 12")
  cf=cf[time(cf)<=time(nav)]
  if(length(cf)==0) {
    warning ("no cash flows on or before nav date")
    return(NULL)
  }
  if(!any(cf<0)) {
    warning("no cash flows less than zero")
    return(NULL)
  }
  #calculate fee accrual basis
  #calculate preferred return dollars
  x=pfd.return(mergesum.z(cf,nav),int=pref,freq=fre,mdate=time(nav))
  prefearned=sum(x$pref.paid)+lastinvec(x$acc.pref)
  profitpaid=max(0,sum(x$pref.paid)+sum(x$residual)-lastinvec(x$unreturned))
  #calculate the incentive fee
  excess1=max(0,profitpaid-prefearned)
  #catch up layer
  lpsharecu=gpsharecu=0
  if(cu>0&excess1>0) {
    cuagg=prefearned*cry/(cu-cry)
    lpsharecu=(1-cu)*cuagg
    gpsharecu=cu*cuagg
    if(excess1<lpsharecu) {
      cumult=lpsharecu/excess1
      lpsharecu=lpsharecu*cumult
      gpsharecu=gpsharecu*cumult
    }
  }
  #parri passu layer
  excess2=excess1-lpsharecu
  incentfee=gpsharecu+(excess2*(-1+1/(1-cry)))

  #calculate fee paid w european waterfall
  x2=pfd.return(cf,int=pref,freq=fre,mdate=time(nav))
  prefearned=sum(x2$pref.paid)+lastinvec(x2$acc.pref)
  profitpaid=max(0,sum(x2$pref.paid)+sum(x2$residual)-lastinvec(x2$unreturned))
  #calculate the incentive fee
  excess1=max(0,profitpaid-prefearned)
  #catch up layer
  lpsharecu=gpsharecu=0
  if(cu>0&excess1>0) {
    cuagg=prefearned*cry/(cu-cry)
    lpsharecu=(1-cu)*cuagg
    gpsharecu=cu*cuagg
    if(excess1<lpsharecu) {
      cumult=lpsharecu/excess1
      lpsharecu=lpsharecu*cumult
      gpsharecu=gpsharecu*cumult
    }
  }
  #parri passu layer
  excess2=excess1-lpsharecu
  incentfee2=gpsharecu+(excess2*(-1+1/(1-cry)))

  #return answer
  return (data.frame(Accrual=zoo(incentfee,time(nav)),Cash=zoo(incentfee2,time(nav))))
}

#' Preferred return
#'
#' calculates preferred return from cash flow
#' @param cf is a time series cash flow
#' @param int is the preferred return rate
#' @param freq is the compouding period defaults to quarterly
#' @param mdate is the measurement date
#' @keywords private_equity
#' @export
#' @examples
#' cf=zoo(c(-100,-100),as.Date("2018-1-1")+c(1,90))
#' pfd.return(cf,.08)
pfd.return=function(cf,int,freq=4,mdate=NA) {
  require(zoo)
  require(lubridate)
  #check validity of compounding period
  if(!freq%in%c(1,4,12)) stop('freq must be annual(1), quarterly(4) or monthly(12)')
  #check that cf is a zoo object
  if(!is.zoo(cf)) stop('cash flow must be zoo object')
  #ignore cash flow elements after the measurement date
  if(!is.na(mdate)) {
    cf=cf[time(cf)<=mdate]
    cf=zoosum(cf,zoo(0,mdate))
  }
  tl=time(cf)
  if("Date"!=class(tl)) {
    tl=as.Date(tl)
    warning ('time index coerced to Date class')
  }
  if(freq==1) {
    time.ld=seq(year(tl[1]),year(lastinvec(tl)))
    time.ld=-1+as.Date(1+yearmon(time.ld))
    report.ld=.25+seq(as.yearqtr(tl[1]),as.yearqtr(lastinvec(tl)),by=.25)
    while(1!=quarter(lastinvec(report.ld))) {
      report.ld=c(report.ld,.25+lastinvec(report.ld))
    }
    report.ld=-1+as.Date(report.ld)
    if(is.na(mdate)) mdate=lastinvec(time.ld)
    if(lastinvec(time.ld)>mdate) {
      time.ld[length(time.ld)]=mdate
      report.ld=report.ld[report.ld<mdate]
      if(mdate!=lastinvec(report.ld)) report.ld=c(report.ld,mdate)
    }
  }
  if(freq==4) {
    time.ld=.25+seq(as.yearqtr(tl[1]),as.yearqtr(lastinvec(tl)),by=.25)
    time.ld=-1+as.Date(time.ld)
    if(is.na(mdate)) mdate=lastinvec(time.ld)
    if(lastinvec(time.ld)>mdate) {
      time.ld[length(time.ld)]=mdate
    }
    report.ld=time.ld
  }
  if(freq==12) {
    nmon=1+round(12*(as.yearmon(lastinvec(tl))-as.yearmon(tl[1])))
    time.ld=-1+as.Date(as.yearmon((tl[1])+months(1:nmon)))
    if(is.na(mdate)) mdate=lastinvec(time.ld)
    if(lastinvec(time.ld)>mdate) {
      time.ld[length(time.ld)]=mdate
    }
    report.ld=time.ld
  }
  acc.pref=current.pref=unreturned=zoo(0,report.ld)
  pref.paid=cap.paid=residual=0*cf
  time.comb=time(zoosum(cf,unreturned))
  unreturned.bal=current.bal=acc.bal=0
  n=length(time.comb)
  for (i in 1:n) {
    now=time.comb[i]
    ndays=0
    if(i!=1) ndays=as.numeric(time.comb[i]-time.comb[i-1])
    current.bal=current.bal+(int*ndays/365)*(acc.bal+unreturned.bal)
    if (now%in%tl) {
      cf.now=as.numeric(cf[now])
      if(cf.now>0) {
        split=waterfall(c(current.bal,acc.bal,unreturned.bal),cf.now)
        current.bal=current.bal-split[1]
        acc.bal=acc.bal-split[2]
        unreturned.bal=unreturned.bal-split[3]
        pref.paid[time.comb[i]]=split[1]+split[2]
        cap.paid[time.comb[i]]=split[3]
        residual[time.comb[i]]=split[4]
      } else {
        unreturned.bal=unreturned.bal-cf.now
      }
    }
    if(now%in%report.ld) {
      unreturned[now]=unreturned.bal
      acc.pref[now]=acc.bal
      current.pref[now]=current.bal
    }
    if(now%in%time.ld) {
      acc.bal=acc.bal+current.bal
      acc.pref[now]=acc.bal
      current.pref[now]=0
      current.bal=0
    }
  }
  ans=list()
  ans$current.pref=current.pref
  ans$acc.pref=acc.pref
  ans$unreturned=unreturned
  ans$pref.paid=pref.paid
  ans$cap.paid=cap.paid
  ans$residual=residual
  ans$cf=cf
  return(ans)
}

#' general partner compensation
#' 
#' a function to evaluate gp compensation for one or more returns
#' @param dmat a data frame describing the deal structure with columns for asset management fee, pref, catchup and carry
#' @param ret one or more returns as a vector
#' @param capital amount contributed
#' @param invcost cost of investments (may be different from cost because of asset management fee)
#' @keywords private_equity
#' @export
#' @examples 
#' peinv=100
#' pecap=102
#' dmat.pe=data.frame(am=0,pref=(.08*pecap),catchup=.5,carry=.2)
#' ans.pe=gpcomp(dmat.pe,ret=seq(100,130,.1),invcost=peinv,capital=pecap)
gpcomp = function(dmat, ret, capital = 100, invcost = 100) {
  am = dmat$am
  pref = dmat$pref
  catchup = dmat$catchup
  carry = dmat$carry
  if (any(1 < c(catchup, carry))) 
    stop("catchup and carry must be stated as decimals<1")
  pref = c(pref, 1e+16)
  am = c(am, 0)
  stack = vector()
  lpcut = vector()
  typ = vector()
  nlayer = nrow(dmat)
  if (am[1] > 0) {
    stack = c(stack, am[1])
    lpcut = c(lpcut, 0)
    typ = c(typ, paste("Asset mgmt", 0))
  }
  if (capital > 0) {
    stack = c(stack, capital)
    lpcut = c(lpcut, 1)
    typ = c(typ, paste("Return of Capital"))
  }
  if (pref[1] > 0) {
    stack = c(stack, pref[1])
    lpcut = c(lpcut, 1)
    typ = c(typ, paste("Preferred Return", 1))
  }
  for (j in 1:nlayer) {
    if (am[j + 1] > 0) {
      stack = c(stack, am[j + 1])
      lpcut = c(lpcut, 0)
      typ = c(typ, paste("Asset Mgmt", j))
    }
    nextpref = pref[j + 1]
    lpsofar = sum(stack * lpcut) - capital
    lpshort = nextpref - lpsofar
    cu = catchup[j]
    cy = carry[j]
    catchuplayer = 0
    if (cu > cy) {
      catchuplayer = (lpsofar * cy)/(cu - cy)
      if (cu < 1) 
        catchuplayer = min(catchuplayer, lpshort/(1 - cu))
      stack = c(stack, catchuplayer)
      lpcut = c(lpcut, (1 - cu))
      typ = c(typ, paste("Catchup", j))
    }
    lpsofar = sum(stack * lpcut) - capital
    lpshort = nextpref - lpsofar
    carrylayer = lpshort/(1 - cy)
    if (carrylayer > 0) {
      stack = c(stack, carrylayer)
      lpcut = c(lpcut, (1 - cy))
      typ = c(typ, paste("Carry", j))
    }
  }
  ansmat = matrix(0, nrow = length(stack), ncol = length(ret))
  for (i in 1:length(ret)) {
    ansmat[, i] = wf(stack, ret[i])[-(1 + length(stack))]
  }
  ans = list()
  ans$lpshare = matrix(lpcut, nrow = length(stack), ncol = length(ret)) * 
    ansmat
  rownames(ans$lpshare) = typ
  ans$gpshare = ansmat - ans$lpshare
  rownames(ans$gpshare) = typ
  ans$grossreturn = 100 * (ret - invcost)/invcost
  ans$netreturn = 100 * (colSums(ans$lpshare) - capital)/capital
  ans$stack = stack
  ans$lpcut = lpcut
  return(ans)
}


#' waterfall
#' 
#' given waterfall w in dollars and available cash c, distribute the cash to the waterfall
#' @param w the waterfall as vector
#' @param c the amount of cash to distribute
#' @keywords private_equity
#' @export
#' @examples 
#' wf(c(1,2,3),5)
wf = function(w, c) {
  x = c - cumsum(w)
  x[x > 0] = 0
  x = x + w
  x[x < 0] = 0
  c(x, c - sum(x))
}

#' test gpcomp
#' 
#' a convenience function to test the result of gpcomp
#' @parm ans the result of a call to gpcomp
#' @keywords private_equity
#' @export
#' @examples  
#' testans(ans)
testans = function(ans) {
  (colSums(ans$lpshare)) + (colSums(ans$gpshare))
}

#' waterfall
#' 
#' given waterfall w in dollars and available cash c, distribute the cash to the waterfall
#' @param w the waterfall as vector
#' @param c the amount of cash to distribute
#' @keywords private_equity
#' @export
#' @examples 
#' wf(c(1,2,3),5)
waterfall=function(w,c){
  wf(w,c)
}