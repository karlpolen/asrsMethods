#' pe performance
#'
#' calculate pe performance metrics
#' @param cf is cashflow zoo object
#' @param ind is a total return index zoo object
#' @keywords private_equity
#' @export
#' @examples
#' dates=as.Date("2018-1-1")+c(1,31,61)
#' cf=zoo(c(-100,5,110),dates)
#' ind=zoo(c(100,101,102),dates)
#' pestats(cf,ind)
pestats=function(cf,ind)  {
  ans=list()
  ans$tvpi=NA
  ans$irr=NA
  ans$pme=NA
  ans$pme.plus=NA
  ans$alpha=NA
  ans$ind.irr=NA
  ans$pme.wealth=NA
  ans$sb=NA
  ans$ss=NA
  if(length(cf)<=1)  return(ans)
  if(all(cf<=0)) return(ans)
  if(all(cf>=0)) return(ans)
  ans$tvpi=tvpi(cf)
  ans$irr=irr.z(cf,gips=TRUE)
  cf.neg=cf.pos=cf
  cf.pos[cf.pos<0]=0
  cf.neg[cf.neg>0]=0
  if(all(!(is.na(ind)))) {
    fvfactor=as.numeric(lastinvec(ind))/ind
    ans$pme=-sum(cf.pos*fvfactor)/sum(cf.neg*fvfactor)
    cf.fv=cf*fvfactor
    ans$alpha=log(1+irr.z(cf.fv,gips=TRUE))
    logpe.irr=log(1+ans$irr)
    logdm.irr=logpe.irr-ans$alpha
    ans$ind.irr=-1+exp(logdm.irr)
    ans$pme.wealthdiff=sum(cf.fv)
    sb=-cf.neg/ind
    if(length(which(cf.pos>0))>1) {
      sharesterm=as.numeric(lastinvec(cf.pos)/lastinvec(ind))
      stosell=sum(sb)-sharesterm
      cf.posxlast=cf.pos
      cf.posxlast[length(cf.pos)]=0
      scale=stosell/sum(cf.posxlast/ind)
      cf.pos.scaled=cf.pos*scale
      cf.pos.scaled[length(cf.pos)]=cf.pos[length(cf.pos)]
      ans$pme.plus=irr.z(mergesum.z(cf.pos.scaled,cf.neg),gips=TRUE) } else {
        cf.pos.scaled=cf.pos
        cf.pos.scaled[length(cf.pos)]=(sum(sb))*ind[length(ind)]
        ans$pme.plus=irr.z(mergesum.z(cf.pos.scaled,cf.neg),gips=TRUE)
      }

    #   lndelt=-sum(fvfactor*cf)
    #   cf.ln=cf
    #   cf.ln[length(cf.ln)]=lndelt+cf.ln[length(cf.ln)]
    #   ans$pme.ln=irr.z(cf.ln,gips=TRUE)
    ans$sb=sb
    ans$ss=cf.pos.scaled/ind
  }

  return(ans)
}

#' pe irr
#'
#' calculate irr for pe fund and related index
#' @param cf is cashflow zoo object
#' @param ind is a total return index zoo object
#' @keywords private_equity
#' @export
#' @examples
#' dates=as.Date("2018-1-1")+c(1,31,61)
#' cf=zoo(c(-100,5,110),dates)
#' ind=zoo(c(100,101,102),dates)
#' pe_irr(cf,ind)
pe_irr=function(cf,ind)  {
  ans=c(NA,NA)
  names(ans)=c("FundIRR","IndexIRR")
  if(length(cf)<=1)  return(ans)
  if(all(cf<=0)) return(ans)
  if(all(cf>=0)) return(ans)
  ans[1]=irr.z(cf,gips=TRUE)
  cf.neg=cf.pos=cf
  cf.pos[cf.pos<0]=0
  cf.neg[cf.neg>0]=0
  if(all(!(is.na(ind)))) {
    fvfactor=as.numeric(lastinvec(ind))/ind
    cf.fv=cf*fvfactor
    alpha=log(1+irr.z(cf.fv,gips=TRUE))
    logpe.irr=log(1+ans[1])
    logdm.irr=logpe.irr-alpha
    ans[2]=-1+exp(logdm.irr)
    }
  return(ans)
}

#' evolution of pe performance
#'
#' @param cf is a cash flow zoo object
#' @param val is zoo object of valuations
#' @param idx ia a zoo object total return index
#' @keywords private_equity
#' @export
#' @examples
#' dates=as.Date("2018-1-1")+c(1,31,61)
#' cf=zoo(c(-100,5,10),dates)
#' ind=zoo(c(100,101,102),dates)
#' val=zoo(c(100,100),dates[-1])
#' pestats(cf,val,ind)
pe.performance.roll=function(cf,val,idx) {
  #calculate pe performance statististics
  #cf is cash flow as a zoo object, length >= 1, must contain a negative value
  #val is values as a zoo object, length >=1
  #idx is index as a zoo object, must contain index value for all dates in cf and val
  #returns a 5 column matrix
  ans=matrix(0,nrow=length(val),col=6)
  colnames(ans)=c('Date','IRR','TVPI','PME','PME+','Direct Alpha')
  if(!is.zoo(cf)|!is.zoo(val)|!is.zoo(idx)) stop('pe.performance.roll arguments must all be zoo objects')
  if (!all(index(cf) %in% index(idx))) stop('index object does not contain all dates in cash flow ')
  if (!all(index(val) %in% index(idx))) stop('index object does not contain all dates in values')
  for (i in length(val)) {
    v.i=val[i]
    date.i=index(v.i)
    cf.i=cf[index(cf)<=date.i]
    cf.i=mergesum.z(cf.i,v.i)
    dind=match(index(cf.i),index(idx))
    ans.i=pestats(cf.i,idx[dind])
    ans[i,1]=date.i
    ans[i,2]=ans.i$irr
    ans[i,3]=ans.i$tvpi
    ans[i,4]=ans.i$pme
    ans[i,5]=ans.i$pme.plus
    ans[i,6]=ans.i$alpha
  }
  ans.l=list()
  ans.l$irr=zoo(ans[,2],ans[,1])
  ans.l$tvpi=zoo(ans[,3],ans[,1])
  ans.l$pme=zoo(ans[,4],ans[,1])
  ans.l$pme.plus=zoo(ans[,5],ans[,1])
  ans.l$alpha=zoo(ans[,6],ans[,1])
  return(ans.l)
}


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
  #update 3/29/2019
  for (i in 1:length(ret)) {
    ansmat[, i] = wf(stack[-length(stack)], ret[i])
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
#' @param ans the result of a call to gpcomp
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

#' pe performance with cash flow data from file
#'
#' @param cf.filename the name of the file
#' @param bench.ticker.vec a vector of tickers
#' @param useallvals limts to "C" and "V" factors if FALSE
#' @param service is "yahoo" or "bloomberg"
#' @keywords private_equity
#' @export
#' @examples
#' file="filename"
#' pe.performance(file)
pe.performance=function(cf.filename,bench.ticker.vec=c('^RUT','SPY'),
                        useallvals=FALSE,service="yahoo") {
  #function requires zoo and quantmod
  #cf.filename is the name of a file
  # in csv format, column one "Date" is date yyyy/mm/dd
  # column two is cash flow, labeled "Amount" , negative = draw
  # column three, labeled "Type", is a code "C" means a cash flow and "V" means a value, only the last value is used
  #bench.ticker.lst is a a vetor of ticker names available
  # at yahoo finance
  #Returns a matrix with a column for each benchmark
  #Rows are
  # IRR
  # TVPI
  # Realized %
  # PME (Kaplan)
  # Alpha (Griffiths)
  # PME+ (Capital Dynamics)
  if(is.data.frame(cf.filename)) {
    x=cf.filename} else {
      x=read.csv(file=cf.filename)
    }
  # remove NAs
  # if(any(is.na(x$Date))) {warning("Bad date formats");stop}
  x$Date=as.Date(x$Date,format='%m/%d/%Y')
  x$Amount=as.numeric(x$Amount)
  badrows=which(is.na(x$Date)|is.na(x$Amount))
  if (length(badrows>0)) {
    warning(paste("you have data causing NAs at row",paste(badrows,collapse=" ")))
    warning("removing these rows and running calculations")
    x=x[-badrows,]
  }
  x=subset(x,x$Amount!=0|x$Type=="V")


  if(useallvals) {
    cf=aggregate(x$Amount,by=list(x$Date),sum)
    cf.wval=zoo(cf[,2],cf[,1])
    last=max(x$Date)
  } else {
    x.cf=subset(x,x$Type=='C')
    x.v=subset(x,x$Type=='V')
    if(dim(x.v)[1]==0) {warning("No valuation");stop}
    tval=lastinvec(x.v$Amount)
    last=lastinvec(x.v$Date)
    tval=zoo(tval,last)
    #remove amounts after valuation date
    x.cf=subset(x.cf,x.cf$Date<=last)
    #aggregate by date and convert to zoo obect
    cf=aggregate(x.cf$Amount,by=list(x.cf$Date),sum)
    cf.noval=zoo(cf[,2],cf[,1])
    cf.wval=mergesum.z(cf.noval,tval)
  }
  #download benchmarks
  first=min(x$Date)
  bench.lst=list()
  if (service == "yahoo") {
    for (i in 1:length(bench.ticker.vec)) {
      rut=getSymbols("^RUT",src='yahoo',from=first-5,to=last+5,auto.assign=FALSE)
      bench.lst[[i]]=as.zoo(rut[,6])
    }
  } else {
    conn=blpConnect()
    first=as.Date(first,"%Y%m%d")
    last=as.Date(last,"%Y%m%d")
    for (i in 1:length(bench.ticker.vec)) {
      temp=bdh(conn,bench.ticker.vec[i],"PX_LAST",first-5,last+5)
      bench.lst[[i]]=zoo(temp$PX_LAST,as.Date(temp$date))
    }
    blpDisconnect(conn)
  }
  names(bench.lst)=bench.ticker.vec
  #add missing days to benchmark
  #fill NAs with neighboring values
  days365=zooreg(rep(0,1+last-first),start=first,end=last)
  for (i in 1:length(bench.ticker.vec)){
    b=merge(days365,bench.lst[[i]])
    b=b[,2]
    b=na.approx(b,na.rm=FALSE)
    bench.lst[[i]]=b
  }
  #match dates for benchmark reduction index
  dind=match(index(cf.wval),index(bench.lst[[1]]))
  #call pestats for each benchmark
  ans=matrix(0,nrow=5,ncol=length(bench.lst))
  for (i in 1:length(bench.lst)) {
    perf=pestats(cf.wval,bench.lst[[i]][dind])
    ans[,i]=c(perf$tvpi,100*perf$irr,perf$pme,100*perf$ind.irr,100*perf$alpha)
  }
  #build the return matrix
  ans=round(ans,2)
  colnames(ans)=bench.ticker.vec
  rownames(ans)=c('TVPI','IRR','PME','Dollar Matched Index IRR','Direct Alpha')
  return(ans)
}

#' pe performance with cash flow data from file
#'
#' @param cf.filename the name of the file
#' @param bench.ticker.vec a vector of tickers
#' @param useallvals limts to "C" and "V" factors if FALSE
#' @param service is "yahoo" or "bloomberg"
#' @keywords private_equity
#' @export
#' @examples
#' file="filename"
#' pe.performance.return.index(file)
pe.performance.return.index=function(cf.filename,bench.ticker.vec=c('^RUT','SPY'),
                                     useallvals=FALSE,service="yahoo") {
  #function requires zoo and quantmod
  #cf.filename is the name of a file
  # in csv format, column one "Date" is date yyyy/mm/dd
  # column two is cash flow, labeled "Amount" , negative = draw
  # column three, labeled "Type", is a code "C" means a cash flow and "V" means a value, only the last value is used
  #bench.ticker.lst is a a vetor of ticker names available
  # at yahoo finance
  #Returns a matrix with a column for each benchmark
  #Rows are
  # IRR
  # TVPI
  # Realized %
  # PME (Kaplan)
  # Alpha (Griffiths)
  # PME+ (Capital Dynamics)
  if(is.data.frame(cf.filename)) {
    x=cf.filename} else {
      x=read.csv(file=cf.filename)
    }
  # remove NAs
  # if(any(is.na(x$Date))) {warning("Bad date formats");stop}
  x$Date=as.Date(x$Date,format='%m/%d/%Y')
  x$Amount=as.numeric(x$Amount)
  badrows=which(is.na(x$Date)|is.na(x$Amount))
  if (length(badrows>0)) {
    warning(paste("you have data causing NAs at row",paste(badrows,collapse=" ")))
    warning("removing these rows and running calculations")
    x=x[-badrows,]
  }
  x=subset(x,x$Amount!=0|x$Type=="V")


  if(useallvals) {
    cf=aggregate(x$Amount,by=list(x$Date),sum)
    cf.wval=zoo(cf[,2],cf[,1])
    last=max(x$Date)
  } else {
    x.cf=subset(x,x$Type=='C')
    x.v=subset(x,x$Type=='V')
    if(dim(x.v)[1]==0) {warning("No valuation");stop}
    tval=lastinvec(x.v$Amount)
    last=lastinvec(x.v$Date)
    tval=zoo(tval,last)
    #remove amounts after valuation date
    x.cf=subset(x.cf,x.cf$Date<=last)
    #aggregate by date and convert to zoo obect
    cf=aggregate(x.cf$Amount,by=list(x.cf$Date),sum)
    cf.noval=zoo(cf[,2],cf[,1])
    cf.wval=mergesum.z(cf.noval,tval)
  }
  #download benchmarks
  first=min(x$Date)
  bench.lst=list()
  if (service == "yahoo") {
    for (i in 1:length(bench.ticker.vec)) {
      rut=getSymbols("^RUT",src='yahoo',from=first-5,to=last+5,auto.assign=FALSE)
      bench.lst[[i]]=as.zoo(rut[,6])
    }
  } else {
    conn=blpConnect()
    first=as.Date(first,"%Y%m%d")
    last=as.Date(last,"%Y%m%d")
    for (i in 1:length(bench.ticker.vec)) {
      temp=bdh(conn,bench.ticker.vec[i],"PX_LAST",first-5,last+5)
      bench.lst[[i]]=zoo(temp$PX_LAST,as.Date(temp$date))
    }
    blpDisconnect(conn)
  }
  names(bench.lst)=bench.ticker.vec
  #add missing days to benchmark
  #fill NAs with neighboring values
  days365=zooreg(rep(0,1+last-first),start=first,end=last)
  for (i in 1:length(bench.ticker.vec)){
    b=merge(days365,bench.lst[[i]])
    b=b[,2]
    b=na.approx(b,na.rm=FALSE)
    bench.lst[[i]]=b
  }
  #match dates for benchmark reduction index
  dind=match(index(cf.wval),index(bench.lst[[1]]))
  #call pestats for each benchmark
  ans=matrix(0,nrow=5,ncol=length(bench.lst))
  for (i in 1:length(bench.lst)) {
    bench.lst[[i]]=bench.lst[[i]][dind]
    perf=pestats(cf.wval,bench.lst[[i]])
    ans[,i]=c(perf$tvpi,100*perf$irr,perf$pme,100*perf$ind.irr,100*perf$alpha)
  }
  #build the return matrix
  ans=round(ans,2)
  colnames(ans)=bench.ticker.vec
  rownames(ans)=c('TVPI','IRR','PME','Dollar Matched Index IRR','Direct Alpha')
  return(list(ans,bench.lst,cf.wval))
}
