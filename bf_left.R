#pe performance
#data is data frame zoo object, 1st column is cash flow (negative = draw), 2nd column is market index
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
  require(Rbbg)
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
    require(Rbbg)
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

pe.performance.roll=function(cf,val,idx) {
#calculate pe performance statististics (TVPI, IRR, PME, PME+)
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
return(ans.l)
}

#line of credit function
#args are beginning balance (default=0), interest rate and a cash flow
#calculates a line of credit assuming all negative cash is financed
#by the LOC. Interest is calculated on the average loan balance beginning 
#of period and end of period.  
#returns in a list the ending loan balance, interest expense and any leftover cash
#sign convention -- cash flows negative is cash out, positive cash in --
#for loan balance -- positive number reflects a loan balance
#uses solve to find answer to system of linear equations
lineofcredit=function(cf,ir,beg=0) {
  begbal=vector()
  begbal[1]=beg
  endbal=vector()
  intexp=vector()
  remain=vector()
  for (i in 1:length(cf)) {
    if(cf[i]>=((1+(ir/2))*begbal[i])) {
      intexp[i]=(ir/2)*begbal[i]
      endbal[i]=begbal[i+1]=0
      remain[i]=cf[i]-begbal[i]+endbal[i]-intexp[i]
    } else {
      coef=matrix(c(1,-1,-1,ir/2),byrow=TRUE,nrow=2)
      ys=c(cf[i]-begbal[i],-ir/2*begbal[i])
      ans=solve(coef,ys)
      intexp[i]=ans[1]
      endbal[i]=begbal[i+1]=ans[2]
      remain[i]=cf[i]-begbal[i]+endbal[i]-intexp[i]
    }
  }
  ans=list()
  ans$intexp=intexp
  ans$endbal=endbal
  ans$remain=remain
  return(ans)
}


blankfun=function(x,digits,addtxt,big.mark) {
  ifelse((x==0|is.na(x)|is.nan(x)),'   ',
         paste0(format(round(x,digits),big.mark=big.mark,nsmall=digits),addtxt)
         )
}
formatwblank=function(x,digits=0,addtxt=NULL,big.mark=',') {
  sapply(x,blankfun,digits,addtxt,big.mark)
}
grow.z=function(r,c,n,freq=1,start=1,retclass='zoo') {
  if('zoo'==retclass & start==1) {
    if(is.zoo(c)) start=time(c)
  }
  n=n*freq
  r=-1+(1+r)^(1/freq)
  ts=as.numeric(c)*(1+r)^(0:(n-1))
  if ('zoo'==retclass) {
    ts=zooreg(ts,start=start,freq=freq)
  }
  return(ts)
}

