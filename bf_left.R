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
