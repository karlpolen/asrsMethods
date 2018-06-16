

#performance report
invest.measures=function(cf,disc,f=Frequency) {
  cf.irr=100*irr(cf,frequ=f)
  name1="IRR"
  cf.npv=npv(disc/f,cf)
  name2= paste("NPV at",as.character(disc*100),"%")
  cf.tvpi=tvpi(cf)
  name3="Total Value Multiple to Invested"
  cf.payback=payback(cf)
  pbnames=c("years","2","3","Quarters","5","6","7","8","9","10","11","Months")
  name4=paste("Payback in",pbnames[f])
  ans=matrix(c(cf.irr,cf.npv,cf.tvpi,cf.payback),4,1)
  rownames(ans)=c(name1,name2,name3,name4)
  colnames(ans)=" "
  round(ans,2)
} 


# simple analysis of incentive pay in a multi-tier waterfall structure
incent=function(deal,return){
  if (is.null(dim(deal))) deal=matrix(deal,1,3,byrow=TRUE)
  colnames(deal)<-c("pref","carry","catchup")
  base=excess=matrix(0,0,length(return))
  incentfee=matrix(0,2,length(return))
  pref3=pref2=c(deal[,"pref"],10000)
  prefdif=diff(pref2)
  pref3[1]=0
  for (j in 1:dim(deal)[1]) {
    excess=rbind(excess,(pmax(0,pmin(prefdif[j],return-deal[j,"pref"]))))
    base=rbind(base,(pmin(pref2[j+1],return))*(return>pref3[j]))
    prior=(base[j,]>0)*colSums(incentfee)
    x=pmin(excess[j,]*deal[j,"catchup"],base[j,]*deal[j,"carry"]-prior)
    incentfee=rbind(incentfee,x)
  }
  colSums(incentfee)
}

#' z object to annual
#'  
#' convert quarterly or monthly zoo object to annual zoo object
toannual=function(z,fun=sum) {
  ans=aggregate(z,as.character(floor(index(z))),fun)
  rownames(ans)=unique(as.character(floor(index(z))))
  ans
}
toannual.d=function(z,fun=sum,fiscal=.5) {
  ans=aggregate(z,as.yearqtr,fun)
  ans=aggregate(ans,as.character(floor(fiscal+index(ans))),fun)
  return(ans)
}
#convert zoo data to quarterly
toquarterly=function(z,fun=sum) {
  ans=aggregate(z,as.yearqtr(index(z)),fun)
  rownames(ans)=unique(as.character(as.yearqtr(index(z))))
  ans
}



#maketable is merge and change NA's to zero, establish names for variables
maketable=function(x,y,...,names=NULL) {
  ans=merge(x,y,...)
  ind=index(ans)
  ans=apply(ans,2,natozero)
  ans=zoo(ans,ind)
  if(!is.null(names)) colnames(ans)=names
  ans   
}
#monthdiff calculates difference in months between two dates represented c(year,mon)
monthdiff=function(b,e) {
  ans=12*(e[1]-b[1])
  ans+1-b[2]+e[2]
}
#monthadd calculates a new c(year,mon) from start and n=months to add
monthadd=function(start,n) {
  ans=start
  ans[2]=ans[2]+n-1
  ans[1]=ans[1]+floor(ans[2]/12)
  ans[2]=1+(ans[2]%%12)
  ans
}

#create a zoo object with global variable defaults
fullzoo=function(x=rep(0,Term),start=Start,end=End,frequency=Frequency,growth=0,lag=0,warn=FALSE) {
  if (length(x)==1) {
    vals=c(rep(0,lag),grow(x,Term-lag,growth))
    ans=zooreg(vals,start,end,frequency)
    return(ans)
  }
  x=c(rep(0,lag),x)
  if((length(x)!=Term)&warn) print('fullzoo warning -- number of values do not equal term,will pad zeros or truncate')
  if(length(x)<Term) {x=c(x,rep(0,Term-length(x)))}
  if(length(x)>Term) {x=x[1:Term]}
  ans=zooreg(x,start,end,frequency)
  return(ans)
}
#create an index to a zoo object with global variable defaults
fullzoo.ind=function(x=rep(0,Term),start=Start,end=End,frequency=Frequency) {
  if(length(x)!=Term) print('fullzoo warning -- number of values do not equal term')
  index(zooreg(x,start,end,frequency))
}
#create a yearmon for the fullzoo index
fullzoo.ym=function(x=rep(0,Term),start=Start,end=End,frequency=Frequency) {
  if(length(x)!=Term) print('fullzoo warning -- number of values do not equal term')
  as.yearmon(index(zooreg(x,start,end,frequency)))
}
#create zoo object with single value using global defaults for overall size
sparsezoo=function(v,date) {
  if(monthdiff(Start,date)<0) print('sparsezoo warning-date out of range')
  if((monthdiff(date,End)+1-length(v))<0) print('sparsezoo warning-date out of range')
  ans=fullzoo()
  v.z=zooreg(v,date,frequency=Frequency)
  si=match(index(v.z),fullzoo.ind())
  if(length(v)==1) {ans[si]=v}
  if(length(v)>1) {
    for (i in 1:(length(v))) {
      ans[si+i-1]=v[i]
    }    
  }
  ans
}
#assign in to a zoo object by month using global defaults
bymonzoo=function(v,month,growth=0) {
  ans=fullzoo()
  ans.ym=fullzoo.ym()
  if(is.numeric(month)) {
    month=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")[month]
  }
  ans.ind=grep(month,ans.ym)
  if(length(ans.ind)==0) {
    print('bymonzoo warning -- no match, format for month is Jan, Feb, Mar, . . .')
    return(ans)
  }
  if(length(v)==1) {
    values=grow(v,length(ans.ind),growth)
    for (i in 1:length(ans.ind)) {
      ans[ans.ind[i]]=values[i]
    }
    return(ans)
  }
  if(length(v)>length(ans.ind)) print('bymonzoo warning -- too many values provided, not all used')
  if(length(v)<length(ans.ind)) print('bymonzoo warning -- not enough values, final value repeated')
  for (i in 1:(length(ans.ind))) {
    ans[ans.ind[i]]=v[min(i,length(v))]
  }
  return(ans)
}
# initiate empty data frames
initiate.cat=function(name) {
  ans=data.frame(fullzoo())
  colnames(ans)=name
  ans
}
# end a category by adding a total and removing the all zero 1st row
total.cat=function(x) {
  x$"Total"=rowSums(x)
  zoo(x,fullzoo.ind())
}
end.cat=function(x) {
  zoo(x,fullzoo.ind())
}
makeannualreport=function(x,...,orient="h",decim=0) {
  ans=round(toannual(merge(x,...)),decim)
  if (orient=="h") ans=t(ans)
  ans
}
allcases=function(cases) {
  ncol=length(cases)
  ans=matrix(0,prod(cases),ncol)
  ans[,ncol]=1:cases[ncol]
  if(ncol==1) return(ans)
  for (i in 1:(ncol-1)) {
    nprior=prod(cases[ncol:(ncol-i+1)])
    ans[,ncol-i]=rep(1:cases[ncol-i],rep(nprior,cases[ncol-i]))
  }
  return(ans)
  #old version below
  #ans=permutations(max(cases),length(cases),repeats.allowed=TRUE)
  #ans.ind=t(ans)<=cases
  #ans.ind=apply(ans.ind,2,all)
  #ans[ans.ind,]
}
casemat=function(x,...,names) {
  x=list(x,...)
  narg=length(x)
  chars=sapply(x,is.character)
  x.ind=as.matrix(allcases(sapply(x,length)))
  ncases=dim(x.ind)[1]
  ans.mat=matrix(" ",ncases,narg)
  ans2=list()
  for (i in 1:narg) {
    ans.mat[,i]=x[[i]][x.ind[,i]]
    ans2[[i]]=x[[i]][x.ind[,i]]
  }
  ans2=data.frame(ans2)
  colnames(ans2)=names
  for (i in 1:narg) {
    if (chars[i]) { 
      for (j in 1:ncases) {
        ans.mat[j,i]=paste("'",ans.mat[j,i],"'",sep="")
      }
    }
  }  
  for (i in 1:narg) {
    for (j in 1:ncases) {
      ans.mat[j,i]=paste(names[i],"=",ans.mat[j,i],sep="")
    }
  }
  ans=ans.mat[,1]
  if(narg==1) return(list(ans,ans2))
  for (i in 1:ncases) {
    for (j in 2:narg) {
      ans[i]=paste(ans[i],ans.mat[i,j],sep=",")      
    }
  }
  return(list(ans,ans2))
}
casemanager=function(fun,x,...,argnames,ansnames) {
  cases=casemat(x,...,names=argnames)
  case.str=cases[[1]]
  case.mat=cases[[2]]
  #for each case run fun and accumulate the answers in a list called answers
  answers=matrix(0,length(case.str),length(ansnames))
  for (i in 1:length(case.str)) {
    eval(parse(text=paste("temp=",fun,"(",case.str[i],")"))) 
    answers[i,]=as.vector(temp[[1]])
  }
  #organize the case data frame
  #data contract is that answer is a list and we take first element of the as the data.frame metrics
  answers=data.frame(answers)
  colnames(answers)=ansnames
  return(cbind(case.mat,answers))
}

lsp <- function(package, all.names = FALSE, pattern) 
{
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"), 
    all.names = all.names, 
    pattern = pattern
  )
}



## replacement for tseries get.hist.quote
get.hist.quote.fix=function (instrument = "^gdax", start, end, quote = c("Open", 
                                                                         "High", "Low", "Close"), provider = c("yahoo", "oanda"), 
                             method = NULL, origin = "1899-12-30", compression = "d", 
                             retclass = c("zoo", "its", "ts"), quiet = FALSE, drop = FALSE) 
{
  if (missing(start)) 
    start <- "1991-01-02"
  if (missing(end)) 
    end <- format(Sys.Date() - 1, "%Y-%m-%d")
  provider <- match.arg(provider)
  retclass <- match.arg(retclass)
  start <- as.Date(start)
  end <- as.Date(end)
  if (is.null(method)) {
    method <- getOption("download.file.method")
    if (is.null(method)) 
      method <- "auto"
  }
  if (provider == "yahoo") {
    url <- paste("http://ichart.finance.yahoo.com/table.csv?s=", 
                 instrument, format(start, paste("&a=", as.character(as.numeric(format(start, 
                                                                                       "%m")) - 1), "&b=%d&c=%Y", sep = "")), format(end, 
                                                                                                                                     paste("&d=", as.character(as.numeric(format(end, 
                                                                                                                                                                                 "%m")) - 1), "&e=%d&f=%Y", sep = "")), "&g=", 
                 compression, "&q=q&y=0&z=", instrument, "&x=.csv", 
                 sep = "")
    destfile <- tempfile()
    i <- 1L
    repeat {
      status <- tryCatch(download.file(url, destfile, 
                                       method = method, quiet = quiet), error = identity)
      if (!inherits(status, "error") && (status == 0)) 
        break
      unlink(destfile)
      if (i >= 5L) {
        if (inherits(status, "error")) 
          stop(conditionMessage(status))
        else stop(sprintf("download error, status %d", 
                          status))
      }
      message("download error, retrying ...")
      i <- i + 1L
    }
    nlines <- length(count.fields(destfile, sep = "\n"))
    if (nlines == 1) {
      unlink(destfile)
      stop(paste("no data available for", instrument))
    }
    x <- read.table(destfile, header = TRUE, sep = ",", 
                    as.is = TRUE, fill = TRUE)
    x <- na.omit(x)
    if (nrow(x) >= 2L && x[1L, 1L] == x[2L, 1L]) {
      warning("first date duplicated, first instance omitted")
      x <- x[-1L, , drop = FALSE]
    }
    unlink(destfile)
    names(x) <- gsub("\\.", "", names(x))
    nser <- pmatch(quote, names(x)[-1]) + 1
    if (any(is.na(nser))) 
      stop("this quote is not available")
    n <- nrow(x)
    dat <- as.Date(as.character(x[, 1]), "%Y-%m-%d")
    if (!quiet && (dat[n] != start)) 
      cat(format(dat[n], "time series starts %Y-%m-%d\n"))
    if (!quiet && (dat[1] != end)) 
      cat(format(dat[1], "time series ends   %Y-%m-%d\n"))
    if (retclass == "ts") {
      jdat <- unclass(julian(dat, origin = as.Date(origin)))
      ind <- jdat - jdat[n] + 1
      y <- matrix(NA, nrow = max(ind), ncol = length(nser))
      y[ind, ] <- as.matrix(x[, nser, drop = FALSE])
      colnames(y) <- names(x)[nser]
      y <- y[, seq_along(nser), drop = drop]
      return(ts(y, start = jdat[n], end = jdat[1]))
    }
    else {
      x <- as.matrix(x[, nser, drop = FALSE])
      rownames(x) <- NULL
      y <- zoo(x, dat)
      y <- y[, seq_along(nser), drop = drop]
      if (retclass == "its") {
        if (inherits(tryCatch(getNamespace("its"), error = identity), 
                     "error")) 
          warning("package its could not be loaded: zoo series returned")
        else {
          index(y) <- as.POSIXct(index(y))
          y <- its::as.its(y)
        }
      }
      return(y)
    }
  }
  else if (provider == "oanda") {
    if (!missing(quote)) {
      warning("argument 'quote' ignored for provider 'oanda'")
    }
    if (!missing(compression)) {
      warning("argument 'compression' ignored for provider 'oanda'")
    }
    url <- paste("http://www.oanda.com/convert/fxhistory?lang=en&date1=", 
                 format(start, "%m"), "%2F", format(start, "%d"), 
                 "%2F", format(start, "%y"), "&date=", format(end, 
                                                              "%m"), "%2F", format(end, "%d"), "%2F", format(end, 
                                                                                                             "%y"), "&date_fmt=us&exch=", unlist(strsplit(instrument, 
                                                                                                                                                          split = "/"))[1], "&exch2=&expr=", unlist(strsplit(instrument, 
                                                                                                                                                                                                             split = "/"))[2], "&expr2=&margin_fixed=0&&SUBMIT=Get+Table&format=ASCII&redirected=1", 
                 sep = "")
    destfile <- tempfile()
    status <- download.file(url, destfile, method = method, 
                            quiet = quiet)
    if (status != 0) {
      unlink(destfile)
      stop(paste("download error, status", status))
    }
    x <- readLines(destfile, warn = quiet)
    unlink(destfile)
    if (length(grep("Sorry", x)) > 0) {
      msg <- unlist(strsplit(gsub("<[a-zA-Z0-9\\/]*>", 
                                  "", x[grep("Sorry", x)]), split = " "))
      msg <- paste(msg[msg != ""], collapse = " ")
      stop("Message from Oanda: ", msg)
    }
    first <- grep("<PRE>", x, fixed = TRUE)
    last <- grep("</PRE>", x, fixed = TRUE) - 1
    if ((length(first) != 1) || (length(last) != 1) || (last < 
                                                        first)) {
      stop(paste("no data available for", instrument))
    }
    x[first] <- sub(".*<PRE>", "", x[first])
    con <- textConnection(x[first:last])
    on.exit(close(con))
    x <- scan(con, what = list(character(), double()), quiet = TRUE)
    dat <- as.Date(x[[1]], format = "%m/%d/%Y")
    n <- length(dat)
    if (!quiet && (dat[1] != start)) 
      cat(format(dat[1], "time series starts %Y-%m-%d\n"))
    if (!quiet && (dat[n] != end)) 
      cat(format(dat[n], "time series ends   %Y-%m-%d\n"))
    if (retclass == "ts") {
      jdat <- unclass(julian(dat, origin = as.Date(origin)))
      ind <- jdat - jdat[1] + 1
      y <- rep.int(NA, max(ind))
      y[ind] <- x[[2]]
      return(ts(y, start = jdat[1], end = jdat[n]))
    }
    else {
      y <- zoo(x[[2]], dat)
      if (retclass == "its") {
        if (inherits(tryCatch(getNamespace("its"), error = identity), 
                     "error")) 
          warning("package its could not be loaded: zoo series returned")
        else {
          index(y) <- as.POSIXct(index(y))
          y <- its::as.its(y)
        }
      }
      return(y)
    }
  }
  else stop("provider not implemented")
}
