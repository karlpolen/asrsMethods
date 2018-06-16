

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
