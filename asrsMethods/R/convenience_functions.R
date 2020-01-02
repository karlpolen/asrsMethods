#' last elecment in vector
#' 
#' convenience function to grab the last element in a vector
#' @param x is the vector
#' @param naremove changes trailing NA to zero if set to true
#' @keywords convenience
#' @export
#' @examples 
#' lastinvec(1:3)
lastinvec=function(x,naremove=FALSE) {
  ans=tail(x,1)
  if(length(ans)> 0){
    if(naremove & is.na(ans)) ans=0
  }
  return(ans)
}
#' first in vector
#' 
#' convenience function to return first element in a vector
#' @param x is the vector
#' @keywords convenience
#' @export
#' @examples 
#' firstinvec(1:3)
firstinvec=function(x) {x[1]}

#' Merge Sum
#' 
#' Merge a list of zoo objects and return a single zoo object which is there sum
#' @param x the zoo objects
#' @keywords convenience
#' @export
#' @examples 
#' mergesum.z(list(zoo(c(1,2),c(3,4)),zoo(c(2,3),c(4,5))))
mergesum.z=function(x,...) {
  mt=merge(x,...,fill=0)
  if(is.null(dim(mt))) return(mt)
  zoo::zoo(rowSums(mt),time(mt))
}

#' Merge list
#' 
#' Merge a list of zoo objects into a zoo data.frame
#' @param x the zoo objects
#' @keywords convenience
#' @export
#' @examples 
#' mergelist.z(list(zoo(c(1,2),c(3,4)),zoo(c(2,3),c(4,5))))
mergelist.z=function(z.list) {
  listlen=length(z.list)
  if (listlen==0) return(z.list)
  if (listlen==1) {
    ans=matrix(unlist(z.list),ncol=1)
    colnames(ans)=names(z.list)
    return(ans)
  }
  len.each=lapply(z.list,length)
  len.idx=which(len.each==0)
  for (i in len.idx) {
    z.list[[i]]=zoo(0,today())
  }
  ans=merge(z.list[[1]],z.list[[2]],fill=0)
  if (listlen==2) {
    colnames(ans)=names(z.list)
    return(ans)
  }
  for (k in 3:(listlen)) {
    ans=merge(ans,z.list[[k]],fill=0)
  }
  colnames(ans)=names(z.list)
  return(ans)
}

#' Merge Sum
#' 
#' Merge a list of zoo objects and return a single zoo object which is there sum
#' @param x the zoo objects
#' @keywords convenience
#' @export
#' @examples 
#' zoosum(list(zoo(c(1,2),c(3,4)),zoo(c(2,3),c(4,5))))
zoosum = function(...) {
  sumz = merge(...)
  if(is.null(dim(sumz))) return(sumz)
  zoo(rowSums(sumz, na.rm = TRUE), time(sumz))
}

#' Add total
#' 
#' Calculate the total of a list of zoo objects and as a new zoo object called total
#' @param x the zoo objects
#' @keywords convenience
#' @export
#' @examples 
#' addtotal.list(list(zoo(c(1,2),c(3,4)),zoo(c(2,3),c(4,5))))
addtotal.list = function(x) {
  total = do.call(zoosum, x)
  x$Total = total
  return(x)
}

#' Merge zero
#' 
#' Merge with a fill=0 parameter
#' @keywords convenience
#' @export
#' @examples 
#' merge0(list(zoo(c(1,2),c(3,4)),zoo(c(2,3,4),c(4,5,6))))
merge0 = function(...) {
  merge(..., fill = 0)
}

#' Growth of a dollar
#' 
#' Growth of a dollar of an arithmetic return series
#' @param x an arithmetic return series
#' @keywords convenience
#' @export
#' @examples 
#' gd(c(.1,.2))
gd=function(x) {
  exp(cumsum(log(1+x)))
}

#' Growth of a dollar
#' 
#' Growth of a dollar of an arithmetic return series returns on ly the return part
#' @param x an arithmetic return series
#' @keywords convenience
#' @export
#' @examples 
#' gdminus1(c(.1,.2))
gdminus1=function(x,one=1) {
  temp=gd(x)
  temp-matrix(one,nrow=nrow(x),ncol=ncol(x),byrow=TRUE)
}

#' Growth of a dollar weight
#' 
#' Growth of a dollar of an arithmetic return series returns on ly the return part
#' @param x an arithmetic return series
#' @keywords convenience
#' @export
#' @examples 
#' gdminus1(c(.1,.2))
gdweight=function(x){
  temp=gd(x)
  temp/matrix(rowSums(temp),ncol=ncol(x),nrow=nrow(x))
}

#' gg
#' 
#' convert a zoo data.frame to long data frame with date as a column ready for ggplot
#' @param zoo.object is the zoo object
#' @param wide starting form is wide if true
#' @param dataname "Value" is default
#' @param datename "Date" is default
#' @param category "Categories is default
#' @keywords convenience
#' @export
#' @examples 
#' unzoo(merge0(list(zoo(c(1,2),c(3,4)),zoo(c(2,3,4),c(4,5,6))))) 
gg=function(x,variable.name="Variable",value.name="Value") {
  melt(unzoo(x),id.vars="Date",
       variable.name=variable.name,
       value.name=value.name)
}

#' unzoo
#' 
#' convert a zoo data.frame to regular data frame with date as a column
#' @param zoo.object is the zoo object
#' @param wide starting form is wide if true
#' @param dataname "Value" is default
#' @param datename "Date" is default
#' @param category "Categories is default
#' @keywords convenience
#' @export
#' @examples 
#' unzoo(merge0(list(zoo(c(1,2),c(3,4)),zoo(c(2,3,4),c(4,5,6))))) 
#'
unzoo=function(zoo.object,wide=TRUE,dataname="Value",datename="Date",category="Categories") {
  if(!is.zoo(zoo.object)) stop("not a zoo object")
  t=time(zoo.object)
  v=coredata(zoo.object)
  if(is.null(dim(zoo.object))) {
    ans=data.frame(t,v)
    colnames(ans)=c(datename,dataname)
    return(ans)
  }
  ans=as.data.frame(v)
  columnames=c(datename,colnames(v))
  ans=cbind(t,ans)
  colnames(ans)=columnames
  if(wide) return(ans)
  require(tidyr)
  colnames(ans)=c("Date",colnames(v))
  ans=gather(ans,cats,value,-Date)
  colnames(ans)=c(datename,category,dataname)
  return(ans)
}

#' window a list
#' 
#' apply window to all members of a list of zoo objects
#' @param x is the zoo list
#' @param start is the start date
#' @param end is the end date
#' @keywords convenience
#' @export
#' @examples 
#' cf1=zoo(1:20, as.Date("2018-1-1")+(1:20*40))
#' cf2=zoo(1:20, as.Date("2018-1-1")+(1:20*50))
#' x=list(cf1,cf2)
#' window.list(x,as.Date("2018-1-1),as.Date("2018-12-31))
window.list=function(x,start,end) {
  ans=x
  for (i in 1:length(x)) {
    ans[[i]]=window(x[[i]],start=start,end=end)
  }
  return(ans)
}

#' zoo list to a table
#' 
#' make a table out of a list of zoo objects organized by year, month or quarter
#' @param x is the zoo list
#' @param by is "year", "quarter" or "month"
#' @param time.horizontal makes time as the columns if TRUE otherwise as rows
#' @param fun is function for aggregating by time frame, defaults to sum
#' @keywords convenience
#' @export
#' @examples 
#' cf1=zoo(1:20, as.Date("2018-1-1")+(1:20*40))
#' cf2=zoo(1:20, as.Date("2018-1-1")+(1:20*50))
#' x=list(cf1,cf2)
#' make.table(x)
make.table=function(x,by='year',time.horizontal=TRUE,fun=sum) {
  table=do.call(merge0,x)
  if (by=='year') table=aggregate(table,by=year(time(table)),fun)
  if (by=='quarter') table=aggregate(table,by=as.yearqtr(time(table)),fun)
  if (by=='month') table=aggregate(table,by=as.yearmon(time(table)),fun)
  timeline=time(table)
  table=as.matrix(table)
  rownames(table)=as.character(timeline)
  if(time.horizontal) table=t(table)
  return(table)
}

#' blankfun
#' 
#' convenice function to format numbers
#' @param x is a number
#' @param digits is number of digits for rounding
#' @param addtxt is text to append after a number 
#' @param big.mark is the separator every three digits, usually ","
#' @export
#' @examples 
#' blankfun(3.1416,2,"",",")
blankfun=function(x,digits,addtxt,big.mark) {
  ifelse((x==0|is.na(x)|is.nan(x)),'   ',
         paste0(format(round(x,digits),big.mark=big.mark,nsmall=digits),addtxt)
  )
}


#' format a vector of numbers
#' 
#' uses blankfun with sapply
#' @param x is the vector of numbers
#' @param digits is the number of digits for rounding
#' @param addtxt is to text to add after each number
#' @param big.mark defaults to ","
#' @keywords convenience
#' @export
#' @examples 
#' formatwblank(sqrt(1:4),2)
formatwblank=function(x,digits=0,addtxt=NULL,big.mark=',') {
  sapply(x,blankfun,digits,addtxt,big.mark)
}

