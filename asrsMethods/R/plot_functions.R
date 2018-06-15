#' cone plot
#'
#' creates a cone plot which cumulative investment performance compared one and two sigma bands
#' @param return is the return
#' @param annual_expected_return is the expected return
#' @param annual_standard_deviation is the expected volatility
#' @param periodicity is yearly, quarterly or monthly
#' @param ylabel an optional y label for the graph
#' @keywords plot
#' @export
#' @examples
#' coneplot(rnorm(20,.02,.04),.02,.04,'m')
coneplot=function(return,annual_expected_return,annual_standard_deviation,
                  periodicity=c('y','q','m'),ylabel=NULL) {
  #
  # return is a time series of monthly, quarterly or annual arithmetic return values
  # annual_expected_return is the annual expected return for the strategy
  # annual_standard_deviation is the expected annual volatility
  # periodicity is 'y', 'q' or 'm' indicating yearly, quarter or monthly data
  # ylabel is optional argument to provide a label for the y axis
  #
  # requires lubridate,zoo packages
  #
  n=length(return)
  months=0:n
  timelabel=months
  if (is.zoo(return)) {
    datehold=time(return)
    if(class(datehold)=="Date") {
      if (periodicity=='y') datehold=year(datehold)
      if (periodicity=='q') datehold=as.yearqtr(datehold)
      if (periodicity=='m') datehold=as.yearmon(datehold)
    }
    timelabel=as.numeric(datehold)
  }
  if(periodicity=='y') {
    expected_return=annual_expected_return
    standard_deviation=annual_standard_deviation
    if(is.zoo(return)) {
      timelabel=c(timelabel[1]-1,timelabel)
    }
    xlabel="Yearly Data"
  }
  if(periodicity=='q') {
    expected_return=-1+exp(log(1+annual_expected_return)/4)
    standard_deviation=annual_standard_deviation/sqrt(4)
    if(is.zoo(return)) {
      timelabel=c(timelabel[1]-(1/12),timelabel)
    }
    xlabel="Quarterly Data"
  }
  if(periodicity=='m') {
    expected_return=-1+exp(log(1+annual_expected_return)/12)
    standard_deviation=annual_standard_deviation/sqrt(12)
    if(is.zoo(return)) {
      timelabel=c(timelabel[1]-.25,timelabel)
    }
    xlabel="Monthly Data"
  }
  expected_gd=c(1,exp(cumsum(rep(log(1+expected_return),n))))
  actual_gd=c(1,exp(cumsum(log(1+coredata(return)))))
  one_sigma_up=expected_gd+(standard_deviation*months/sqrt(n))
  two_sigma_up=expected_gd+(2*standard_deviation*months/sqrt(n))
  one_sigma_down=expected_gd-(standard_deviation*months/sqrt(n))
  two_sigma_down=expected_gd-(2*standard_deviation*months/sqrt(n))
  conedf=data.frame(timelabel,expected_gd,actual_gd,one_sigma_up,two_sigma_up,
                    one_sigma_down,two_sigma_down)
  if (is.null(ylabel)) ylabel=names(x)[1]
  x=ggplot(conedf,(aes(x=timelabel)))+ylab(ylabel)+xlab(xlabel)+
    geom_line(linetype=2,colour='black',aes(y=expected_gd))+
    geom_line(colour='blue',aes(y=actual_gd))+
    geom_line(linetype=2,colour='orange',aes(y=one_sigma_up))+
    geom_line(linetype=2,colour='orange',aes(y=one_sigma_down))+
    geom_line(linetype=2,colour='red',aes(y=two_sigma_up))+
    geom_line(linetype=2,colour='red',aes(y=two_sigma_down))
  return(x)
}
