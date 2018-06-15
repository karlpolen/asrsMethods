#' ASRS IMD palette
#' 
#' the palette for ASRS presentations
#' @keywords palette
#' @export
#' @examples 
#' IMD.palette()
IMD.palette=function(){
  return(c("#00619C","#DF8F26","#017769","#A61F2A",
           "#92D7EC","#FFCB78","#6DBCB6","#E27D84",
           "#886FA3","#4A2D7F","#A1A2A2","#F389B8"))
}

#' Display palette
#' 
#' Show the palette for ASRS presentations
#' @param palis a palette default to IMD.palette
#' @keywords palette
#' @export
#' @examples 
#' display.palette()
display.palette=function(pal=IMD.palette()) {
  pie(rep(1,length(pal)),labels=seq_along(pal),col=pal)
}

#' ASRS PME palette
#' 
#' the palette for PME graphs
#' @keywords palette
#' @export
#' @examples 
#' PME.palette()
PME.palette=function(){
  return(c("#C05640", "#EDD170", "#1ECFD6", "#0878A4", "#003D73"))
}