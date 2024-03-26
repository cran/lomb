pershow=function(object){
  
  datn=data.frame(scanned=object$scanned,power=object$power)
  fig=plot_ly(data=datn,type="scatter",mode="lines+markers",linetype="solid",
              x=~scanned,y=~power) #%>%
  
  fig
}

