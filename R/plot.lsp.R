
plot.lsp <- function(x, main ="Lomb-Scargle Periodogram", xlabel = NULL, ylabel = "normalized power", level = TRUE, plot=TRUE, ...) {  
  
  if (is.null(xlabel)) 
    xlabel <- x$type
  scn=pow=NULL
  dfp=data.frame(x$scanned,x$power)
  names(dfp)=c("scn","pow")
  p=ggplot(data=dfp,aes(x=scn,y=pow))+geom_line()
  
  if (level == TRUE) {
    if (!is.null(x$sig.level)) {
      
      p=p+geom_hline(yintercept=x$sig.level, linetype="dashed",color="blue")
      p=p+annotate("text",x=max(x$scanned)*0.85,y=x$sig.level*1.05,label=paste("P<",x$alpha),size=6,vjust=0)
    }
  }
  
  p=p+ggtitle(main)
  p=p+ ylab(ylabel)
  p=p+ xlab(xlabel)
  p=p+theme_lsp(20)
  if (plot==T) print(p)
  return (p)
}
