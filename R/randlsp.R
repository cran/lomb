
randlsp <- function(repeats=1000, x,times = NULL, from = NULL, to = NULL, type = c("frequency", "period"), ofac = 1, alpha = 0.01, plot = TRUE, trace = TRUE, ...) {
  
  if (is.ts(x)){
    x=as.vector(x)
  }
  
  if (!is.vector(x)) {
    times <- x[, 1]
    x <- x[, 2]
  }
  
  
  realres <- lsp(x, times, from, to, type, ofac, alpha,plot = plot, ...)
  realpeak <- realres$peak
  classic.p <-realres$p.value
  pks <- NULL
  if (trace == TRUE) 
    cat("Repeats: ")
  for (i in 1:repeats) {
    randx <- sample(x, length(x))  # scramble data sequence
    randres <- lsp(randx, times, from, to, type, ofac, alpha, plot = F)
    pks <- c(pks, randres$peak)
    if (trace == TRUE) {
      if (i/25 == floor(i/25)) 
        cat(i, " ")
    }
  }
  if (trace == TRUE) 
    cat("\n")
  
  prop <- length(which(pks >= realpeak))
  p.value <- prop/repeats
  p.value=round(p.value,digits=3)
  
  if (plot == TRUE) {
    
    p1=plot(realres,main="LS Periodogram",level=F)
    
    
    dfp=data.frame(pks)
    names(dfp)="peaks"
    p2=ggplot(data=dfp,aes(x=peaks))+geom_histogram(color="black",fill="white")
    p2=p2+geom_vline(aes(xintercept=realpeak),color="blue", linetype="dotted", size=1)
    p2=p2+theme_lsp(20)
    p2=p2+ xlab("peak amplitude")
    p2=p2+ggtitle(paste("P-value= ",p.value))
    
    
    suppressMessages(grid.arrange(p1,p2,nrow=1))
  }
  
  res=realres[-(8:9)]
  res=res[-length(res)]
  res$random.peaks = pks
  res$repeats=repeats
  res$p.value=p.value
  res$classic.p=classic.p
  class(res)="randlsp"
  return(invisible(res))
}
