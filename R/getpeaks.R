
getpeaks=function (object,npeaks=5,plotit=TRUE){

  pks=findpeaks(object$power,npeaks=npeaks,minpeakheight=0,sortstr=TRUE) 
  peaks=pks[,1] 
  tmes=object$scanned[pks[,2]]
  tme=round(tmes,2)
  if (plotit==TRUE){
    p=plot.lsp(object)
    p=p+ylim(0,peaks[1]*1.2)
    for (i in 1:npeaks){
      p=p+annotate("text", label=paste(tme[i]),y=peaks[i],x=tme[i],color="red",angle=45,size=6,vjust=-1,hjust=-.1)
    }
    
    print(p)
  }
  
  d=data.frame(time=tme,peaks=peaks)
  
  return(d)
}