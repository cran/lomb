
makedf<- function (tvar,pvar){
  cm=data.frame(tvar,pvar)
  cm=na.omit(cm)
  tvar=cm$tvar
  pvar=cm$pvar

dd=difftime(tvar,tvar[1],units="hours")
dd=as.numeric(dd)
fr=data.frame(tvar=dd,pvar=pvar)
return(fr)


}


