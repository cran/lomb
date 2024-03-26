
utils::globalVariables(c("dechour", "yp", "y", "up","down"))



actogram=function(date,response,from=NULL,to=NULL,scalefac=.5,subtract=0,dble=TRUE,dig=FALSE,
              border="black", fill="red", grad=FALSE,lwd=0.3,
              photo=TRUE, latitude=NULL, longitude=NULL, zone=0,twilight ="nautic") {

  
# date: required, POSIXlt of date, format "%Y-%m-%d %T"
# response: required, which Variable to plot
# from,to: dates for plotting subsection
# scalefac: one day is 1,0 wide. Use >1 if plots are exaggerated
# subtract; Value to subtract from response before scaling
# dble=T: make double plot
# dig=T: bin into 0 or 1
# border_colour: coulor of rectangle edges
# fill_colour: colour of rectangle fills, relevant only if timepoint separation is large
# grad==TRUE :plot gradient
# photo=T: plot sun up and down as lines on left half (0-24 h)
# longitude, latitude for photoperiod. Default:zone = 0
# zone:Time zone
# sun rise/set to use: rise/set, or civil, or nautic

  date=as.POSIXlt(date, format="%Y-%m-%d %T",tz <- "Europe/Vienna")

# Store data in pframe 
pframe=data.frame(date)


pframe$plotvar=response-subtract


if (dig==TRUE){
  m=mean(pframe$plotvar,na.rm=TRUE)
  pframe$plotvar=ifelse(pframe$plotvar>m,1,0)
}
#### from/to ####
if(!is.null(from)){
  from=paste(from, "00:01:00")
  from=as.POSIXlt(from)
}

if(!is.null(to)){
  to=paste(to, "23:59:59")
  to=as.POSIXlt(to)
}
nn=length(pframe$date)
if (is.null(from)) from=pframe$date[1] 
if (is.null(to)) to= pframe$date[nn]

from=substr(from,1,19)
to=substr(to,1,19)

ix=which(pframe$date>from & pframe$date<to)
pframe=pframe[ix,]


focus=pframe


day=difftime(pframe$date,pframe$date[1],units="days")
day=as.numeric(floor(day))+1
pframe$day=day
ndays=max(day)

pframe$date=as.POSIXlt(pframe$date)
ndays=max(day)

pframe$dechour=pframe$date$hour+pframe$date$min/60+pframe$date$sec/60
pframe$yp=abs(pframe$day-ndays+1)

if (!is.null(latitude) ) photo=TRUE

# Store photoperiod in pframe 
if (photo==TRUE){
  pp=photoperiod(pframe$date$yday+1,latitude,longitude,zone,twilight)
  pframe$up=pp$rise
  pframe$down=pp$set
}

#If doubleplot: prepare pframe
if (dble==TRUE){
  new=subset(pframe,(day==1))
  new$yp=new$yp-1 
 
  for (i in 2:(ndays-1)){
    sect=subset(pframe,day==i)
    sectR=sect
    sectR$dechour=sect$dechour+24
    new=rbind(new,sectR)
    sect$yp=sect$yp-1
    if (i != ndays-1)  new=rbind(new,sect)
  }
  pframe=new
}

if (is.null(scalefac)) scalefac=.5
if (is.null (subtract)) subtract=0


#Scale data 0-1
mini=quantile(pframe$plotvar,probs=0.01,na.rm=TRUE)
pframe$plotvar=pframe$plotvar-mini
maxi=quantile(pframe$plotvar,probs=0.99,na.rm=TRUE)
pframe$plotvar=pframe$plotvar/(maxi-mini)
pframe$y=pframe$yp+(pframe$plotvar)*scalefac
delta=median(diff(pframe$dechour),na.rm=TRUE)#



lim=max(pframe$date)
          
# Make plot with rectangles
if (grad == TRUE) {
  p=ggplot(data=pframe,aes(xmin=dechour,xmax=dechour+delta,ymin=yp,ymax=y,color=y-yp))+geom_rect() 
} else {
 p=ggplot(data=pframe,aes(xmin=dechour,xmax=dechour+delta,ymin=yp,ymax=y))+geom_rect(colour=border,fill=fill,linewidth=lwd)
}

# Plot sunrise/set
if (photo==TRUE){
  p=p+geom_segment(aes(x = up, y = yp, xend = up, yend = yp+1), colour = "skyblue",data = pframe)
  p=p+geom_segment(aes(x = down, y = yp, xend = down, yend = yp+1), colour = "blue3",data = pframe)
}

# X axes
if (dble==TRUE) {
  p=p+xlim(0,48)
  p=p+scale_x_continuous(breaks = seq(0,48,by=12),labels=c("00:00","12:00","24:00","12:00","24:00"),expand=c(0.01,0.01))
}else{ 
  p=p+xlim(0,24)
  p=p+scale_x_continuous(breaks = seq(0,24,by=6),labels=c("00:00","06:00","12:00","18:00","24:00"),expand=c(0.1,0.1))
}

#Y axes
stp=floor(ndays/5)
sq=seq(1,ndays-1,by=stp)
dd=substring(pframe$date,1,10)
labs=NULL
for (i in sq){
  ix=unique(which(i==pframe$yp+1))[1]
  labs=c(labs,dd[ix])
}


if(ndays>180) ofs=-3 else ofs=.5
p=p+scale_y_continuous(breaks=sq-ofs,labels=labs,expand=c(0,0))


if (grad==TRUE) p=p+scale_color_gradient(low="gray90", high="gray10")

p=p+xlab("")
p=p+ylab("")
p=p+theme_classic()
p=p+xlab("Time of Day")
p=p+theme(axis.text.y   = element_text(size=15),
          axis.text.x   = element_text (size=15),
          axis.title.x = element_text(margin=margin(t=10),size=18),
          panel.background = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.border = element_rect(colour = "black", fill=NA),
          plot.margin=unit(c(1,2,1,2),"cm")
)
p=p+theme(legend.position = "none")
print(p)
ix=which(focus$date > lim)
focus=focus[-ix,]
invisible(focus)
}

