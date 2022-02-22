

theme_lsp=function (bs=18){
  theme_bw(base_size =bs,base_family="sans")+ theme(plot.margin = unit(c(.5,.5,.5,.5 ), "cm"))+
    theme( axis.text.y =element_text (colour="black", angle=90, hjust=0.5,size=14),
           axis.text.x = element_text(colour="black",size=14),
           axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
           axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
           panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

lsp <- function (x, times = NULL, from = NULL, to = NULL, type = c("frequency", "period"), ofac = 1, alpha = 0.01, normalize=c("standard","press"), plot = TRUE, ...) {   
    
    type <- match.arg(type)
    normalize<-match.arg(normalize)
    
    
    if (ofac != floor(ofac)) {
        ofac <- floor(ofac)
        warning("ofac coerced to integer")
    }
    if (ofac < 1) {
        ofac <- 1
        warning("ofac must be integer >=1. Set to 1")
    }
    
    
    if (!is.null(times)) {
        if (!is.vector(times)) 
            stop("no multivariate methods available")
        if (length(x) != length(times)) 
            stop("Length of data and times vector must be equal")
        names <- c(deparse(substitute(times)), deparse(substitute(x)))
    }
    
    if (is.null(times) && is.null(ncol(x))) {
        names <- c("Time", deparse(substitute(x)))
        times <- 1:length(x)
    }
    
    if (is.matrix(x) || is.data.frame(x)) {
        
        if (ncol(x) > 2) 
            stop("no multivariate methods available")
        
        if (ncol(x) == 2) {
            names <- colnames(x)
            times <- x[, 1]
            x <- x[, 2]
        }
    }
    
    times <- times[!is.na(x)]
    x <- x[!is.na(x)]
    
    nobs <- length(x)
    if (nobs < 2) 
        stop("time series must have at least two observations")
    
    times <- as.numeric(times)
    start <- min(times)
    end <- max(times)
    av.int <- mean(diff(times))
    
    o <- order(times)
    times <- times[o]
    x <- x[o]
    
    y <- cbind(times, x)
    colnames(y) <- names
    
    datanames <- colnames(y)
    t <- y[, 1]
    y <- y[, 2]
    
    
    n <- length(y)
    tspan <- t[n] - t[1]
    fr.d <- 1/tspan
    step <- 1/(tspan * ofac)
    
    if (type == "period") {
        hold <- from
        from <- to
        to <- hold
        if (!is.null(from)) 
            from <- 1/from
        if (!is.null(to)) 
            to <- 1/to
    }
    
    if (is.null(to)) {
        f.max <- floor(0.5 * n * ofac) * step
    } else {
        f.max <- to
    }
    
    freq <- seq(fr.d, f.max, by = step)
    if (!is.null(from)) 
        freq <- freq[freq >= from]
    n.out <- length(freq)
    if (n.out == 0) 
        stop("erroneous frequency range specified ")
    
    x <- t * 2 * pi
    y <- y - mean(y)
    if (normalize=="standard") {
        norm=1/sum(y^2)
    } else if (normalize=="press"){
        norm <- 1/(2 * var(y))
    } else {
        stop ("normalize must be 'standard' or 'press'")
    }    
    
    w <- 2 * pi * freq
    PN <- rep(0, n.out)
    for (i in 1:n.out) {
        wi <- w[i]
        tau <- 0.5 * atan2(sum(sin(wi * t)), sum(cos(wi * t)))/wi
        arg <- wi * (t - tau)
        cs <- cos(arg)
        sn <- sin(arg)
        A <- (sum(y * cs))^2
        B <- sum(cs * cs)
        C <- (sum(y * sn))^2
        D <- sum(sn * sn)
        PN[i] <- A/B + C/D
    }

    PN <- norm * PN

    
    PN.max <- max(PN)
    peak.freq <- freq[PN == PN.max]
    if (type == "period") 
        peak.at <- c(1/peak.freq, peak.freq) else peak.at <- c(peak.freq, 1/peak.freq)
    
    scanned <- if (type == "frequency") 
        freq else 1/freq
    if (type == "period") {
        scanned <- scanned[n.out:1]
        PN <- PN[n.out:1]
    }
    if (normalize=="press"){
        effm <- 2 * n.out/ofac
        level <- -log(1 - (1 - alpha)^(1/effm))
        exPN <- exp(-PN.max)
        p <- effm * exPN
        if (p > 0.01)  p <- 1 - (1 - exPN)^effm
    }
    
    if (normalize=="standard"){
      

        fmax<-max(freq)
        Z<-PN.max
        tm=t
 
        p<-pbaluev (Z,fmax,tm=t)
        level=fibsearch(levopt,0,1,alpha,fmax=fmax,tm=t)$xmin
      
    }
    

    sp.out <- list(normalize=normalize, scanned = scanned, power = PN, data = datanames, n = n,
                   type = type, ofac = ofac, n.out = n.out, alpha = alpha,
                   sig.level = level, peak = PN.max, peak.at = peak.at, p.value = p)
    
    class(sp.out) <- "lsp"
    
    if (plot) {
        plot(sp.out, ...)
        return(invisible(sp.out))
    } else {
        return(sp.out)}
}


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


summary.lsp <- function(object,...) {
    first <- object$type
    if (first == "frequency") {
        second <- "At period"
    } else {
        second <- "At frequency"
    }
    first <- paste("At ", first)
    from <- min(object$scanned)
    to <- max(object$scanned)
    
    Value <- c(object$data[[1]], object$data[[2]], object$n, object$type, object$ofac, from, to, object$n.out, object$peak, object$peak.at[[1]], object$peak.at[[2]], object$p.value,object$normalize)
    options(warn = -1)
    for (i in 1:length(Value)) {
        if (!is.na(as.numeric(Value[i]))) 
            Value[i] <- format(as.numeric(Value[i]), digits = 5)
    }
    options(warn = 0)
    nmes <- c("Time", "Data", "n", "Type", "Oversampling", "From", "To", "# frequencies", "PNmax", first, second, "P-value (PNmax)", "normalized")
    report <- data.frame(Value, row.names = nmes)
    report
}



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

summary.randlsp <- function(object,...) {
    first <- object$type
    if (first == "frequency") {
        second <- "At period"
    } else {
        second <- "At frequency"
    }
    first <- paste("At ", first)
    from <- min(object$scanned)
    to <- max(object$scanned)
    
    Value <- c(object$data[[1]], object$data[[2]], object$n, object$type, object$ofac, from, to, length(object$scanned), object$peak, object$peak.at[[1]], object$peak.at[[2]], object$repeats,object$p.value)
    options(warn = -1)
    for (i in 1:length(Value)) {
        if (!is.na(as.numeric(Value[i]))) 
            Value[i] <- format(as.numeric(Value[i]), digits = 5)
    }
    options(warn = 0)
    nmes <- c("Time", "Data", "n", "Type", "Oversampling", "From", "To", "# frequencies", "PNmax", first, second, "Repeats","P-value (PNmax)")
    report <- data.frame(Value, row.names = nmes)
    report
}

ggamma <- function(N){
    return (sqrt(2 / N) * exp(lgamma(N / 2) - lgamma((N - 1) / 2)))
}

pbaluev <- function(Z,fmax,tm) {
  #code copied from astropy timeseries
    N=length(tm)
    Dt=mean(tm^2)-mean(tm)^2
    NH=N-1
    NK=N-3
    fsingle=(1 - Z) ^ (0.5 * NK)
    Teff = sqrt(4 * pi * Dt) # Effective baseline
    W = fmax * Teff
    tau=ggamma(NH) * W * (1 - Z) ^ (0.5 * (NK - 1))*sqrt(0.5 * NH * Z)
    p=-(exp(-tau)-1) + fsingle * exp(-tau)
    return(p)
}


levopt<-  function(x,alpha,fmax,tm){
    prob=pbaluev(x,fmax,tm)
    (log(prob)-log(alpha))^2
}

pershow=function(object){
  datn=data.frame(period=object$scanned,power=object$power)
  plot_ly(data=datn,type="scatter",mode="lines+markers",linetype="solid",
          x=~period,y=~power)
}


getpeaks=function (object,npeaks=5,plotit=TRUE){
  
  pks=findpeaks(object$power,npeaks=npeaks,minpeakheight=0,sortstr=TRUE) 
  peaks=pks[,1] 
  tmes=object$scanned[pks[,2]]
  tme=round(tmes,2)
  
  p=plot.lsp(object)
  p=p+ylim(0,peaks[1]*1.2)
  for (i in 1:npeaks){
    p=p+annotate("text", label=paste(tme[i]),y=peaks[i],x=tme[i],color="red",angle=45,size=6,vjust=-1,hjust=-.1)
  }

  d=data.frame(time=tme,peaks=peaks)
  result=list(data=d,plot=p)
  return(result)
}