scatter <-
function (y, x, strata=rep(1,length(y)), subset= rep(TRUE,length(y)), reference=sort(unique(strata)), 
                     plotPoints=TRUE, plotLowess=TRUE, plotLSfit=FALSE, legend=0.05,
                     colors=c("black", "blue", "orange", "pink", "green", "red", "cornflowerblue", "darkolivegreen", "magenta"), 
                     xJitter=TRUE, yJitter=FALSE, newplot=TRUE,lty=1:6, lwd=1, pch=1:25, ..., version=FALSE) {
  
  vrsn <- "20110928"
  if (version) return(vrsn)
  
  if (!is.null(strata)) {
    if (is.list(strata)) {
      for (i in 1:length(strata)) {
        if (!is.vector(strata[[i]])) stop("strata can only be a vector, matrix, or list of vectors")
      }
      n <- length(strata[[1]])
      if (length(strata) > 1) {
        for (i in 2:length(strata)) {
          if (length(strata[[i]]) != n) stop("all elements in strata must be same length")
        }
      }
      snms <- names(strata)
      if (is.null(snms)) snms <- rep("",length(strata))
      tmp <- paste(snms[1],format(strata[[1]]))
      if (length(strata) > 1) {
        for (i in 2:length(strata)) tmp <- paste(tmp,snms[i],format(strata[[i]]))
      }
    } else {
      strata <- as.matrix(strata)
      snms <- dimnames(strata)[[2]]
      if (is.null(snms)) snms <- rep("",dim(strata)[2])
      tmp <- paste(snms[1],format(strata[,1]))
      if (dim(strata)[2] > 1) {
        for (i in 2:(dim(strata)[2])) tmp <- paste(tmp,snms[i],format(strata[,i]))
      }
    }
    strata <- tmp
  }
  
  y <- y[subset]
  x <- x[subset]
  strata <- strata[subset]
  hyperNames <- as.character(names(match.call(expand.dots=F)$...))
  xlbl <- ylbl <- NULL
  if (!("xlab" %in% hyperNames)) {
    xlbl <- as.character(match.call(expand.dots=F)$x)
    if(xlbl[1] == "$"){
      xlbl <- paste0(xlbl[2], xlbl[1], xlbl[3])
    }
  }
  if (!("ylab" %in% hyperNames)) {
    ylbl <- as.character(match.call(expand.dots=F)$y)
    if(ylbl[1] == "$"){
      ylbl <- paste0(ylbl[2], ylbl[1], ylbl[3])
    }
  }
  u <- !is.na(x) & !is.na(y) & !is.na(strata)
  y <- y[u]
  x <- x[u]
  xj <- xJitter * min(diff(sort(unique(x)))) / 8
  yj <- yJitter * min(diff(sort(unique(y)))) / 8
  strata <- strata[u]
  xrng <- range(x)
  yrng <- range(y)
  if (length(reference) == 1) legend <- 0
  if (newplot) {
    if (is.null(xlbl)) {
      if (is.null(ylbl)) {
        plot(xrng + c(0,(legend > 0)*0.25*diff(xrng)),yrng,type="n",...)
      } else {
        plot(xrng + c(0,(legend > 0)*0.25*diff(xrng)),yrng,type="n",ylab=ylbl,...)
      }
    } else if (is.null(ylbl)) {
      plot(xrng + c(0,(legend > 0)*0.25*diff(xrng)),yrng,type="n",xlab=xlbl,...)
    } else plot(xrng + c(0,(legend > 0)*0.25*diff(xrng)),yrng,type="n",xlab=xlbl,ylab=ylbl,...)
  }
  for (i in 1:length(reference)) {
    u <- strata==reference[i]
    if (plotPoints) {
      if(!missing(pch)){
        points(x[u] + rnorm(sum(u))*xj,y[u] + rnorm(sum(u))*yj,col=colors[(i %% (length(colors))+1)],pch=pch)
      } else{
        points(x[u] + rnorm(sum(u))*xj,y[u] + rnorm(sum(u))*yj,col=colors[(i %% (length(colors))+1)],pch=(i %% 25))
      }
    }
    if (plotLowess){
      if(!missing(lty) & !missing(lwd)){
        lines(lowess(x[u],y[u]),lty=lty,lwd=lwd, col=colors[(i %% (length(colors))+1)])
      }  else if(!missing(lty)){
        lines(lowess(x[u],y[u]),lty=lty,col=colors[(i %% (length(colors))+1)])
      } else if(!missing(lwd)){
        lines(lowess(x[u],y[u]),lty=((i %% 6)+1),lwd=lwd, col=colors[(i %% (length(colors))+1)])
      } else{
        lines(lowess(x[u],y[u]),lty=((i %% 6)+1),col=colors[(i %% (length(colors))+1)])
      }
    } 
    if (plotLSfit) {
      z <- lm.fit(cbind(1,x[u]),y[u])$coeff
      if(missing(lty) & missing(lwd)){
        lines(xrng,z[1]+z[2]*xrng,lty=((i %% 6)+1),col=colors[(i %% (length(colors))+1)])
      } else if (!missing(lwd) & !missing(lty)){
        lines(xrng,z[1]+z[2]*xrng,lty=lty,lwd=lwd,col=colors[(i %% (length(colors))+1)])
      } else if(!missing(lty)){
        lines(xrng,z[1]+z[2]*xrng,lty=lty,col=colors[(i %% (length(colors))+1)])
      } else{
        lines(xrng,z[1]+z[2]*xrng,lty=((i %% 6)+1),lwd=lwd,col=colors[(i %% (length(colors))+1)])
      }
      
    }
    if (legend > 0) {
      if (plotLowess | plotLSfit) lines(xrng[2] + 2*xj + c(0.02,.1)*diff(xrng),
                                        rep(yrng[2] - i*legend*diff(yrng),2),lty=((i %% 6)+1),col=colors[(i %% (length(colors))+1)])
      points(xrng[2] + 2*xj + .06*diff(xrng), yrng[2] - i*legend*diff(yrng),pch=(i %% 25),col=colors[(i %% (length(colors))+1)])
      text(xrng[2] + 2*xj + .12*diff(xrng), yrng[2] - i*legend*diff(yrng), reference[i],adj=0)
    }
  }
  invisible(NULL)
}
