oneSample <-
function (fnctl,                          # the distribution functional to be compared
                       y,                              # the response variable in the regression analysis
                       null.hypothesis=NA, 
                       test.type="two.sided",
                       subset=rep(TRUE,N),
                       conf.level=0.95,
                       na.rm=TRUE,
                       probs= 0.5, 					  # for use with quantiles
                       replaceZeroes=NULL,			  # for use with geometric means
                       restriction=Inf, 			  # for use with means, geometric means of censored data
                       subjTime=rep(1,length(y)),	  # for use with rates
                       method= NULL,
                       above=NULL, below=NULL, 
                       labove=NULL, rbelow=NULL, 
                       interval=NULL, linterval=NULL, 
                       rinterval=NULL, 
                       lrinterval=NULL,				 	# for use with proportions of discretized continuous data
                       g1=1, g2=0, dispersion=1,	 	# for use with method=="mean-variance"
                       nbstrap=10000, resample="pairs", 
                       seed=0,							# for use with method=="bootstrap"
                       ..., version=FALSE) {
  
  #
  #  Some notes on order of precedence:
  #		If restriction is supplied, y is replaced by a restricted variable that is a "Surv" object only if is.Surv(y)
  #		If any of the argurments above, ..., lrinterval are supplied and !is.Surv(y), y is replaced by an indicator variable
  #		If more than one of the arguments above, ..., lrinterval are supplied, precedence is in the order given in the argument list (no warnings)
  #		If none of the arguments above, ..., lrinterval are supplied and fnctl=="proportion" or fnctl=="odds", default is above=0
  #		Methods that are implemented depend on fnctl and type of data
  #			If is.Surv(y)
  #				If fnctl=="mean" or fnctl=="geometric mean"
  #					"KM" (the default if is.Surv(y))
  #					The following methods can be used, but result in an error if any observations are censored prior to the restriction
  #						"t.test"
  #						"mean-variance" (which takes arguments g1, g2, and dispersion)
  #				If fnctl=="median" or fnctl=="quantile"
  #					"KM" (the default if is.Surv(y))
  #					"bootstrap" (which takes arguments nbstrap and resample=("pairs","independent"))
  #					The following methods can be used, but result in an error if any observations are censored prior to the restriction
  #						"sign"
  #			If fnctl=="proportion" or fnctl=="odds" and 
  #				If fnctl=="mean" or fnctl=="geometric mean"
  #					"t.test" (the default if !is.Surv(y))
  #				"mean-variance" (which takes arguments g1, g2, and dispersion)
  #				"bootstrap" (which takes arguments nbstrap)
  #				"KM" (the default if is.Surv(y))
  #			If fnctl=="median" or fnctl=="quantile"
  #				"sign" (the default if !is.Surv(y))
  #				"bootstrap" (which takes arguments nbstrap)
  #				"KM" (the default if is.Surv(y))
  #			If fnctl=="proportion" or fnctl=="odds" and 
  # can be a character string or a list of character strings named $test and $interval; choices depend on fnctl
  #			If fnctl=="mean"
  #				method can be "t.test" (the default); "bootstrap"
  #
  #	Output will include
  #		- sample size
  #		- functional name
  #		- estimate
  #		- confidence interval
  #		- p value
  #		- link function for analysis model
  #		- estimate on analysis model scale
  #		- standard error on analysis model scale (if relevant)
  
  vrsn <- "20121107"
  if (version) return(vrsn)
  
  KM <- function (x) {
    if (!is.Surv(x)) stop("x must be a Surv object")
    x <- x[!is.na(x)]
    obs <- x[,1]
    ev <- x[,2]
    ce <- 1 - ev
    if (length(obs) == 0) stop("No data to estimate survival curve")	
    N <- length (obs)
    if (!any(obs==0)) {
      obs <- c(0,obs)
      ev <- c(0,ev)
      ce <- c(0,ce)
    }
    i <- order (obs,1-ev)
    obs <- obs[i]
    ev <- ev[i]
    ce <- ce[i]
    ev <- rev (cumsum (rev (ev)))
    ce <- rev (cumsum (rev (ce)))
    n <- ce + ev
    i <- !duplicated (obs)
    obs <- obs[i]
    n <- n[i]
    ev <- ev[i]
    ev <- ev - c (ev[-1],0)
    ce <- ce[i]
    ce <- ce - c (ce[-1],0)
    v <- N * cumsum (ev / (n - ev) / n)
    S <- exp (cumsum (log (1 - ev / n)))
    if (is.na (S[length(S)])) S[length(S)] <- 0
    rslt <- data.frame (t=obs, atrisk=n, events=ev, censored=ce, S=S, v=v)
    class(rslt) <- c("KM","data.frame")
    rslt
  }
  
  sKM <- function (x, times, rightCtsCDF=T) {
    if (!inherits(x,"KM")) stop("x must be a KM object")
    if (rightCtsCDF) {
      rslt <- as.vector(apply(matrix(rep(times,each=length(x$t)),length(x$t)) >= x$t, 2, sum)) + 1
    } else rslt <- as.vector(apply(matrix(rep(times,each=length(x$t)),length(x$t)) > x$t, 2, sum)) + 1
    if (x$S[length(x$S)] > 0) rslt[times > x$t[length(x$t)]] <- NA
    c(1,x$S)[rslt]
  }
  
  pKM <- function (x, times, rightCtsCDF=T) {
    1 - sKM (x, times, rightCtsCDF)
  }
  
  qKM <- function (x, probs) {
    rslt <- length(probs)
    for (i in 1:length(probs)) {
      p <- 1 - probs[i]
      j <- abs(x$S - p) < 1e-15 & x$events > 0
      if (any(j)) {
        if (abs(p - min(x$S)) < 1e-15) {
          rslt[i] <- (x$t[j] + max(x$t)) / 2
        } else {
          rslt[i] <- (x$t[j] + min(x$t[x$t > x$t[j] & x$events > 0])) / 2
        }
      } else {
        j <- sum(x$S > p)
        if (j == length(x$S) | p == 1) {
          rslt[i] <- NA
        } else rslt[i] <- x$t[j+1]
      }
    }
    rslt
  }
  
  meanKM <- function (x, restriction=Inf) {
    if (length(restriction)==1) restriction <- c(x$t[1]-1,restriction)
    if (restriction[2]==Inf) restriction[2] <- x$t[length(x$t)]
    tms <- c(restriction[1],x$t[x$t > restriction[1] & x$t < restriction[2]],restriction[2])
    s <- sKM(x,restriction)
    s <- c(s[1],x$S[x$t > restriction[1] & x$t < restriction[2]],s[2])
    ne <- tms <= 0
    po <- tms >= 0
    neS <- 1 - s[ne]
    neX <- abs(c(diff(tms[ne]),0))
    neI <- neS != 0 & neX != 0
    if (sum(neI) > 0) rslt <- - sum(neS[neI] * neX[neI]) else rslt <- 0
    poS <- s[po]
    poX <- c(diff(tms[po]),0)
    poI <- poS != 0 & poX != 0
    if (sum(poI) > 0) rslt <- rslt + sum(poS[poI] * poX[poI])
    attr(rslt,"restriction") <- restriction
    rslt
  }
  
  chc.fnctl <- c("mean", "geometric mean", "proportion", "median", "quantile", "odds", "rate")
  name.fnctl <- c("Mean","GeomMn","Prop","Mdn",paste(format(100*probs[1]),"%ile",sep=""),"Odds","Rate")
  findx <-  pmatch(fnctl, chc.fnctl)
  if (is.na(findx)) stop("unsupported functional")
  fnctl <- chc.fnctl[findx]
  
  isSurv <- is.Surv(y)
  isDate <- inherits(y,"Date")
  isEvent <- is.logical(y) || all(y[!is.na(y)] %in% 0:1)
  if(!isSurv && !isDate &&!isEvent && !is.numeric(y)) stop("y is an unsupported data type")
  if(isSurv) N <- dim(y)[1]
  else N <- length(y)
  if(restriction < Inf) {
    if(isSurv) {
      u <- y[,1] > restriction
      y[u,1] <- restriction
      y[u,2] <- 0
    } else y[y > restriction] <- restriction
    rName <- paste("Summary measure restricted to", format(restriction))
  } else rName <- NULL
  if (na.rm) u <- !is.na(y)
  else u <- rep(TRUE,N)
  ya <- y[subset & u]
  if(length(ya)==0) stop("no data to analyze")
  if(isSurv) n <- dim(ya)[1]
  else n <- length(ya)
  if(!na.rm) nMsng <- NA
  else nMsng <- sum(subset & !u)
  
  if(isSurv) default.method <- "KM"
  else default.method <- c("t.test", "t.test", "exact", "sign", "sign", "exact", "mean-variance")[findx]
  chc.method <- c("t.test", "exact", "exactLR", "exactTail", "wald", "cwald", "score", "cscore", "agresti", "jeffreys", "sign", "bootstrap", "mean-variance", "KM")
  if(is.null(method)) method <- default.method
  mindx <- pmatch(method, chc.method)	
  if (is.na(mindx)) stop("unsupported method")
  method <- chc.method[mindx]
  
  thresholds <- NULL
  if (length(above)>0) thresholds <- rbind(thresholds,cbind(0,above,0,Inf))
  if (length(below)>0) thresholds <- rbind(thresholds,cbind(0,-Inf,0,below))
  if (length(labove)>0) thresholds <- rbind(thresholds,cbind(1,labove,0,Inf))
  if (length(rbelow)>0) thresholds <- rbind(thresholds,cbind(0,-Inf,1,rbelow))
  if (!is.null(interval)) {
    if (length(interval)==2) interval <- matrix(interval,ncol=2)
    if (dim(interval)[2]!=2) stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds,cbind(0,interval[,1],0,interval[,2]))
  }		
  if (!is.null(linterval)) {
    if (length(linterval)==2) linterval <- matrix(linterval,ncol=2)
    if (dim(linterval)[2]!=2) stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds,cbind(1,linterval[,1],0,linterval[,2]))
  }		
  if (!is.null(rinterval)) {
    if (length(rinterval)==2) rinterval <- matrix(rinterval,ncol=2)
    if (dim(rinterval)[2]!=2) stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds,cbind(0,rinterval[,1],1,rinterval[,2]))
  }		
  if (!is.null(lrinterval)) {
    if (length(lrinterval)==2) lrinterval <- matrix(lrinterval,ncol=2)
    if (dim(lrinterval)[2]!=2) stop("intervals must be specified in a 2 column matrix")
    thresholds <- rbind(thresholds,cbind(1,lrinterval[,1],1,lrinterval[,2]))
  }
  if (is.null(thresholds)) thresholds <- rbind(thresholds,cbind(0,0,0,Inf))
  thresholds <- thresholds[1,,drop=F]
  if(fnctl %in% c("proportion","odds")) {
    if(isEvent) fName <- ifelse(fnctl=="proportion","Pr(Event)","Odds(Event)")
    else fName <- paste(sep="",ifelse(fnctl=="proportion","Pr","Odds"),
                        ifelse(thresholds[,2] == -Inf,
                               paste(sep="",ifelse(thresholds[,3]==0,"<","<="),format(thresholds[,4])),
                               ifelse(thresholds[,4]==Inf,
                                      paste(sep="",ifelse(thresholds[,1]==0,">",">="),format(thresholds[,2])),
                                      paste(sep="",
                                            ifelse(thresholds[,1]==0,"(","["),format(thresholds[,2]),",",format(thresholds[,4]),
                                            ifelse(thresholds[,3]==0,")","]")))))
    if (!isSurv) ya <- ifelse1(thresholds[,1]==0, ya > thresholds[,2], ya >= thresholds[,2]) &
      ifelse1(thresholds[,3]==0, ya < thresholds[,4], ya <= thresholds[,4])
  } else fName <- name.fnctl[findx]
  
  chc.test <- c("greater","less","two.sided")
  tindx <- pmatch(test.type, chc.test)	
  if (is.na(tindx)) stop("unsupported test type")
  test.type <- chc.test[tindx]
  if (is.na(null.hypothesis)) {
    hName <- NULL
    tindx <- 0
  } else hName <- paste("Hypothesis test of", c("upper","lower","two-sided")[tindx], "alternative that", fName, c(">", "<", "<>")[tindx], format(null.hypothesis))
  
  chc.resample <- c("pairs","independent")
  if(!is.null(resample)) {
    rindx <- pmatch(resample, chc.method)	
    if (is.na(mindx)) stop("unsupported method")
    resample <- chc.resample[rindx]
  }
  
  nReplace <- NA
  
  if(isSurv) stop("KM based inference not yet implemented")
  else {
    if(fnctl=="mean") {
      if (method=="t.test") {
        mName <- "One sample t test"
        link <- "identity"
        etaName <- fName
        etaHat <- mean(ya)
        etaHatSE <- sqrt(var(ya) / n)
        etaNull <- null.hypothesis
        statistic <- (etaHat - etaNull) / etaHatSE
        df <- n - 1
        if(test.type=="less") p.value <- pt(statistic,df)
        else if(test.type=="greater") p.value <- 1 - pt(statistic,df)
        else if(test.type=="two.sided") p.value <- 2 * pt(-abs(statistic),df)
        else p.value <- NA
        etaCIlo <- qt((1-conf.level)/2,df) * etaHatSE
        etaCIhi <- etaHat - etaCIlo
        etaCIlo <- etaHat + etaCIlo
        thetaHat <- etaHat
        thetaCIlo <- etaCIlo
        thetaCIhi <- etaCIhi
        methodParams <- NULL
      } else stop(paste("method",method,"not yet implemented"))
    } else if(fnctl=="geometric mean") {
      if(!is.null(replaceZeroes)) {
        u <- ya == 0
        nReplace <- sum(u)
        if(is.logical(replaceZeroes)) replaceZeroes <- min(ya[!u])
        ya[u] <- replaceZeroes
      } else if(any(ya <=0)) stop("cannot compute geometric mean with nonpositive data")
      if (method=="t.test") {
        mName <- "One sample t test on log transformed data"
        link <- "log"
        etaName <- fName
        etaHat <- mean(log(ya))
        etaHatSE <- sqrt(var(log(ya)) / n)
        etaNull <- log(null.hypothesis)
        statistic <- (etaHat - etaNull) / etaHatSE
        df <- n - 1
        if(test.type=="less") p.value <- pt(statistic,df)
        else if(test.type=="greater") p.value <- 1 - pt(statistic,df)
        else if(test.type=="two.sided") p.value <- 2 * pt(-abs(statistic),df)
        else p.value <- NA
        etaCIlo <- qt((1-conf.level)/2,df) * etaHatSE
        etaCIhi <- etaHat - etaCIlo
        etaCIlo <- etaHat + etaCIlo
        thetaHat <- exp(etaHat)
        thetaCIlo <- exp(etaCIlo)
        thetaCIhi <- exp(etaCIhi)
        methodParams <- NULL
      } else stop(paste("method",method,"not yet implemented"))
    } else if(fnctl=="proportion" || fnctl=="odds") {
      if (method=="KM") stop("method KM not yet implemented")
      else {
        if (method=="exact" || method=="exactLR") {
          binomInference <- binomInference.exactLR
          mName <- "exact distribution"
          if (test.type=="two.sided") mName <- paste(mName,"(LR ordering)")
        } else if (method=="exactTail") {
          binomInference <- binomInference.exactTail
          mName <- "exact distribution"
          if (test.type=="two.sided") mName <- paste(mName,"(tail probability ordering)")
        } else if (method=="wald") {
          binomInference <- binomInference.wald
          mName <- "Wald statistic"
        } else if (method=="cwald") {
          binomInference <- binomInference.cwald
          mName <- "continuity corrected Wald statistic"
        } else if (method=="score") {
          binomInference <- binomInference.score
          mName <- "score statistic"
        } else if (method=="cscore") {
          binomInference <- binomInference.cscore
          mName <- "continuity corrected score statistic"
        } else if (method=="agresti") {
          binomInference <- binomInference.agresti
          mName <- "Agresti & Coull"
        } else if (method=="jeffreys") {
          binomInference <- binomInference.jeffreys
          mName <- "Jeffreys"
        } else stop(paste("method",method,"not yet implemented"))
        etaName <- fName
        z <- binomInference(y=sum(ya), n=length(ya), null.hypothesis=null.hypothesis, test.type=test.type, conf.level=conf.level)
        etaHat <- z[2]
        etaHatSE <- z[3]
        etaNull <- null.hypothesis
        statistic <- z[4]
        df <- NA
        p.value <- z[7]
        etaCIlo <- z[5]
        etaCIhi <- z[6]
        methodParams <- NULL
        if(fnctl=="proportion") {
          link <- "identity"
          mName <- paste("One sample inference for binomial proportions using", mName)
          thetaHat <- etaHat
          thetaCIlo <- etaCIlo
          thetaCIhi <- etaCIhi
        } else {
          link <- "logit"
          mName <- paste("One sample inference for binomial odds using", mName)
          thetaHat <- etaHat / (1 - etaHat)
          thetaCIlo <- etaCIlo / (1 - etaCIlo)
          thetaCIhi <- etaCIhi / (1 - etaCIhi)
        }
      }
    } else stop(paste("inference for", fnctl, "not yet implemented"))
  }
  
  Inference <- cbind(n, thetaHat, thetaCIlo, thetaCIhi, null.hypothesis, p.value)
  dimnames(Inference) <- list("",c("n", fName, paste(format(100*conf.level),"% CIlo",sep=""), paste(format(100*conf.level),"% CIhi",sep=""),
                                   "Null Hyp", c("P", "P hi", "P lo", "P two")[tindx+1]))
  attr(Inference,"fnctl") <- fnctl
  attr(Inference,"hName") <- hName
  attr(Inference,"fName") <- fName
  attr(Inference,"method") <- method
  attr(Inference,"mName") <- mName
  attr(Inference,"methodParams") <- methodParams
  attr(Inference,"link") <- link
  attr(Inference,"isSurv") <- isSurv
  attr(Inference,"isDate") <- isDate
  attr(Inference,"isEvent") <- isEvent
  attr(Inference,"restriction") <- restriction
  attr(Inference,"rName") <- rName
  attr(Inference,"replaceZeroes") <- replaceZeroes
  attr(Inference,"nReplace") <- nReplace
  attr(Inference,"thresholds") <- thresholds
  attr(Inference,"nMsng") <- nMsng
  attr(Inference,"test.type") <- test.type
  attr(Inference,"conf.level") <- conf.level
  Statistics <- cbind(n, etaHat, etaHatSE, etaNull, statistic, df)
  dimnames(Statistics) <- list("",c("n", etaName, "SE", "Null", "TestStat", "df"))
  attr(Statistics,"link") <- link
  rslt <- list(Inference=Inference, Statistics=Statistics)
  class(rslt) <- "uOneSample"
  rslt
}
