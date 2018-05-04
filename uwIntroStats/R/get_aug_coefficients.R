## function to process a fitted regression object and return the augmented coefficients matrix
get_aug_coefficients <- function(fit, fnctl, cl, mf, intercept, args, anyRepeated) {
  getn <- function(vec, n){
    if(n > length(vec)){
      return(vec)
    } else {
      return(vec[n:length(vec)])
    }
  }
  
  ## set up the output list
  out_lst <- list(call=cl, terms=NULL,firstPred=NULL,lastPred=NULL,preds=NULL,X=NULL)
  
  ## get the design matrix
  terms <- attr(fit$terms, "term.labels")
  if(fnctl=="hazard" & !is.null(attr(fit$terms, "specials")$cluster)){
    terms <- terms[-length(terms)]
  }
  if(fnctl=="hazard" & !is.null(attr(fit$terms, "specials")$cluster)){
    design <- model.matrix(fit, data)
  } else{
    design <- model.matrix(fit)
  }
  
  ## get the predictors (and intercept, if there)
  preds <- dimnames(design)[[2]]
  preds1 <- preds
  hyperpreds <- dimnames(fit$model)[[2]]
  
  ## check if there are any parens (dummy variables) or interactions
  parens <- grepl(")", preds, fixed=TRUE)
  interact <- grepl(":", preds, fixed=TRUE)
  
  ## get the predictors list
  prList <- reFormatReg(preds, hyperpreds, mf)
  preds <- prList$preds
  args <- prList$args
  
  ## set up the model matrix
  model <- fit$model
  dimnames(model)[[2]] <- preds
  
  ## get the names
  cols <- matrix(preds1, nrow=1)
  cols <- apply(cols, 2, createCols, terms)
  if(intercept) cols[1] <- "(Intercept)"
  
  names(fit$coefficients) <- preds
  
  p <- length(terms)
  for (i in 1:p) {
    out_lst <- processTerm (out_lst, model[,cols==terms[i]], terms[i])
  }
  tmp <- sapply(strsplit(out_lst$preds, ".", fixed=TRUE), getn, n=2)
  if(is.list(tmp)){
    tmp <- lapply(tmp, paste, collapse=".")
  } else if (is.matrix(tmp)) {
    tmp <- apply(tmp, 2, paste, collapse=".")
  } else {
    
  }
  out_lst$preds <- unlist(tmp)    
  out_lst$preds[parens[-1]] <- paste(" ", out_lst$preds[parens[-1]], sep="")
  
  ## add to the output list
  out_lst$X <- model
  out_lst_2 <- c(fit, out_lst, args)
  
  ## Getting the names and setting up the augmented coefficients matrix
  nms <- c(out_lst$terms[out_lst$firstPred!=out_lst$lastPred],out_lst$preds)
  ## first and last ones!
  fst <- c(out_lst$firstPred[out_lst$firstPred!=out_lst$lastPred],1:length(out_lst$preds))
  lst <- c(out_lst$lastPred[out_lst$firstPred!=out_lst$lastPred],1:length(out_lst$preds))
  u <- order(fst,-lst)
  nms <- c(ifelse1(intercept,"Intercept",NULL),nms[u])
  fst <- c(ifelse1(intercept,0,NULL),fst[u]) + intercept
  lst <- c(ifelse1(intercept,0,NULL),lst[u]) + intercept
  
  ## set up the column names
  cinames <- paste(format(100*conf.level),"%",c("L","H"),sep="")
  if (exponentiate) cinames <- paste("e(",c("Est",cinames),")",sep="")
  cinames <- c("Estimate",ifelse1(robustSE,c("Naive SE","Robust SE"),"Std Err"),cinames,
               ifelse1(useFdstn,c("F stat","Pr(>F)"),c("Chi2 stat","Pr(>Chi2)")))
  
  ## which column is the SEs?
  secol <- 2 + robustSE
  
  ## if clustered, need to set things up differently to match geepack
  if(anyRepeated){
    cinames2 <- paste(format(100*conf.level),"%",c("L","H"),sep="")
    if (exponentiate) cinames2 <- paste("e(",c("Est",cinames2),")",sep="")
    cinames2 <- c("Estimate", "Std Err",cinames2,"Wald", "Pr(>|W|)")
    cinames <- cinames2
    secol <- 1+robustSE
  }
  augCoefficients <- matrix(0,sum(out_lst$firstPred!=out_lst$lastPred)+length(out_lst$pred)+intercept,length(cinames))
  dimnames(augCoefficients) <- list(nms,cinames)
  
  ## get the summary object
  summ <- summary(fit)
  
  if (fnctl == "hazard") {
    converge <- NA
    n <- summ$n
    summ$coefficients <- summ$coefficients[,-2,drop=F]
    if (robustSE) {
      summ$robustCov <- fit$var
      summ$naiveCov <- fit$naive.var
      scoreStat <- fit$rscore
    } else {
      summ$naiveCov <- fit$var
      scoreStat <- fit$score
    }
    if(!anyRepeated & !robust){
      if (robustSE) {
        m <- sandwich(fit,adjust=T)
        summ$coefficients <- cbind(summ$coefficients[,1:2,drop=F],sqrt(diag(m)),summ$coefficients[,-(1:2),drop=F])
        dimnames(summ$coefficients)[[2]][2:3] <- c("Naive SE","Robust SE")
        summ$robustCov <- m
      }
    }
    p <- dim(summ$coefficients)[1]
    if (robustSE) m <- summ$robustCov else m <- summ$naiveCov
    summ$coefficients[,secol+1] <- summ$coefficients[,1] / summ$coefficients[,secol]
    u <- (1+intercept):p
    waldStat <- t(summ$coefficients[u,1]) %*% 
      (solve(m[u,u]) %*% summ$coefficients[u,1]) / (p - intercept)
    LRStat <- 2 * diff(fit$loglik)
  } else {
    if(anyRepeated){
      converge <- T
      summ$naiveCov <- NA
      summ$robustCov <- summ$cov.unscaled
      n <- sum(summ$df[1:2])
    } else if (fnctl %in% c("mean","geometric mean")) {
      converge <- T
      summ$naiveCov <-summ$sigma^2 * summ$cov.unscaled
      n <- sum(summ$df[1:2])
    } else {
      converge <- fit$converged
      summ$naiveCov <- summ$cov.scaled
      n <- summ$df.null + intercept
    }
    if(!anyRepeated){
      if (robustSE) {
        m <- sandwich(fit,adjust=T)
        summ$coefficients <- cbind(summ$coefficients[,1:2,drop=F],sqrt(diag(m)),summ$coefficients[,-(1:2),drop=F])
        dimnames(summ$coefficients)[[2]][2:3] <- c("Naive SE","Robust SE")
        summ$robustCov <- m
      }
    }
    p <- dim(summ$coefficients)[1]
    if (robustSE) m <- summ$robustCov else m <- summ$naiveCov
    summ$coefficients[,secol+1] <- summ$coefficients[,1] / summ$coefficients[,secol]
    u <- (1+intercept):p
    waldStat <- t(summ$coefficients[u,1]) %*% 
      (solve(m[u,u]) %*% summ$coefficients[u,1]) / (p - intercept)
    LRStat <- if (fnctl %in% c("mean","geometric mean")) 
      waldStat else (summ$null.deviance - summ$deviance) / (p - intercept) / summ$deviance * (n-p)
    scoreStat <- NULL
    
  }
  
  p <- dim(summ$coefficients)[1]
  
  if(!anyRepeated | fnctl == "hazard"){
    if (useFdstn) {
      waldStat <- c(waldStat,1-pf(waldStat,p-intercept,n-p),p-intercept,n-p)
      LRStat <- c(LRStat,1-pf(LRStat,p-intercept,n-p),p-intercept,n-p)
      if (!is.null(scoreStat)) scoreStat <- c(scoreStat,1-pf(scoreStat,p-intercept,n-p),p-intercept,n-p)
      summ$coefficients[,secol+2] <- 2 * pt(- abs(summ$coefficients[,secol+1]),df=n-p)
    } else {
      waldStat <- c(waldStat,1-pchisq(waldStat,p-intercept),p-intercept)
      LRStat <- c(LRStat,1-pchisq(LRStat,p-intercept),p-intercept)
      if (!is.null(scoreStat)) scoreStat <- c(scoreStat,1-pchisq(waldStat,p-intercept),p-intercept)
      summ$coefficients[,secol+2] <- 2 * pnorm(- abs(summ$coefficients[,secol+1]))
    }
  } 
  
  droppedPred <- is.na(fit$coefficients)
  if(anyRepeated){
    summ$coefficients <- as.matrix(summ$coefficients)
  }
  linearPredictor <- out_lst$X[, !droppedPred] %*% summ$coefficients[,1,drop=F]
  ## return what I need to
  
}