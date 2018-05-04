## A function to perform lm, glm, correlated data regression, and report multiple partial f-tests
## Args: fnctl         - the functional to calculate, e.g. "mean"
##       formula       - the formula to compute
##       data          - the data frame or matrix to use. if missing, assumed to be attached
##       intercept     - whether or not to include an intercept. no intercept for hazard regression
##       strata        - stratified regression
##       id            - id for correlated data regression
##       robustSE      - compute robustSE standard error estimates
##       conf.level    - confidence level for tests
##       exponentiate  - return a table with exponentiated estimates and confidence intervals
##       replaceZeroes - the value to replace zeroes in the data with
##       useFdstn      - use the f-distribution for tests
##       suppress      - suppress the printing of the raw model table
##       ...           - other arbitrary arguments to inner regression functions
##       version       - the version of the function
## Returns: a uRegress object, with a list of tables and tests and coefficients
regress_wrapper <- function(fnctl, formula, data, intercept = (fnctl != "hazard"), 
                            strata = rep(1, n), id = 1:n, ... ) {
  cl <- match.call()
  n <- length(y)
  if(missing(formula)){
    stop("You must enter a formula")
  }
  ## checking functional 
  findx <-  pmatch(fnctl,c("mean", "geometric mean", "odds", "rate", "hazard"))
  if (is.na(findx)) stop("unsupported functional")
  fnctl <- c("mean", "geometric mean", "odds", "rate", "hazard")[findx]
  
  ## stop if wrong conditions for hazard
  if (intercept & fnctl=="hazard") stop("hazard regression cannot include an intercept")
  
  ## get the family for glm if necessary
  glm <- FALSE
  if (fnctl %in% c("odds", "rate")) {
    glm <- TRUE
    family <- ifelse(fnctl == "odds", "binomial", "poisson")
  } else if (fnctl != "hazard") {
    family <- "gaussian"
  }
  
  ## if no intercept, remove it!
  if(!intercept){
    form <- deparse(formula)
    if(length(form) > 1) {
      form <- paste(form, collapse = "")
    }
    form <- paste(form, "-1")
    formula <- as.formula(form, env=.GlobalEnv)
  }
  
  ## put into regression functions; if we have clustered data,
  ## fit gee
  if(is.null(id)){
    id <- 1:n
  }
  anyRepeated <- any(table(id[!is.na(id)]) > 1)
  if (anyRepeated) {
    if(fnctl=="hazard"){
      fit <- coxph(formula, data=data, ...)
    } else if (fnctl %in% c("mean", "geometric mean")){
      if (fnctl=="geometric mean") {
        newy <- deparse(formula[[2]])
        newy <- paste("log(", newy, ")", sep="")
        form <- deparse(formula)
        form <- unlist(strsplit(form, "~", fixed=TRUE))[2]
        form <- paste(newy,"~", form, sep="")
        formula <- as.formula(form, env=.GlobalEnv)
      } 
      fit <-  geepack::geeglm(formula, family="gaussian",id=id, data=data,...)
    } else if (fnctl=="odds"){
      fit <- geepack::geeglm(formula, family="binomial",id=id, data=data,...)
    } else {
      fit <- geepack::geeglm(formula, family="poisson",id=id, data=data,...)
    }
  } else {
    if (glm) {
      fit <- glm(formula, family = family, data = data, ...)
    } else if (fnctl == "hazard") {
      fit <- coxph(formula, data = data, ...)
    } else {
      if (fnctl == "geometric mean") {
        newy <- deparse(formula[[2]])
        newy <- paste("log(", newy, ")", sep="")
        form <- deparse(formula)
        form <- unlist(strsplit(form, "~", fixed=TRUE))[2]
        form <- paste(newy,"~", form, sep="")
        formula <- as.formula(form, env=.GlobalEnv)
      }
      fit <- lm(formula, data = data, ...)
    }
  }
  
}