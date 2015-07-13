pkgname <- "uwIntroStats"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('uwIntroStats')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("U")
### * U

flush(stderr()); flush(stdout())

### Name: U
### Title: Create a Transformed Variable
### Aliases: U
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
attach(mri)
# Create a spline based on absolute
U(ldl, type="lspline", knots=c(70, 100, 130, 160))
U(ldl, type="ls", knots=c(70,100,130,160))

# Create a spline based on change
U(ldl, type="ls", knots=c(70, 100, 130, 160), parameterization="change")

# Create a log transformed variable
U(age, type="log")

## Create a partial formula
U(ma=~male+age)




cleanEx()
nameEx("bplot")
### * bplot

flush(stderr()); flush(stdout())

### Name: bplot
### Title: Boxplot with Lowess Curves, Jittered Data, Overlaid Mean and
###   Standard Deviation, for an Arbitrary Number of Strata
### Aliases: bplot
### Keywords: ~kwd1 ~kwd2

### ** Examples

#- Read in and attach the data -#
fev <- read.table("http://courses.washington.edu/b511/Data/FEV1ClinTrial.dat", sep="")

#- Change the names of the fev data -#
names(fev) <- c("V1", "FEV", "SMOKE")
attach(fev)

#- Produce box plot with jittered data, sample mean, and sd -#
bplot(y=FEV, x=SMOKE, xlab="Smoking Group", ylab="FEV")



cleanEx()
nameEx("clusterStats")
### * clusterStats

flush(stderr()); flush(stdout())

### Name: clusterStats
### Title: Summary Measures within Clusters
### Aliases: clusterStats clusterStatsOld extract.tableStat
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Load required libraries
library(survival)

# Reading in a dataset
mtx <- read.table("http://www.emersonstatistics.com/datasets/mtxlabs.txt",header=TRUE)

# Generating average bilirubin for each subject
mbili <- clusterStats (mtx$bili, mtx$ptid, "mean")
descrip(mbili,strata=mtx$tx)

# Generating average bilirubin for each subject while taking study drug
mdrugbili <- clusterStats (mtx$bili, mtx$ptid, "mean", subset=mtx$ondrug==1)
descrip(mdrugbili,strata=mtx$tx)

# Reading in a dataset
audio <- read.csv("http://www.emersonstatistics.com/datasets/audio.csv",header=TRUE)

# Generating counts for each subject
counts <- clusterStats (audio$R4000, audio$Subject, "count")
table(counts,strata=audio$Dose)

# Generating average R4000 for each subject
mR4000 <- clusterStats (audio$R4000, audio$Subject, "mean")
descrip(mR4000,strata=audio$Dose)

# Generating average R4000 for each subject after visit 0
mtxR4000 <- clusterStats (audio$R4000, audio$Subject, "mean", subset=audio$Visit>0)
descrip(mtxR4000,strata=audio$Dose)




cleanEx()
nameEx("correlate")
### * correlate

flush(stderr()); flush(stdout())

### Name: correlate
### Title: Correlation
### Aliases: correlate print.uCorrelate
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Load required libraries
library(survival)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Estimated correlation matrix using all data, complete cases, or pairwise complete (the default)
with (mri, correlate(age,weight,ldl,use="everything"))
with (mri, correlate(age,weight,ldl,use="complete"))
with (mri, correlate(age,weight,ldl))

# Correlation matrices for each stratum
with (mri, correlate(age,weight,ldl,strata=male))

# Correlations grouped by variable
with (mri, correlate(age,weight,ldl,strata=male,byStratum=FALSE))

# Special formatting of inference for correlations within strata
with (mri, correlate(age,weight,ldl,strata=male,stat="@cor@ (@lo@, @hi@); P @p@; n= @n@"))

# Special formatting of inference for correlations grouped by variable
with (mri, correlate(age,weight,ldl,strata=male,stat="@cor@ (@lo@, @hi@); P @p@; n= @n@",
      byStratum=FALSE))



cleanEx()
nameEx("descrip")
### * descrip

flush(stderr()); flush(stdout())

### Name: descrip
### Title: Descriptive Statistics
### Aliases: descrip ifelse1 print.uDescriptives
### Keywords: ~kwd1 ~kwd2

### ** Examples

#- Load the data -#
clin.trial <- read.table("http://courses.washington.edu/b511/Data/FEV1ClinTrial.dat", 
                        header=TRUE, sep="")
attach(clin.trial)

#- Load libraries -#
library(survival)

#- Create the table -#
descrip(clin.trial)



cleanEx()
nameEx("dummy")
### * dummy

flush(stderr()); flush(stdout())

### Name: dummy
### Title: Create Dummy Variables
### Aliases: dummy
### Keywords: ~kwd1 ~kwd2

### ** Examples

  # Reading in a dataset
  mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
  attach(mri)
  # Create a dummy variable for race
  dummy(race)



cleanEx()
nameEx("lincom")
### * lincom

flush(stderr()); flush(stdout())

### Name: lincom
### Title: Tests of Linear Combinations of Regression Coefficients
### Aliases: lincom
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Loading required libraries
library(survival)
library(sandwich)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)
attach(mri)

# Linear regression of LDL on age (with robust SE by default)
testReg <- regress ("mean", ldl~age+stroke)

# Testing coefficient created by .5*age - stroke (the first 1 comes from including the intercept)
testC <- c(1, .5, -1)
lincom(testReg, testC)



cleanEx()
nameEx("lspline")
### * lspline

flush(stderr()); flush(stdout())

### Name: lspline
### Title: Create Linear Splines
### Aliases: lspline lsplineD
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
attach(mri)
# Create a spline based on absolute
lspline(ldl, c(70, 100, 130, 160))

# Create a spline based on change
lsplineD(ldl, c(70, 100, 130, 160))



cleanEx()
nameEx("oneSample")
### * oneSample

flush(stderr()); flush(stdout())

### Name: oneSample
### Title: One Sample Inferential Methods
### Aliases: oneSample binomInference.exactLR binomInference.exactTail
###   binomInference.halfP binomInference.jeffreys binomInference.wald
###   binomInference.cwald binomInference.score binomInference.cscore
###   binomInference.agresti KMinference.ident CIptKM CIefrKM CIhwKM
###   qSupBrnMotn qSupBrnBrdg print.uOneSample
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Load required libraries
library(survival)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Creating a Surv object to reflect time to death
mri$ttodth <- Surv(mri$obstime,mri$death)

# Reformatting an integer MMDDYY representation of date to be a Date object
mri$mridate <- as.Date(paste(trunc(mri$mridate/10000),trunc((mri$mridate %% 10000)/100),
mri$mridate %% 100,sep="/"),"%m/%d/%y")

# Inference about the mean LDL: a two sample t test that mean LDL is 135 mg/dl
oneSample ("mean", mri$ldl, null.hypothesis=125)

# Inference about the mean LDL: a one sample t test of a lesser alternative
# that mean LDL is 125 mg/dl
oneSample ("mean", mri$ldl, null.hypothesis=125, test.type="less")

# Inference about the mean LDL: a one sample t test of a greater alternative
# that mean LDL is 125 mg/dl
oneSample ("mean", mri$ldl, null.hypothesis=125, test.type="greater")

# Inference about the geometric mean LDL: a one sample t test of a greater
# alternative that geometric mean LDL is 125 mg/dl
oneSample ("geom", mri$ldl, null.hypothesis=125, test.type="greater")

# Inference about the proportion of subjects with LDL greater than 128: exact binomial
# inference that 50% of subjects have LDL greater than 128 mg/dl
oneSample ("prop", mri$ldl, null.hypothesis=0.5, above=128)
oneSample ("prop",mri$ldl>128, null.hypothesis=0.5)




cleanEx()
nameEx("polynomial")
### * polynomial

flush(stderr()); flush(stdout())

### Name: polynomial
### Title: Create Polynomials
### Aliases: polynomial
### Keywords: ~kwd1 ~kwd2

### ** Examples

  # Reading in a dataset
  mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE)
  attach(mri)
  # Create a polynomial on ldl
  polynomial(ldl, degree=3)



cleanEx()
nameEx("predict.uRegress")
### * predict.uRegress

flush(stderr()); flush(stdout())

### Name: predict.uRegress
### Title: Prediction Intervals for 'uRegress' objects
### Aliases: predict.uRegress predict
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Loading required libraries
library(survival)
library(sandwich)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)
attach(mri)

# Linear regression of LDL on age (with robust SE by default)
testReg <- regress ("mean", ldl~age)

# 95% Prediction Interval for age 50
predict(testReg)



cleanEx()
nameEx("regress")
### * regress

flush(stderr()); flush(stdout())

### Name: regress
### Title: General Regression for an Arbitrary Functional
### Aliases: regress fitted.uRegress print.augCoefficients print.uRegress
###   uLRtest uWaldtest termTraverse explode indentNames getLevels testList
###   pasteTwo processTerm addArgs pasteOn pasteOnSpline pastePair
###   movingSum myNext reFormatReg createCols checkNesting splitOnParen
###   reFormat equal
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Loading required libraries
library(survival)
library(sandwich)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Creating a Surv object to reflect time to death
mri$ttodth <- Surv(mri$obstime,mri$death)

# Attaching the mri dataset
attach(mri)

# Linear regression of atrophy on age
regress("mean", atrophy~age, data=mri)

## Linear regression of atrophy on male and race and their interaction, with a multiple-partial F-test on the race-age interaction
regress("mean", atrophy~ male + U(ra=~race*age), data=mri)

## Linear regression of atrophy on age, male, race (as a dummy variable), chf,
## and diabetes. There are two multiple partial F-tests and both are named
regress("mean", atrophy~age+male+U(rc=~dummy(race)+chf)+U(md=~male+diabetes), data=mri)

## Proportional hazards regression clustered on id (here it makes no difference because the ids are unique)
regress("hazard", ttodth~ldl, id=ptid, data=mri)




cleanEx()
nameEx("scatter")
### * scatter

flush(stderr()); flush(stdout())

### Name: scatter
### Title: Scatter Plot with Lowess Curves
### Aliases: scatter
### Keywords: ~kwd1 ~kwd2

### ** Examples

#- Read in data set -#
clin.trial <- read.table("http://courses.washington.edu/b511/Data/FEV1ClinTrial.dat", 
                        header=TRUE, sep="")
#- Set the names -#
names(clin.trial) <- c("Y0", "Y1", "T")
attach(clin.trial)

#- Create a scatterplot with lowess curves and least squares fitted regression lines -#
scatter(Y1, Y0, ylab="FEV1 At 24 Weeks", xlab="FEV1 at Baseline")

#- Create a scatterplot with lowess curves and least squares fitted regression lines -#
scatter(Y1, Y0, strata=T, ylab="FEV1 At 24 Weeks", xlab="FEV1 at Baseline")



cleanEx()
nameEx("tableStat")
### * tableStat

flush(stderr()); flush(stdout())

### Name: tableStat
### Title: Table of Stratified Descriptive Statistics
### Aliases: tableStat tableStat.default tableStat.do print.tableStat
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Load required libraries
library(survival)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Creating a Surv object to reflect time to death
mri$ttodth <- Surv(mri$obstime,mri$death)

# Reformatting an integer MMDDYY representation of date to be a Date object
mri$mridate <- as.Date(paste(trunc(mri$mridate/10000),trunc((mri$mridate %% 10000)/100),
mri$mridate %% 100,sep="/"),"%m/%d/%y")

# Cross tabulation of counts with sex and race strata
with (mri, tableStat (NULL, race, male, stat= "@count@ (r @row%@; c @col%@; t @tot%@)"))

# Cross tabulation of counts with sex, race, and coronary disease strata
# (Note row and column percentages are defined within the first two strata, while overall
# percentage considers all strata)
with (mri, tableStat (NULL, race, male, chd,
stat= "@count@ (r @row%@; c @col%@; t @tot%@)"))

# Description of time to death with appropriate quantiles
with (mri, tableStat(ttodth,probs=c(0.05,0.1,0.15,0.2),
stat="mean @mean@ (q05: @q@; q10: @q@; q15: @q@; q20: @q@; max: @max@)"))

# Description of mridate with mean, range stratified by race and sex
with (mri, tableStat(mridate, race, male,
stat="mean @mean@ (range @min@ - @max@)"))

# Stratified descriptive statistics with proportions
with (mri, tableStat(age,stat=">75: @p@; >85: @p@; [-Inf,75): @p@; [75,85): @p@; 
      [85,Inf): @p@"), above=c(75,85),lbetween=c(75,85))

# Descriptive statistics on a subset comprised of males
with (mri, tableStat(dsst,age,stroke,subset=male==1,
stat="@mean@ (@sd@; n= @count@/@missing@)"))




cleanEx()
nameEx("tabulate")
### * tabulate

flush(stderr()); flush(stdout())

### Name: tabulate
### Title: Table Variables, with Stratification and Statistical Tests
### Aliases: tabulate tabulate.do print.tabulate tabulate.default tabModel
### Keywords: ~kwd1 ~kwd2

### ** Examples

## load the necessary libraries
library(survival)
library(Exact)
library(plyr)

## read in the mri dataset 
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

## attach the mri dataset
attach(mri)

## create a table of stroke and race
tabulate(stroke, race)

## perform a chi-squared test of stroke vs race, display the count, row percentage, 
## and column percentage
tabulate(stroke, race, stat="@count@ @row%@ @col%@")

## perform chi-squared test, likelihood ratio test, and fisher's exact test 
## for stroke vs race
tabulate(stroke, race, tests=c("lrchisq", "fisher"))

## for diabetes vs male by race, perform chi-squared test, display 
## odds ratio/risk ratio, mantel-haenzsel, likelihood ratio chi-squared
tabulate(diabetes, male, race, dispRatios=TRUE, tests=c("lrchisq", "mh"))



cleanEx()
nameEx("ttest")
### * ttest

flush(stderr()); flush(stdout())

### Name: ttest
### Title: T-test with Improved Layout
### Aliases: ttest ttest.do plot.ttest print.ttest ttest.default
### Keywords: ~kwd1 ~kwd2

### ** Examples

#- Read in data set -#
psa <- read.table("http://www.emersonstatistics.com/datasets/psa.txt", header=TRUE)
attach(psa)

#- Perform t-test -#
ttest(pretxpsa, null.hypoth = 100, test.type = "greater", more.digits = 1)

#- Define new binary variable as indicator -#
#- of whether or not bss was worst possible -#
bssworst <- bss
bssworst[bss == 1] <- 0
bssworst[bss == 2] <- 0
bssworst[bss == 3] <- 1

#- Perform t-test allowing for unequal -#
#- variances between strata -#
ttest(pretxpsa, by = bssworst)

#- Perform matched t-test -#
ttest(pretxpsa, nadirpsa, matched = TRUE, conf.level = 99/100, more.digits = 1)




cleanEx()
nameEx("ttesti")
### * ttesti

flush(stderr()); flush(stdout())

### Name: ttesti
### Title: T-test Given Descriptive Statistics with Improved Layout
### Aliases: ttesti
### Keywords: ~kwd1 ~kwd2

### ** Examples

#- T-test given sample descriptives -#
ttesti(24, 175, 35, null.hyp=230)

#- Two-sample test -#
ttesti(10, -1.6, 1.5, 30, -.7, 2.1)




cleanEx()
nameEx("uResiduals")
### * uResiduals

flush(stderr()); flush(stdout())

### Name: uResiduals
### Title: Extract Residuals from uRegress objects
### Aliases: uResiduals
### Keywords: ~kwd1 ~kwd2

### ** Examples

# Load required libraries
library(survival)

# Reading in a dataset
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt",header=TRUE)

# Create a uRegress object, regressing ldl on age
ldlReg <- regress("mean", age~ldl, data=mri)

# Get the studentized residuals
uResiduals(ldlReg, "studentized")

# Get the jackknifed residuals
uResiduals(ldlReg, "jackknife")



cleanEx()
nameEx("wilcoxon")
### * wilcoxon

flush(stderr()); flush(stdout())

### Name: wilcoxon
### Title: Wilcoxon Signed Rank and Mann-Whitney-Wilcoxon Rank Sum Test
### Aliases: wilcoxon wilcoxon.do print.wilcoxon wilcoxon.default
### Keywords: ~kwd1 ~kwd2

### ** Examples

#- Create the data -#
cf <- c(1153, 1132, 1165, 1460, 1162, 1493, 1358, 1453, 1185, 1824, 1793, 1930, 2075)
healthy <- c(996, 1080, 1182, 1452, 1634, 1619, 1140, 1123, 1113, 1463, 1632, 1614, 1836)

#- Perform the test -#
wilcoxon(cf, healthy, paired=TRUE)

#- Perform the test -#
wilcoxon(cf, healthy, conf.int=TRUE)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
