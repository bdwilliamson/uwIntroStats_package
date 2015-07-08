## Intro To R accompanying code
library(Exact)
library(geepack)
library(plyr)
library(sandwich)
library(survival)
library(uwIntroStats)

## Descrip
clin.trial <- read.table("http://courses.washington.edu/b511/Data/FEV1ClinTrial.dat", sep="")
names(clin.trial) <- c("Y1", "Y0", "T")
attach(clin.trial)
descrip(clin.trial)
detach(clin.trial)
## Tabulate
mri <- read.table("http://www.emersonstatistics.com/datasets/mri.txt", header=TRUE, quote="\"")
attach(mri)
tabulate(stroke, race)
tabulate(stroke, race, stat="@count@ @row%@ @col%@")
tabulate(stroke, race, tests=c("lrchisq", "fisher"))
tabulate(diabetes, male, race, dispRatios=TRUE, tests=c("lrchisq", "mh"))
tabulate(diabetes, male, tests=c("fisher", "uWald", "uScore"))
tabulate(stroke, race, male, stat="@count@ @row%@ @col%@ @tot%@")
tabulate(stroke, race, male, stratified=FALSE, stat="@count@ @row%@ @col%@ @tot%@")

## Boxplot
fev <- read.table("http://courses.washington.edu/b511/Data/FEV1ClinTrial.dat", sep="")
names(fev) <- c("V1", "FEV", "SMOKE")
attach(fev)

bplot(y = FEV, x = SMOKE, xlab = "Smoking Group", ylab = "FEV")
detach(fev)
## Scatterplot
clin.trial <- read.table("http://courses.washington.edu/b511/Data/FEV1ClinTrial.dat", sep="")
names(clin.trial) <- c("Y1", "Y0", "T")
attach(clin.trial)

#- Create a scatterplot with lowess curves and least squares fitted regression lines -#
scatter(Y1, Y0, ylab="FEV1 At 24 Weeks", xlab="FEV1 at Baseline")
detach(clin.trial)
WCGS <- read.csv("E://Documents//UW//BIOST 514//WCGSdata.csv")
attach(WCGS)
ttest(weight, null.hypoth = 169)
ttest(sbp, by = dibpat)
detach(WCGS)

attach(mri)
regress("mean", stroke~race+male)

