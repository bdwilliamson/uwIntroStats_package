## Lectures 13-16, 514 Autumn 2014

##FEV data
FEVdata <- read.csv("P:/514 HW/FEVdata.csv")
attach(FEVdata)

smoke <- ifelse(SMOKE==2, 0, 1)
sex <- SEX

## ttest for difference in means
height <- HEIGHT
ttest(height, by=sex)

## difference of proportions
is_smoker <- smoke==1
successMale <- smoke==1 & sex==1
table(successMale)
prop1 <- 26
obs1 <- 336
successFemale <- smoke==1 & sex==2
prop2 <- 39
obs2 <- 318
prop.test(c(prop1, prop2), c(obs1, obs2), correct=FALSE)

## one sample t test
ttesti(25, 46, 220, null.hypoth=211)

## two sample t-test
detach(FEVdata)
WCGSdata <- read.csv("P:/514 HW/WCGSdata.csv")\
attach(WCGSdata)
ttest(sbp, by=dibpat)

## shoulder pain, paired ttest
detach(WCGSdata)
ShoulderPainData <- read.csv("P:/514 HW/ShoulderPainData.csv")
attach(ShoulderPainData)
ttest(pain[time==1], pain[time==6], matched=TRUE)
detach(ShoulderPainData)

## chisquared
attach(psa)
tabulate(grade, bss)

## survival
relapse <- ifelse(inrem=="no", 1, 0)
atrisk <- Surv(obstime, relapse)
survFit <- survfit(atrisk~1)
plot(survFit)
survdiff(atrisk~1)
