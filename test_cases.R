## Test cases for regress() function
## Last updated 2015 06 04

## load the packages
library(Exact)
library(geepack)
library(plyr)
library(sandwich)
library(survival)
library(uwIntroStats)

data(mri)

## linear regression
#1
mod_1 <- regress("mean", atrophy~age, data=mri)
mod_1_wrap <- regress_wrapper("mean", atrophy ~ age, data = mri)
mod_1$coefficients[2] == lm(atrophy ~ age, data = mri)$coefficients[2]
mod_1_wrap$coefficients[2] == lm(atrophy ~ age, data = mri)$coefficients[2]
#2
mod_2 <- regress("mean", atrophy~age+male, data=mri)
mod_2_wrap <- regress_wrapper("mean", atrophy ~ age + male, data = mri)
mod_2$coefficients[2] == lm(atrophy ~ age + male, data = mri)$coefficients[2]
mod_2$coefficients[3] == lm(atrophy ~ age + male, data = mri)$coefficients[3]

mod_2_wrap$coefficients[2] == lm(atrophy ~ age + male, data = mri)$coefficients[2]
mod_2_wrap$coefficients[3] == lm(atrophy ~ age + male, data = mri)$coefficients[3]

#3
mod_3 <- regress("mean", atrophy~age*male, data=mri)
mod_3_wrap <- regress_wrapper("mean", atrophy~age*male, data=mri)
mod_3$coefficients[2] == lm(atrophy ~ age*male, data = mri)$coefficients[2]
mod_3$coefficients[3] == lm(atrophy ~ age*male, data = mri)$coefficients[3]
mod_3$coefficients[4] == lm(atrophy ~ age*male, data = mri)$coefficients[4]

mod_3_wrap$coefficients[2] == lm(atrophy ~ age*male, data = mri)$coefficients[2]
mod_3_wrap$coefficients[3] == lm(atrophy ~ age*male, data = mri)$coefficients[3]
mod_3_wrap$coefficients[4] == lm(atrophy ~ age*male, data = mri)$coefficients[4]

#4, with geometric mean
mod_4 <- regress("geometric mean", atrophy ~ age + male, data = mri)
mod_4_wrap <- regress_wrapper("geometric mean", atrophy ~ age + male, data = mri)
mod_4$coefficients[2] == lm(log(atrophy) ~ age + male, data = mri)$coefficients[2]
mod_4_wrap$coefficients[2] == lm(log(atrophy) ~ age + male, data = mri)$coefficients[2]

#5, with logistic regression
mod_5 <- regress("odds", chf ~ age + male, data = mri)
mod_5_wrap <- regress_wrapper("odds", chf ~ age + male, data = mri)
mod_5_lm <- glm(chf ~ age + male, data = mri, family = "binomial")
mod_5$coefficients[2] == mod_5_lm$coefficients[2]
mod_5_wrap$coefficients[2] == mod_5_lm$coefficients[2]

#6, with poisson regression
mod_6 <- regress("rate", chf ~ age + male, data = mri)
mod_6_wrap <- regress_wrapper("rate", chf ~ age + male, data = mri)
mod_6_lm <- glm(chf ~ age + male, data = mri, family = "poisson")
mod_6$coefficients[2] == mod_6_lm$coefficients[2]
mod_6_wrap$coefficients[2] == mod_6_lm$coefficients[2]

#7, with survival
mod_7 <- regress("hazard", Surv(obstime, death) ~ age + male, data = mri)
mod_7_wrap <- regress_wrapper("hazard", Surv(obstime, death) ~ age + male, data = mri)
mod_7_lm <- coxph(Surv(obstime, death) ~ age + male, data = mri)
mod_7$coefficients[2] == mod_7_lm$coefficients[2]
mod_7_wrap$coefficients[2] == mod_7_lm$coefficients[2]

#8, with repeated measurements


#4
regress("mean", atrophy~ male + U(ra=~race*age), data=mri)
test.4 <- regress("mean", atrophy~ male + U(ra=~race*age), data=mri)
## U function lincom
lincom(test.4, c(0,0,1,1,1))
#5
regress("mean", atrophy~age+male+U(~dummy(race)+chf), data=mri)
test.5 <- regress("mean", atrophy~age+male+U(~dummy(race)+chf), data=mri)
lincom(test.5, c(0,0,0,1,1,1,1))
#6
regress("mean", atrophy~age+male+U(~dummy(race)*chf), data=mri)
test.6 <- regress("mean", atrophy~age+male+U(~dummy(race)*chf), data=mri)
lincom(test.6, c(0,0,0,1,1,1,1,1,1,1))
#7
regress("mean", atrophy~age+male+U(~dummy(race)+chf)+U(~male+diabetes), data=mri)
test.7 <- regress("mean", atrophy~age+male+U(~dummy(race)+chf)+U(~male+diabetes), data=mri)
lincom(test.7, c(0,0,0,1,1,1,1,0)) ## the first U
lincom(test.7, c(0,0,1,0,0,0,0,1)) ## the second U
#8
regress("mean", atrophy~age+male+U(rc=~dummy(race)+chf)+U(md=~male+diabetes), data=mri)
test.8 <- regress("mean", atrophy~age+male+U(rc=~dummy(race)+chf)+U(md=~male+diabetes), data=mri)
lincom(test.8, c(0,0,0,1,1,1,1,0)) ## the first U
lincom(test.8, c(0,0,1,0,0,0,0,1)) ## the second U
#9
regress("mean", atrophy~age+male+U(rc=~dummy(race)+chf)+U(mc=~male+dummy(chd)), data=mri)
test.9 <- regress("mean", atrophy~age+male+U(rc=~dummy(race)+chf)+U(mc=~male+dummy(chd)), data=mri)
lincom(test.9, c(0,0,0,1,1,1,1,0,0)) ## the first U
lincom(test.9, c(0,0,1,0,0,0,0,1,1)) ## second U
#10
regress("geometric mean", atrophy~age+male+U(~dummy(race)+chf), data=mri)
test.10 <- regress("geometric mean", atrophy~age+male+U(~dummy(race)+chf), data=mri)
lincom(test.10, c(0,0,0,1,1,1,1))
#11
regress("mean", atrophy~U(ma=~male*age)+U(mr=~male*dummy(race)), data=mri)
test.11 <- regress("mean", atrophy~U(ma=~male*age)+U(mr=~male*dummy(race)), data=mri)
lincom(test.11, c(0,1,1,1,0,0,0,0,0,0)) ## first U
lincom(test.11, c(0,0,0,0,1,1,1,1,1,1)) ## second U
#12
regress("mean", atrophy~lspline(age, knots=c(75,85)), data=mri)
test.12 <- regress("mean", atrophy~lspline(age, knots=c(75,85)), data=mri)
lincom(test.12, c(0,1,1,1))
## interaction between dummy variables
#13
regress("mean", atrophy~dummy(stroke)*dummy(chd), data=mri)
test.13 <- regress("mean", atrophy~dummy(stroke)*dummy(chd), data=mri)
lincom(test.13, c(0,1,1,0,0,0,0,0,0)) ## dummy(stroke)
lincom(test.13, c(0,0,0,1,1,0,0,0,0)) ## dummy(stroke)
lincom(test.13, c(0,0,0,0,0,1,1,1,1)) ## interaction

#14
regress("mean", atrophy~polynomial(ldl)*dummy(chd), data=mri)
test.14 <- regress("mean", atrophy~polynomial(ldl)*dummy(chd), data=mri)
lincom(test.14, c(0,1,1,0,0,0,0,0,0)) ## polynomial
lincom(test.14, c(0,0,0,1,1,0,0,0,0)) ## dummy
lincom(test.14, c(0,0,0,0,0,1,1,1,1)) ## interaction

## nesting the U function
#15
regress("mean", atrophy~U(grbg=~dummy(race)*U(x=~age+ldl)), data=mri)
test.15 <- regress("mean", atrophy~U(grbg=~dummy(race)*U(x=~age+ldl)), data=mri)
lincom(test.15, c(0,1,1,1,1,1,1,1,1,1,1,1)) ## grbg
lincom(test.15, c(0,1,1,1,0,0,0,0,0,0,0,0)) ## dummy(race)
lincom(test.15, c(0,0,0,0,1,1,0,0,0,0,0,0)) ## x
lincom(test.15, c(0,0,0,0,0,0,1,1,1,1,1,1)) ## interaction
lincom(test.15, c(0,0,0,0,0,0,1,1,1,0,0,0)) ## dummy-age interaction
lincom(test.15, c(0,0,0,0,0,0,0,0,0,1,1,1)) ## dummy-ldl interaction

#16
regress("mean", atrophy~U(grbg=~race+U(x=~age+ldl+U(y=~diabetes))), data=mri)
test.16 <- regress("mean", atrophy~U(grbg=~race+U(x=~age+ldl+U(y=~diabetes))), data=mri)
lincom(test.16, c(0,1,1,1,1)) ## grbg
lincom(test.16, c(0,0,1,1,1)) ## x
lincom(test.16, c(0,0,0,0,1)) ## y

## proportional hazards regression
mri$ttodth <- Surv(mri$obstime, mri$death)
regress("hazard", ttodth~ldl, data=mri)
regress("hazard", ttodth~ldl+cluster(ptid), data=mri) ## first alternate form
regress("hazard", ttodth~ldl, id=ptid, data=mri) ## second alternate form (aligned with other functionals)
regress("hazard", ttodth~polynomial(ldl, degree=3), data=mri)

salaryDat <- read.table("http://www.emersonstatistics.com/datasets/salary.txt", header=TRUE, stringsAsFactors=FALSE)
started90 <- salaryDat[salaryDat$startyr>=90,]
salaryNew <- started90[started90$startyr-started90$yrdeg <= 1,]
female <- ifelse(salaryNew$sex=="F", 1, 0)
regress("mean",salary~female+year+female*year, id=id, data=salaryNew)
regress("mean", salary~female+year+female*year, data=salaryNew)
regress("geom",salary~female+year+female*year, id=id, data=salaryNew)
regress("geom",salary~female+year+female*year, data=salaryNew)
## show the difference between the two 
