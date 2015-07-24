/* This file is meant to accompany the STATA-R Translations document
   created by Brian Williamson and Scott Emerson, MD PhD at the University 
   of Washington. This contains all of the STATA code used in that document. 
   Created 23 July 2015.
*/
   
* First load the mri data *
infile ptid mridate age male race weight height packyrs yrsquit alcoh physact chf chd stroke diabetes genhlth ldl alb crt plt sbp aai fev dsst atrophy whgrd numinf volinf obstime death using "http://www.emersonstatistics.com/datasets/mri.txt"

* Now drop the first row of NAs due to the headers in the file *
drop in 1

/* Now call summarize. Note that this will print all variables, 
but we have not included them all in the document */
summarize

* Create a table based on a Case-Control setup (calculate Odds Ratio) *
cc male diabetes

* Create a table based on a Cohort Study setup (calculate Risk Ratio) *
cs male diabetes

* Clear the old data* 
clear
* Read in the FEV clinical trial data *
use "https://courses.washington.edu/b511/Data/FEV1ClinTrial.dta"

* Rename the variables *
rename Y0 FEV1base
rename Y1 FEV1wk24
rename T smoke

* Make the boxplot *
graph box FEV1base, by(smoke) ytitle(FEV)

* Make the scatterplot *
scatter FEV1wk24 FEV1base || lfit FEV1wk24 FEV1base || lowess FEV1wk24 FEV1base

* Compute the correlation matrix *
correlate FEV1wk24 FEV1base

* Display probabilities *
display 1-normprob(1)

* Reuse the mri data *
clear
infile ptid mridate age male race weight height packyrs yrsquit alcoh physact chf chd stroke diabetes genhlth ldl alb crt plt sbp aai fev dsst atrophy whgrd numinf volinf obstime death using "http://www.emersonstatistics.com/datasets/mri.txt"
drop in 1

* Run a t-test to test that the mean atrophy score is 30 *
ttest atrophy == 30

* Two sample t-test *
ttest atrophy, by(male)

* Linear Regression of atrophy on age *
regress atrophy age

* Logistic regression of diabetes on age *
glm diabetes age, family(binomial) link(log)

* Post-estimation, test if the coefficients for age and male sum to one *
regress atrophy age male
lincom age+male

* Predictions *
predict preds

* Display the first 10 *
list preds in 1/10

