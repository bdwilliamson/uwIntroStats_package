/* Read in the data, delete the first row */
infile ptid mridate age male race weight height packyrs yrsquit alcoh physact chf chd stroke diabetes genhlth ldl alb crt plt sbp aai fev dsst atrophy whgrd numinf volinf obstime death using "http://www.emersonstatistics.com/datasets/mri.txt"
drop in 1

/* Run the regressions from test_cases in R.
   Also run lincom tests for the cases with "U" functions
*/
* 1
regress atrophy age, robust
*2
regress atrophy age male, robust
*3
regress atrophy c.age##male, robust
*4 
regress atrophy male c.race##c.age, robust
testparm c.race##c.age
*5 This one has a U in it
regress atrophy age male i.race chf, robust
testparm i.race chf
*6
regress atrophy age male i.race##chf
testparm i.race##chf
*7
regress atrophy age male i.race chf male diabetes, robust
testparm i.race chf
testparm male diabetes
*8 is the same
*9 
regress atrophy age male i.race chf male i.chd, robust
testparm i.race chf
testparm male i.chd
*10 
gen logatr = log(atrophy)
regress logatr age male i.race chf, robust
testparm i.race chf
*11
regress male#c.age male#i.race, robust
testparm male#c.age
testparm male#i.race
*12
mkspline sage1 75 sage2 85 sage3 = age
regress atrophy sage*, robust
*13
regress atrophy i.stroke##i.chd, robust
*14
ldl2 = ldl^2
regress atrophy c.ldl##i.chd c.ldl2##i.chd, robust
*15
regress atrophy i.race##c.age i.race##c.ldl, robust
testparm i.race##c.age i.race##c.ldl
testparm c.age c.ldl
