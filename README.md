# uwIntroStats

As of April, 2021, this package has been deprecated and ported into [`rigr`](https://github.com/statdivlab/rigr). `rigr` will be actively maintained.

This is a package designed for introductory statistics students. It adds functionality to many R functions, in addition to streamlining output and implementing many STATA functions in R. Developed by Brian Williamson, Andrew Spieker, and Scott Emerson MD PhD at the University of Washington. 

The major changes in `uwIntroStats` are:

* Making all types of regression (linear, generalized, proportional hazards, and corelated data) available in one function
* Using robust standard error estimates (from the `sandwich` package) by default in regression and inference
* Printing output in a much more intuitive manner
* Upgrading the `boxplot` function to support stratification, the overlay of jittered data, and the overlay of mean and standard deviation lines
* Upgrading scatterplot functionality to support stratification, jittered data, and plotting loess and least squares lines
* Allowing the user to specify multiple-partial F-tests within a regression call
* Creating functions for descriptive statistics with flexibly formatted output

