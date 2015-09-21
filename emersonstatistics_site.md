---
output: html_document
---
# uwIntroStats

This is a package designed for introductory statistics students. It adds functionality to many R functions, in addition to streamlining output and implementing many STATA functions in R. Developed by Brian Williamson, Andrew Spieker, and Scott Emerson MD PhD at the University of Washington. 

The major changes in `uwIntroStats` are:

* Making all types or regression (linear, generalized, proportional hazards, and corelated data) available in one function
* Using robust standard error estimates (from the `sandwich` package) by default in regression and inference
* Printing output in a much more intuitive manner
* Upgrading the `boxplot` function to support stratification and the overlay of mean and standard deviation lines
* Upgrading scatterplot functionality to support stratification and plotting loess and least squares lines
* Allowing the user to specify multiple-partial F-tests within a regression call
* Creating functions for descriptive statistics with flexibly formatted output

This site hosts the current version of the package, which is 0.0.1, in .tar.gz form for [Mac or Linux](uwIntroStats_0.0.1.tar.gz) and in .zip form for [Windows](uwIntroStats_0.0.1.zip). This version was updated on 1 September, 2015. 

While package vignettes can be accessed via the `browseVignettes()` function in R, we also provide the vignettes here. We have one for [regression in `uwIntroStats`](regress_intro.html), one for [writing multiple-partial F-tests in regression](u_intro.html), and one showing the various [test cases](test_cases.html) that we have used in different documents.

We have written [*An Introduction to R*](IntroToR.pdf) aimed at students who wish to gain an introduction to coding in R, reading data in to a session, and the various ways that data is stored.

For those who have previously used STATA, we offer a [STATA-R translation document](stata_translation.html).

Last, we present a document outlining our philosophy and approach to analyzing a data set, along with examples and code, titled [Notes re: FEV](fevdoc.html). We analyze the FEV dataset, hosted on this website. This analysis shows the typical process that Scott goes through in a quarter teaching Applied Biostatistics at the University of Washington.