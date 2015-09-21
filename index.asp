
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<% response.expires=-1 %>
<html>
<head>
<title>Emerson Statistics | R information </title>
<meta http-equiv="expires" content="-1" />
<link rel="stylesheet" type="text/css" href="../../../ESmain.css" />
<style type="text/css">
<!--
a,p,td {font-size:11;text-indent:0px;font-family:verdana,arial,sans-serif;}
th {width:130px;text-align:left;font-size:11;}
-->
</style>

</head>
<body>
<!-- Begin Main Content Pane -->

<div style="background-color:#a0a0ff;width:800px;padding-left:40px;padding-top:20px;">
<h4>Emerson Statistics: R information</h4>
</div>
<a href="http://www.emersonstatistics.com/index.asp">Emerson Statistics Home</a>
&nbsp;| <a href="http://www.emersonstatistics.com/courses/formal/index.asp">University of Washington Courses</a>
&nbsp;| <a href="http://www.RCTdesign.org">RCTdesign.org</a>
&nbsp;| <a href="http://emersonstatistics.com/GeneralMaterials/R/index.asp"> uwIntroStats Package </a>
<br />

<p style="text-indent:0.0in;font-size:9pt;margin-left:120px;">
<b>Scott S. Emerson, M.D., Ph.D.</b>,
Professor of Biostatistics,
Department of Biostatistics,
University of Washington<br />
<b>Brian D. Williamson</b>,
Graduate Student,
Department of Biostatistics,
University of Washington<br />
<b>Andrew J. Spieker</b>,
Graduate Student,
Department of Biostatistics,
University of Washington<br />
</p>

<p style="text-indent:0.0in;font-size:9pt;margin-left:0px;width:800px;"> 
This page provides introductory information on the use of R in an introductory applied statistics class.
In particular, it provides:
<ul>
<li> An R package for introductory statistics (<b>uwIntroStats</b>),
<li> An introduction to R (and the above package),
<li> Some annotated examples of the use of specific functions from uwIntroStats,
<li> Some video tutorials on the use of R to perform basic statistical analyses, and
<li> A rough translation between Stata commands and R commands.
</ul> 
</p>

<h1>uwIntroStats</h1>
<p>This is a package designed for introductory statistics students. It adds functionality to many R functions, in addition to streamlining
 output and implementing many STATA functions in R. The package grew out of a desire for students in the introductory biostatistics courses at the
University of Washington to learn statistical analysis techniques using R. We believe that the output given by the package is more germane to 
the problems most introductory students will face than that in the base R functions, or in many other packages. 
</p>
<p>The major changes in <code>uwIntroStats</code> are:</p>
<ul>
<li>Making all types of regression (linear, generalized, proportional hazards, and corelated data) available in one function</li>
<li>Using robust standard error estimates (from the <code>sandwich</code> package) by default in regression and inference</li>
<li>Printing output in a much more intuitive manner</li>
<li>Upgrading the <code>boxplot</code> function to support stratification and the overlay of mean and standard deviation lines</li>
<li>Upgrading scatterplot functionality to support stratification and plotting loess and least squares lines</li>
<li>Allowing the user to specify multiple-partial F-tests within a regression call</li>
<li>Creating functions for descriptive statistics with flexibly formatted output</li>
</ul>
<p>This site hosts the current version of the package, which is 0.0.1, in .tar.gz form for <a href="uwIntroStats_0.0.1.tar.gz">Mac or Linux</a> and in .zip form for <a href="uwIntroStats_0.0.1.zip">Windows</a>. This version was updated on 1 September, 2015.</p>
<p>While package vignettes can be accessed using the <code>browseVignettes()</code> function in R, we also provide the vignettes here. We have one for <a href="regress_intro.html">regression in <code>uwIntroStats</code></a>, one for <a href="u_intro.html">writing multiple-partial F-tests in regression</a>, and one showing the various <a href="test_cases.html">test cases</a> that we have used in different documents.</p>
<p>We have written <a href="IntroToR.pdf"><em>An Introduction to R</em></a>, aimed at students who wish to gain an introduction to coding in R, reading data in to a session, and the various ways that data is stored.</p>
<p>For those who have previously used STATA, we offer a <a href="stata_translation.html">STATA-R translation document</a>.</p>
<p>Last, we present a document outlining our philosophy and approach to analyzing a data set, along with examples and code, titled <a href="fevdoc.html">Notes re: FEV</a>. We analyze the FEV dataset, hosted on this website under the <a href="http://www.emersonstatistics.com/Datasets/index.asp">datasets</a> page. 
This analysis shows the typical process that Scott goes through in a quarter teaching Applied Biostatistics at the University of Washington.
</p>
<p>
All of the documents are presented again below. We also have some video tutorials on installing R and RStudio for Windows and Mac OS, using graphical user interfaces with R,
the R workspace and data frames, and an introductory video to the <code>uwIntroStats</code> package.
</p>

<table>
<tr> <td><b>R package</b></td>
     <td>&nbsp;</td>
     <td> uwIntroStats </td>
     <td>&nbsp;</td>
     <td> <a href="http://www.emersonstatistics.com/GeneralMaterials/R/uwIntroStats_0.0.1.zip"> Windows (.zip) </a></td>
     <td>&nbsp;</td>
     <td> <a href="http://www.emersonstatistics.com/GeneralMaterials/R/uwIntroStats_0.0.1.tar.gz"> Mac OS and Linux (.tar.gz) </a></td>
     </tr>
<tr> <td> &nbsp; </td> </tr
<tr> <td><b>Written Materials</b></td>
     <td>&nbsp;</td>
     <td> An Introduction to R</td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/IntroToR.pdf"> (pdf) </a></td>
<td>&nbsp;</td>
</tr>
<tr> <td>&nbsp;</td>
     <td>&nbsp;</td>
     <td> Approach to Analyzing a dataset (FEV) </td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/fevdoc.html"> (html) </a></td> 
     <td>&nbsp;</td>
     <td>&nbsp;</td>
</tr>
<tr>
     <td><b> Package Vignettes: </b></td>
     <td>&nbsp;</td>
     <td> Regression in uwIntroStats </td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/regress_intro.html"> (html) </a></td>
</tr>
<tr>  <td>&nbsp;</td>
     <td>&nbsp;</td>   
     <td> Multiple-Partial F-tests in Regression </td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/u_intro.html"> (html) </a></td>
</tr>
<tr>  <td>&nbsp;</td>
     <td>&nbsp;</td>
     <td> Test Cases </td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/test_cases.html"> (html) </a></td>
     </tr>
<tr> <td>&nbsp;</td>
     <td>&nbsp;</td>
     <td> Table of Useful STATA commands and R functions</td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/table.pdf"> (pdf) </a></td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/analysis.pdf"></a></td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/stata_translations.html"> (html) </a></td>
     </tr>
<tr> <td> &nbsp; </td> </tr
<tr> <td><b>Video Tutorials</b></td>
     <td>&nbsp;</td>
     <td> Installing R on Windows</td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/Videos/WindowsDownload/WindowsDownload.html"> Video </a></td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/analysis.pdf"></a></td>
     </tr>
<tr> <td>&nbsp;</td>
     <td>&nbsp;</td>
     <td> Installing R on Mac OS</td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/VideoFiles/R/b536L06-2014-10-14a/b518L06-2014-10-14a.html"></a> Video </td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/analysis.pdf"></a></td>
     </tr>
<tr> <td>&nbsp;</td>
     <td>&nbsp;</td>
     <td> RGUI and RStudio</td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/Videos/GUI-RStudio/GUI-RStudio.html"> Video </a></td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/analysis.pdf"></a></td>
     </tr>
<tr> <td>&nbsp;</td>
     <td>&nbsp;</td>
     <td> The Workspace and Data Frames</td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/Videos/WorkspaceDataframe/WorkspaceDataframe.html"> Video </a></td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/analysis.pdf"></a></td>
     </tr>
<tr> <td>&nbsp;</td>
     <td>&nbsp;</td>
     <td> The uwIntroStats package</td>
     <td>&nbsp;</td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/Videos/uwIntroStats/uwIntroStats.html"> Video </a></td>
     <td><a href="http://www.emersonstatistics.com/GeneralMaterials/R/analysis.pdf"></a></td>
     </tr>
</table>
<p>&nbsp;</p>
</p>

</body>
</html>
