QuickReg
================
Sondre U. Solstad

Easy OLS with options in R
==========================

The QuickReg package and associated function provides an easy interface for linear regression in R. This includes the option to request robust and clustered standard errors (equivalent to STATA's ", robust" option), automatic labeling, an easy way to specify multiple regression specifications simultaneously, and a compact html or latex output (relying on the widely used "stargazer" package).

QuickReg also includes a method to speed up OLS computation. In particular, it offers the option to implement a "fixed effect" demeaning procedure which demeans a set of covariates and then shares this across multiple regression specifications. In tests (reported below), this reduces calculation time by more than 60 percent for analysis with a large number of fixed effects compared to base R. This relative performance gain is increasing in the number of specifications passed to the function simultaneously.

Written by Sondre U. Solstad, Princeton University (<ssolstad@princeton.edu>). Send me an email if you find this package useful or want to suggest an improvement or feature.

Installation instructions:

``` r
library(devtools)
install_github("sondreus/seeAI")
```

Example:
--------

``` r
library(seeAI)

# Loading data
mydata <- readRDS("3d_example.RDS")

# Use the QuickReg to produce a regression table     
```


Arguments:
----------

-   **data** - Data frame in which all model variables are located.
-   **iv.vars** - Vector of independent variable names in dataset (e.g. c("gdppc", "pop"))
-   **iv.vars.names** - *(Optional)* Vector of desired independent variable names in table output (e.g. c("GDP per capita", "Population")). Defaults to values in "iv.vars" if none provided.
-   **dv.vars** - Vector of dependent variable in dataset (e.g. c("democracy", "war"))
-   **dv.vars.names** - *(Optional)* Vector of desired dependent variable names in table output (e.g. c("Democracy (Boix-Rosato-Miller 2012)", "War (with at least 1000 battle deaths)")). Defaults to values in "dv.vars" if none provided.
-   **specifications** - *(Optional)* List of desired regression specifications (selections of independent variables). The list of regression specifications are applied to all dependent variables. E.g. list(c(1), c(1,2), c(2))).
-   **fixed.effects** - *(Optional)* Vector of desired fixed effect variable names in dataset (e.g. c("region", "year"))
-   **fixed.effects.names** - *(Optional)* Vector of desired fixed effects labels in table output (e.g. c("Region FE", "Year FE")). Defaults to values in "fixed.effects" if none provided.
-   **fixed.effects.specifications** - *(Optional)* List of desired fixed effect specifications (selections of independent variables). These specifications are applied in sequence from the first to last model. If the number of specifications is less than the number of models, all fixed effects are applied in the remaining columns by default. If none provided, defaults to all fixed effects in all models.
-   **robust.se** - *(Optional)* If TRUE, returns robust standard errors calculated using a sandwich estimator from the "sandwich" package. Defaults to FALSE (i.e. normal standard errors).
-   **cluster** - *(Optional)* Name of variable in dataset by which cluster-robust standard errors should be computed using the cluster.vcov command of the multiwayvcov package.
-   **cluster.names** - *(Optional)* Desired name or label of clustering variable to be reported in table output (e.g. "Country" yields a note on the bottom of the table reading "Country-Clustered Standard Errors in Parenthesis"). If cluster specified but no "cluster.names" provided, "Cluster-Robust Standard Errors in Parenthesis" is reported.
-   **table.title** - *(Optional)* Specifies the title of the table with regression output. Defaults to "QuickReg" plus the date and time of creation in parenthesis.
-   **out.name** - *(Optional)* Specifies the output file name. Defaults to "QuickReg.html".
-   **dynamic.out.name** - *(Optional)* If TRUE, adds date and time of creation in brackets between the out.name and the file extension (e.g. QuickReg (2017-04-05-14-01-27).html)
-   **html.only** - *(Optional)* If TRUE, no latex output produced (only HTML table). Defaults to FALSE.
-   **type** - *(Optional)* Specifies the type of table output that will be requested from Stargazer. Possible values are: "latex", "html", and "text". Defaults to "latex".
-   **silent** - *(Optional)* If TRUE, no messages are returned by the function. Defaults to FALSE.
-   **save.fits** - *(Optional)* If TRUE, saves fitted lm objects in a list by the name "QuickReg.fits" adding an integer if an object by this name already exists. Defaults to FALSE.
-   **demeaning.acceleration** - *(Optional)* If TRUE, attempts to speed up regression by the method of alternating projections. In particular, it utilizes the "demeanlist" function of the "lfe" package to create a matrix of all covariates demeaned by all fixed effects, and then fits the different regression specifications on this demeaned matrix. Time saved is increasing in the number of fixed effects, specifications and observations, and this method is slower when all these are low. If there are thousands of fixed effects and many specifications, time saved is potentially quite large. Note: Overrides fixed.effects.specifications, always including all variables specified in fixed.effects, and does not supply R-squared or other model statistics. Defaults to FALSE.
-   **...** - Various options passed to the stargazer function. See ?stargazer. E.g.: *digits* = integer of number of digits to be displayed, *font.size* = font size (e.g. "tiny") if output is latex (no font size is imposed by default), *style* = table style (see "?stargazer\_style\_list"), *omit.stat* = character vector of model statistics to be omitted from table output. 

Explanation and detail
----------------------

The QuickReg function is meant to provide a comprehensive and convenient linear regression interface in R. It has been designed with the objective of being intuitive and easy to use at default settings, but with enough options for advanced users. Most importantly, the function is meant to facilitate a smooth, quick and productive workflow.

QuickReg is designed to work seamlessly with knitr and Rmarkdown, and allows output to be requested from stargazer in "latex", "html", or "text" format.

To illustrate the use of QuickReg, consider a researcher considering the linear relationships between a few variables.

``` r
N <- 1000
mydata <- cbind.data.frame(rnorm(N), rnorm(N), rnorm(N), rnorm(N), rnorm(N), 
                           rep(seq(1:10), N/10), sample(1:10, N, replace = TRUE))
colnames(mydata) <- c("y", "alternative.y", "x1", "x2", "x3", "group1", "group2")
```

Let's fit a simple regression in base R:

Acknowledgements
----------------

This package relies on the glmnet package by Jerome Friedman, Trevor Hastie, Noah Simon and Rob Tibshirani.