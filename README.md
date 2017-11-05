seeAI
================
Sondre U. Solstad

Visualizations of AI "thinking" in R
==========================

This package provides functions for visualizing AI / Machine Learning. Currently only one learning process is supported, namely LASSO regression with cross-validation. 

Written by Sondre U. Solstad, Princeton University (<ssolstad@princeton.edu>). Send me an email if you find this package useful or want to suggest an improvement or feature.

Installation instructions:

``` r
library(devtools)
install_github("sondreus/seeAI")
```


animate_glmnet:
--------

``` r
library(seeAI)
library(glmnet)

set.seed(1010)
n=1000;p=100
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta=rnorm(nzc)
fx= x[,seq(nzc)] %*% beta
eps=rnorm(n)*5
y=drop(fx+eps)
px=exp(fx)
px=px/(1+px)
ly=rbinom(n=length(px),prob=px,size=1)

set.seed(1011)
cvob1 <- cv.glmnet(x,y)

animate_glmnet(cvob1)

```

Arguments:
----------


-   **cv.glmnet** An object of class 'cv.glmnet'
-   **plot.cvm** Should cross-validation error be plotted? Defaults to TRUE.
-   **plot.cv.folds** Should cross-validation folds be plotted? Defaults to TRUE.
-   **total.time** Desired time of animation in seconds. Defaults to 10.
-   **new.save** Should this animation be saved as a new object rather than overwrite the preceeding animation? Defaults to TRUE.
-   **save.html** Save as HTML? Defaults to TRUE. If FALSE, saves GIF.
-   **debug** Only plot subset of lambda values? Defaults to FALSE.
-   **debug.n** If plotting subset of lambda values, sets number of values to plot. Defaults to 100. 


Explanation and detail
----------------------



Acknowledgements
----------------

This package relies on the *glmnet* package by Jerome Friedman, Trevor Hastie, Noah Simon and Rob Tibshirani, and the *animation* package by Yihui Xie.