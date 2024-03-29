% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animate_glmnet.R
\name{animate_glmnet}
\alias{animate_glmnet}
\alias{seeAI}
\title{Animate glmnet}
\usage{
animate_glmnet(cv.glmnet, replay = FALSE, plot.cv = TRUE,
  total.time = ifelse(plot.cv, 15, 10), new.save = TRUE, save = "html",
  debug = FALSE, debug.n = 10, captions = FALSE, alt.captions = FALSE,
  transition.n = 10, ...)
}
\arguments{
\item{cv.glmnet}{An object of class 'cv.glmnet'}

\item{replay}{Should the animation be replayed in the visual device? Defaults to FALSE.}

\item{plot.cv}{Should cross-validation be plotted? Defaults to TRUE.}

\item{total.time}{Desired time of animation in seconds. Defaults to 15 if plot.cv selected, else 10.}

\item{new.save}{Should this animation be saved as a new object rather than overwrite the preceeding animation? Defaults to TRUE.}

\item{save}{Save as "html" or "gif"? Defaults to "html".}

\item{debug}{Only plot subset of lambda values? Defaults to FALSE.}

\item{debug.n}{If plotting subset of lambda values, sets number of values to plot. Defaults to 10.}

\item{captions}{Should captions be added to animation? Defaults to FALSE.}

\item{transition.n}{How many frames should be used to transition between cross-validation and model fit? Defaults to 10.}

\item{...}{Options passed to saveHTML or saveGIF functions. See ?animate::saveHTML and ?animate::saveGIF}

\item{captions.alt}{Should alternative captions be added to animation? Defaults to FALSE.}
}
\description{
This function visualizes the a cv.glmnet machine learning process.
}
\examples{
# See also: ?cv.glmnet:
set.seed(1010)
n=1000;p=100
nzc=trunc(p/10)
x=matrix(rnorm(n*p),n,p)
beta=rnorm(nzc)
fx= x[,seq(nzc)] \%*\% beta
eps=rnorm(n)*5
y=drop(fx+eps)
px=exp(fx)
px=px/(1+px)
ly=rbinom(n=length(px),prob=px,size=1)
set.seed(1011)
cvob1=cv.glmnet(x,y)
animate_glmnet(cvob1)
}
\keyword{cv.glmnet}
\keyword{glmnet}
\keyword{lasso}
\keyword{machinelearning}
\keyword{seeAI}
