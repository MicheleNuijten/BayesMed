\name{plot.rjags}
\alias{plot.rjags}

\title{
Plot the chains of the JAGS samples
}
\description{
Displays a plot of iterations vs. sampled values for each variable in the chain, with a separate plot per variable (see \code{traceplot} of the package \code{R2jags}; Su & Yajima, 2012).
}
\usage{
\method{plot}{rjags}(x,\dots)
}
\arguments{
  \item{x}{
an \code{rjags} object.
}
  \item{\dots}{
additional arguments to be passed on to the traceplot method, such as graphical parameters (see \code{traceplot}).
}
}
\references{
Nuijten, M.B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (in preparation). A default Bayesian hypothesis test for mediation. 

Su, Y.-S., & Yajima, M. (2012). R2jags: A package for running jags from Rb[Computer software manual]. Available from http://CRAN.R-project.org/package=R2jags (R package version 0.03-08)
}
\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>
}

\examples{
# simulate correlational data
X <- rnorm(50,0,1)
Y <- .3*X + rnorm(50,0,1)

# save jzs_corSD output
res <- jzs_corSD(X,Y)

# plot results
plot(res$jagssamples)

############

# simulate mediational data
a <- .5 
b <- .6
t_accent <- .3

X <- rnorm(50,0,1)
M <- a*X + rnorm(50,0,1)
Y <- t_accent*X + b*M + rnorm(50,0,1)

# run jzs_medSD
res2 <- jzs_medSD(independent=X,dependent=Y,mediator=M)

# plot resulting chains for alpha, beta, and tau
plot(res2$jagssamplesA)
plot(res2$jagssamplesTB)
}