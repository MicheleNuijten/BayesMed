\name{BayesMed}
\alias{BayesMed}
\docType{package}
\title{
A default Bayesian hypothesis test for mediation, correlation, and partial correlation.
}
\description{
This package can be used to perform a default Bayesian hypothesis test for mediation, correlation, and partial correlation, either analytically or through the Savage-Dickey method (Dickey & Lientz, 1970). All tests make use of a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008). 
}
\details{
\tabular{ll}{
Package: \tab BayesMed\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2012-07-31\cr
License: \tab GPL-2\cr
}
The main functions \code{jzs_med} and \code{jzs_medSD} can be used to establish mediation in a data set. With \code{jzs_cor} and \code{jzs_corSD} you can establish correlation, and with jzs_partcor and jzs_partcorSD partial correlation. 
}
\author{
Michele B. Nuijten <michele.nuijten@gmail.com>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers. Many thanks to Sacha Epskamp.
}
\references{
Dickey, J. M., & Lientz, B. P. (1970). The weighted likelihood ratio, sharp hypotheses about chances, the order of a Markov chain. The Annals of Mathematical Statistics, 214-226.

Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g priors for Bayesian variable selection. Journal of the American Statistical Association, 103(481), 410-423.

Wetzels, R. & Wagenmakers, E.-J. (in press). A default Bayesian hypothesis test for correlations and partial correlations. Psychonomic Bulletin & Review.
}

\examples{
# simulate mediational data
X <- rnorm(50,0,1)
M <- .5*X + rnorm(50,0,1)
Y <- .3*X + .6*M + rnorm(50,0,1)

# run jzs_med to perform the Bayesian hypothesis test for mediation
(result <- jzs_med(independent=X,dependent=Y,mediator=M))


# plot results
plot(result)
}
