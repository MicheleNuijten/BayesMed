\name{BayesMed}
\alias{BayesMed}
\docType{package}
\title{
A default Bayesian hypothesis test for mediation, correlation, and partial correlation.
}
\description{
This package can be used to perform a default Bayesian hypothesis test for mediation, correlation, and partial correlation, either analytically or through the Savage-Dickey method (Dickey & Lientz, 1970). All tests make use of a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008). This package is based on the paper by Nuijten, Wetzels, Matzke, Dolan, and Wagenmakers (under review).
}
\details{
\tabular{ll}{
Package: \tab BayesMed\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2012-07-31\cr
License: \tab GPL-2\cr
}
The main functions \code{jzs_med} and \code{jzs_medSD} can be used to establish and test mediation in a data set. With \code{jzs_cor} and \code{jzs_corSD} you can establish and test correlation, and with jzs_partcor and jzs_partcorSD partial correlation. 
}
\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers. Many thanks to Sacha Epskamp.
}
\references{
Dickey, J. M., & Lientz, B. P. (1970). The weighted likelihood ratio, sharp hypotheses about chances, the order of a Markov chain. The Annals of Mathematical Statistics, 214-226.

Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g priors for Bayesian variable selection. Journal of the American Statistical Association, 103(481), 410-423.

Nuijten, M. B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (2014). A default Bayesian hypothesis test for mediation. Manuscript submitted for publication.

Wetzels, R. & Wagenmakers, E.-J. (2012). A default Bayesian hypothesis test for correlations and partial correlations. Psychonomic Bulletin & Review.

}

\examples{
\dontrun{
# simulate mediational data
X <- rnorm(50,0,1)
M <- .5*X + rnorm(50,0,1)
Y <- .3*X + .6*M + rnorm(50,0,1)

###########

# run jzs_med to perform the Bayesian hypothesis test for mediation
# jzs_med is an analytical test and is faster than jzs_medSD

result <- jzs_med(independent=X,dependent=Y,mediator=M)
result

# plot results
plot(result)

###########

# run jzs_medSD to perform the Savage-Dickey (SD) Bayesian hypothesis test for mediation
# advantages of the SD method are:
      # the possibility to test parameters one-sided
      # the possibility to estimate the posterior distributions of the parameters


result_SD <- jzs_medSD(independent=X,dependent=Y,mediator=M)
result_SD

### NOTE ###
#Sometimes this error will pop up:
#
#Error in solve.default(nItheta) : 
#  system is computationally singular: reciprocal condition number = *some small number*
#Error in mydt2(0, mT, sT, dfT) : unused arguments (mT, sT, dfT)
#In addition: Warning message:
#In jzs_medSD(X, Y, M) :
#  fit.st did not converge. Alternative optimization method was used.
#  
#If this happens, just run jzs_medSD() again. This usually solves the convergence problem. If it does #not, try a different SD method. For instance: jzs_medSD(X,Y,M,SDmethod="dnorm").
#
#############

# plot(results)
plot(result_SD$main_result)

# plot posterior samples including credible interval, mean, and median of the indirect effect alpha*beta
plot(result_SD$ab_samples)

# inspect separate posterior distributions of alpha, beta, and tau_prime
plot(result_SD$alpha_samples)
plot(result_SD$beta_samples)
plot(result_SD$tau_prime_samples)

# print a traceplot of the chains
# where the first chain (theta[1]) is for tau' and the second chain (theta[2]) for beta
plot(result$jagssamplesA)
plot(result$jagssamplesTB) 
}
}
