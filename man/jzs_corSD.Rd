\name{jzs_corSD}
\alias{jzs_corSD}

\title{
A default Bayesian hypothesis test for correlation using the Savage-Dickey method.
}
\description{
This function can be used to perform a default Bayesian hypothesis test for correlation, using the Savage-Dickey method (Dickey & Lientz, 1970). The test uses a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008).
}
\usage{
jzs_corSD(V1, V2, 
          SDmethod = c("fit.st", "dnorm", "splinefun", "logspline"), 
          alternative = c("two.sided", "less", "greater"),
          n.iter=10000,n.burnin=500)
}
\arguments{
  \item{V1}{
a numeric vector.
}
  \item{V2}{
a numeric vector of the same length as V1.
}
  \item{SDmethod}{
specify the precise method with which the density of the posterior distribution will be estimated in order to compute the Savage-Dickey ratio.
}
  \item{alternative}{
specify the alternative hypothesis for the correlation coefficient: \code{two.sided}, \code{greater} than zero, or \code{less} than zero.
}
  \item{n.iter}{
  number of total iterations per chain (see the package \code{R2jags}).  Defaults to 10000.
}
  \item{n.burnin}{
  length of burn in, i.e. number of iterations to discard at the beginning(see the package \code{R2jags}).  Defaults to 500.
}
}
\value{
A list containing the following components:
\item{Correlation}{
The correlation coefficient for the relation between V1 and V2.
}
\item{BayesFactor}{
The Bayes factor for the correlation coefficient. A value greater than one indicates evidence in favor of correlation, a value smaller than one indicates evidence against correlation.
}
\item{PosteriorProbability}{
The posterior probability for the existence of a correlation between V1 and V2.
}
\item{alpha}{
The posterior samples for the correlation coefficient alpha.
}
\item{jagssamples}{The JAGS output for the MCMC estimation of the path. This object can be used to construct a traceplot.}
}
\references{
Dickey, J. M., & Lientz, B. P. (1970). The weighted likelihood ratio, sharp hypotheses about chances, the order of a Markov chain. The Annals of Mathematical Statistics, 214-226.

Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g priors for Bayesian variable selection. Journal of the American Statistical Association, 103(481), 410-423.

Nuijten, M.B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (in preparation). A default Bayesian hypothesis test for mediation. 

Wetzels, R. & Wagenmakers, E.-J. (in press). A default Bayesian hypothesis test for correlations and partial correlations. Psychonomic Bulletin & Review.
}
\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers.
}
\section{Warning}{
In some cases the SDmethod \code{fit.st} will fail to converge. If so, another optimization method is used, using different starting values. If the other optimization method does not converge either or gives you a negative Bayes factor (which is meaningless), you could try one of the other SDmethod options or see \code{\link{jzs_cor}}.
}
\seealso{
\code{\link{jzs_cor}}, \code{\link{jzs_partcorSD}}
}
\examples{
# generate correlational data
X <- rnorm(100)
Y <- .4*X + rnorm(100,0,1)

# run jzs_corSD
(res <- jzs_corSD(X,Y))

# plot posterior samples
hist(res$alpha,freq=FALSE)
}