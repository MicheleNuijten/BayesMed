\name{jzs_cor}
\alias{jzs_cor}

\title{
A default Bayesian hypothesis test for correlation (Wetzels, R., & Wagenmakers).
}
\description{
This function can be used to perform a default Bayesian hypothesis test for correlation, using a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008).
}
\usage{
jzs_cor(V1, V2,
        alternative = c("two.sided", "less", "greater"),
        n.iter=10000,n.burnin=500,standardize=TRUE)
}
\arguments{
  \item{V1}{
a numeric vector. 
}
  \item{V2}{
a numeric vector of the same length as V1.
}
  \item{alternative}{
specify the alternative hypothesis for the correlation coefficient: \code{two.sided}, \code{greater} than zero, or \code{less} than zero.
}
  \item{n.iter}{
  number of total iterations per chain (see the package \code{R2jags}). Defaults to 10000.
}
  \item{n.burnin}{
  length of burn in, i.e. number of iterations to discard at the beginning(see the package \code{R2jags}).  Defaults to 500.
}
  \item{standardize}{
  logical. Should the variables be standardized? Defaults to TRUE. 
}
}
\details{
See Wetzels & Wagenmakers (2012).
}
\value{
The function returns a list with the following items:
\item{Correlation}{
The correlation coefficient for the relation between V1 and V2. The correlation coefficient is calculated by standardizing the mean of the posterior samples: mean(samples)*(sd(V1)/sd(V2)).
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
\item{jagssamples}{The JAGS output for the MCMC estimation of the path. This object can be used to construct a traceplot.
}
}
\references{
Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g priors for Bayesian variable selection. Journal of the American Statistical Association, 103(481), 410-423.

Nuijten, M.B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (2014). A default Bayesian hypothesis test for mediation. Manuscript submitted for publication.

Wetzels, R., & Wagenmakers, E.-J. (2012). A Default Bayesian Hypothesis Test for Correlations and Partial Correlations. Psychonomic Bulletin & Review, 19, 1057-1064.
}
\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers.
}
\seealso{
\code{\link{jzs_partcor}}, \code{\link{jzs_med}}
}
\examples{
\dontrun{
# generate correlational data
X <- rnorm(100)
Y <- .4*X + rnorm(100,0,1)

# run jzs_cor
result <- jzs_cor(X,Y)

# inspect posterior distribution 
plot(result$alpha_samples)

# print a traceplot of the chains
plot(result$jagssamples)

}
}
