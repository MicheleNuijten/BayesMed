\name{jzs_cor}
\alias{jzs_cor}

\title{
A default Bayesian hypothesis test for correlation.
}
\description{
This function can be used to perform a default Bayesian hypothesis test for correlation, using a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008).
}
\usage{
jzs_cor(V1, V2,standardize=TRUE)
}
\arguments{
  \item{V1}{
a numeric vector. 
}
  \item{V2}{
a numeric vector of the same length as V1.
}
  \item{standardize}{
  logical. Should the variables be standardized? Defaults to TRUE. 
}
}
\details{
See Wetzels & Wagenmakers, in press.
}
\value{
The function returns a list with the following items:
\item{Correlation}{
The correlation coefficient for the relation between V1 and V2.
}
\item{BayesFactor}{
The Bayes factor for the correlation coefficient. A value greater than one indicates evidence in favor of correlation, a value smaller than one indicates evidence against correlation.
}
\item{PosteriorProbability}{
The posterior probability for the existence of a correlation between V1 and V2.
}
}
\references{
Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g priors for Bayesian variable selection. Journal of the American Statistical Association, 103(481), 410-423.

Nuijten, M.B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (in preparation). A default Bayesian hypothesis test for mediation. 

Wetzels, R. & Wagenmakers, E.-J. (in press). A default Bayesian hypothesis test for correlations and partial correlations. Psychonomic Bulletin & Review.
}
\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers.
}
\seealso{
\code{\link{jzs_partcor}}, \code{\link{jzs_med}}
}
\examples{
# generate correlational data
X <- rnorm(100)
Y <- .4*X + rnorm(100,0,1)

# run jzs_cor
jzs_cor(X,Y)

}
