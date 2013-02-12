\name{jzs_med}
\alias{jzs_med}

\title{
Perform a default Bayesian hypothesis test for mediation.
}
\description{
This function can be used to perform a default Bayesian hypothesis test for mediation, using a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008).
}
\usage{
jzs_med(independent, dependent, mediator)
}
\arguments{
  \item{independent}{
a vector containing values for the independent variable.
}
  \item{dependent}{
a vector containing values for the dependent variable.
}
  \item{mediator}{
a vector containing values for the mediating variable.
}
}
\details{
The test consists of four steps. Firstly, it computes the posterior probability for the existence of the path between the independent and the mediating variable (path alpha) by means of a default Bayesian hypothesis test for correlation (Wetzels & Wagenmakers, 2012). 

Secondly, it computes the posterior probability for the existence of the path between the mediating and the dependent variable, controlled for the influence of the independent variable (path beta) by means of a default Bayesian hypothesis test for partial correlation (Wetzels & Wagenmakers, 2012).

Thirdly, the evidence for mediation is computed by multiplying the posterior probabilities for the paths alpha and beta.

Fourthly, the evidence for full mediation is computed by multiplying the evidence for mediation with one minus the posterior probability for the existence of path tau', the path between the independent and dependent variable, controlled for the mediator.
}
\value{
The function returns a list with the following items:
\item{EvidenceMediation}{The posterior probability that the relation between the independent and the dependent variable is mediated by the specified mediator.
}
\item{EvidenceFullMediation}{The posterior probability that the relation between the independent and the dependent variable is fully mediated by the specified mediator and the direct effect of the independent variable on the dependent variable disappears after introducing the mediator.
}
\item{BayesFactorMediation}{The Bayes factor for mediation compared to no mediation. A value greater than one indicates evidence in favor of mediation, a value smaller than one indicates evidence against mediation.
}
\item{BayesFactorFullMediation}{The Bayes factor for full mediation compared to no mediation. A value greater than one indicates evidence in favor of full mediation, a value smaller than one indicates evidence against full mediation.
}
\item{prob_alpha}{The posterior probability that the path alpha (the relation between the independent and the mediating variable) is not zero.
}
\item{prob_beta}{The posterior probability that the path beta (the relation between the mediating and the dependent variable after controlling for the independent variable) is not zero.
}
\item{prob_tau_accent}{The posterior probability that the path tau' (the relation between the independent and the dependent variable after controlling for the mediator) is not zero.}
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
\code{\link{jzs_cor}}, \code{\link{jzs_partcor}}, \code{\link{jzs_medSD}}
}
\examples{
# simulate mediational data
a <- .5 
b <- .6
t_accent <- .3

X <- rnorm(50,0,1)
M <- a*X + rnorm(50,0,1)
Y <- t_accent*X + b*M + rnorm(50,0,1)

# run jzs_med
jzs_med(independent=X,dependent=Y,mediator=M)
}
