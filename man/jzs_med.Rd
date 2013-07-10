\name{jzs_med}
\alias{jzs_med}

\title{
Perform a default Bayesian hypothesis test for mediation.
}
\description{
This function can be used to perform a default Bayesian hypothesis test for mediation, using a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008).
}
\usage{
jzs_med(independent, dependent, mediator,...)
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
  \item{\dots}{
  additional arguments to be passed on to \code{\link{jzs_cor}} and \code{\link{jzs_partcor}}, such as \code{standardize}.
  }
  
}
\details{
The test consists of four steps. Firstly, it computes the posterior probability for the existence of the path between the independent and the mediating variable (path alpha) by means of a default Bayesian hypothesis test for correlation (Wetzels & Wagenmakers, 2012). 

Secondly, it computes the posterior probability for the existence of the path between the mediating and the dependent variable, controlled for the influence of the independent variable (path beta) by means of a default Bayesian hypothesis test for partial correlation (Wetzels & Wagenmakers, 2012).

Thirdly, the evidence for mediation is computed by multiplying the posterior probabilities for the paths alpha and beta.

Fourthly, the evidence for full mediation is computed by multiplying the evidence for mediation with one minus the posterior probability for the existence of path tau', the path between the independent and dependent variable, controlled for the mediator.
}
\value{
The function returns a data frame with the following values:

\item{BF_alpha}{The Bayes factor for the existence of path alpha. A value greater than one indicates evidence that alpha exists, a value smaller than one indicates evidence that alpha does not exist.
}
\item{BF_beta}{The Bayes factor for the existence of path beta. A value greater than one indicates evidence that beta exists, a value smaller than one indicates evidence that beta does not exist.
}
\item{BF_tau_prime}{The Bayes factor for the existence of path tau_prime. A value greater than one indicates evidence that tau_prime exists, a value smaller than one indicates evidence that tau_prime does not exist.
}
\item{BF_Mediation (alpha*beta)}{The Bayes factor for mediation compared to no mediation. A value greater than one indicates evidence in favor of mediation, a value smaller than one indicates evidence against mediation.
}

\item{PostProb_alpha}{The posterior probability that the path alpha (the relation between the independent and the mediating variable) is not zero.}
\item{PostProb_beta}{The posterior probability that the path beta (the relation between the mediating and the dependent variable after controlling for the independent variable) is not zero.}
\item{PostProb_tau_prime}{The posterior probability that the path tau_prime (the relation between the independent and the dependent variable after controlling for the mediator) is not zero.}
\item{PostProb_Mediation (alpha*beta)}{The posterior probability that the relation between the independent and the dependent variable is mediated by the specified mediator.}
}

\references{
Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g priors for Bayesian variable selection. Journal of the American Statistical Association, 103(481), 410-423.

Nuijten, M.B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (in preparation). A default Bayesian hypothesis test for mediation. 

Wetzels, R., & Wagenmakers, E.-J. (2012). A Default Bayesian Hypothesis Test for Correlations and Partial Correlations. Psychonomic Bulletin & Review, 19, 1057-1064.
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
t_prime <- .3

X <- rnorm(50,0,1)
M <- a*X + rnorm(50,0,1)
Y <- t_prime*X + b*M + rnorm(50,0,1)

# run jzs_med
jzs_med(independent=X,dependent=Y,mediator=M)

#----------------------------

# Firefighter data
data(Firefighters)

X <- Firefighters$x
Y <- Firefighters$y
M <- Firefighters$m

# run jzs_med
result <- jzs_med(independent=X,dependent=Y,mediator=M)

# print result
result

# plot the result
plot(result)
}