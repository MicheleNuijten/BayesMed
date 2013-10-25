\name{jzs_med}
\alias{jzs_med}

\title{
Perform a default Bayesian hypothesis test for mediation.
}
\description{
This function can be used to perform a default Bayesian hypothesis test for mediation, using a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008). The test is based on the default Bayesian hypothesis tests for correlation and partial correlation (Wetzels & Wagenmakers, 2012).
}
\usage{
jzs_med(independent, dependent, mediator,
        alternativeA=c("two.sided","less","greater"), 
        alternativeB=c("two.sided","less","greater"), 
        alternativeT=c("two.sided","less","greater"),
        n.iter=10000,n.burnin=500,standardize=TRUE)
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
\item{alternativeA}{
specify the alternative hypothesis for path alpha: \code{two.sided}, \code{greater} than zero, or \code{less} than zero.
}
\item{alternativeB}{
specify the alternative hypothesis for path beta: \code{two.sided}, \code{greater} than zero, or \code{less} than zero.
}
\item{alternativeT}{
specify the alternative hypothesis for path tau_accent: \code{two.sided}, \code{greater} than zero, or \code{less} than zero.
}
  \item{n.iter}{
  number of total iterations per chain (see the package \code{R2jags}). Defaults to 10000. 
}
  \item{n.burnin}{
  length of burn in, i.e. number of iterations to discard at the beginning(see the package \code{R2jags}). Defaults to 500. 
}
\item{standardize}{
  logical. Should the variables be standardized? Defaults to TRUE. 
} 
}
\details{
The test consists of four steps. Firstly, it computes the posterior probability for the existence of the path between the independent and the mediating variable (path alpha) by means of a default Bayesian hypothesis test for correlation (Wetzels & Wagenmakers, 2012). 

Secondly, it computes the posterior probability for the existence of the path between the mediating and the dependent variable, controlled for the influence of the independent variable (path beta) by means of a default Bayesian hypothesis test for partial correlation (Wetzels & Wagenmakers, 2012).

Thirdly, the evidence for mediation is computed by multiplying the posterior probabilities for the paths alpha and beta.

Fourthly, the evidence for full mediation is computed by multiplying the evidence for mediation with one minus the posterior probability for the existence of path tau', the path between the independent and dependent variable, controlled for the mediator.
}
\value{
jzs_md returns a list containing visible (printed) and invisible components. The visible components are a data frame with the main results and the 95\% credible interval of the mediated effect (see next section). The invisible components contain additional information on the parameters, and can be used for plot functions etc.
}

\section{Visible Output}{
\describe{

The visible output, the output that is printed to the screen, is a list containing a data frame and a credible interval.

\item{Estimate_alpha}{The mean of the posterior samples of alpha.
}
\item{Estimate_beta}{The mean of the posterior samples of beta.
}
\item{Estimate_tau_prime}{The mean of the posterior samples of tau_prime.
}
\item{Estimate_Mediation (alpha*beta)}{The mean of the posterior samples of the indirect effect alpha*beta.}

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

\item{CI_ab}{The 95\% credible interval of the indirect effect "ab".}
}
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
\dontrun{
# simulate mediational data
a <- .5 
b <- .6
t_prime <- .3

X <- rnorm(50,0,1)
M <- a*X + rnorm(50,0,1)
Y <- t_prime*X + b*M + rnorm(50,0,1)

# run jzs_med

result <- jzs_med(independent=X,dependent=Y,mediator=M)
result 

#-------------------------------------

# load Firefighter data
data(Firefighters)

X <- Firefighters$x
M <- Firefighters$m
Y <- Firefighters$y

# run jzs_med
result <- jzs_med(independent=X,dependent=Y,mediator=M)

# plot the result in a mediation diagram
plot(result$main_result)

# inspect posterior distribution of the coefficients
plot(result$alpha_samples)
plot(result$beta_samples)
plot(result$tau_prime_samples)

# print a traceplot of the chains
plot(result$jagssamplesA)
plot(result$jagssamplesTB) 
# where the first chain (theta[1]) is for tau' and the second chain (theta[2]) for beta

# calculate and plot a 95\% credible interval for the 
# posterior mean of the indirect effect
result$CI_ab
plot(result$ab_samples)


}
}