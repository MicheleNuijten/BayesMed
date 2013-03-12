\name{jzs_medSD}
\alias{jzs_medSD}
\title{
Perform a default Bayesian hypothesis test for mediation using the Savage-Dickey method.
}
\description{
This function can be used to perform a default Bayesian hypothesis test for mediation, using the Savage-Dickey method (Dickey & Lientz, 1970). The test uses a Jeffreys-Zellner-Siow prior set-up (Liang et al., 2008).
}
\usage{
jzs_medSD(independent, dependent, mediator, 
          SDmethod=c("fit.st","dnorm","splinefun","logspline"), 
          alternativeA=c("two.sided","less","greater"), 
          alternativeB=c("two.sided","less","greater"), 
          alternativeT=c("two.sided","less","greater"),
          n.iter=10000,n.burnin=500,
          standardize=TRUE)
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
\item{SDmethod}{
specify the precise method with which the density of the posterior distribution will be estimated in order to compute the Savage-Dickey ratio.
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
\note{
This function requires the program "JAGS" (Just Another Gibbs Sampler). This program can be obtained from http://mcmc-jags.sourceforge.net.
}
\details{
The test consists of four steps. Firstly, it computes the posterior probability for the existence of the path between the independent and the mediating variable (path alpha) through the Savage-Dickey density ratio method. 

Secondly, it computes the posterior probability for the existence of the path between the mediating and the dependent variable, controlled for the influence of the independent variable (path beta), again with the Savage-Dickey density ratio method.

Thirdly, the evidence for mediation is computed by multiplying the posterior probabilities for the paths alpha and beta.

Fourthly, the evidence for full mediation is computed by multiplying the evidence for mediation with one minus the posterior probability for the existence of path tau', the path between the independent and dependent variable, controlled for the mediator.
}
\value{
A list containing the following components:
\item{EvidenceMediation}{The posterior probability that the relation between the independent and the dependent variable is mediated by the specified mediator.
}
\item{EvidenceFullMediation}{The posterior probability that the relation between the independent and the dependent variable is fully mediated by the specified mediator and the direct effect of the independent variable on the dependent variable disappears after introducing the mediator.
}
\item{BF_Mediation}{The Bayes factor for mediation compared to no mediation. A value greater than one indicates evidence in favor of mediation, a value smaller than one indicates evidence against mediation.
}
\item{BF_FullMediation}{The Bayes factor for full mediation compared to no mediation. A value greater than one indicates evidence in favor of full mediation, a value smaller than one indicates evidence against full mediation.
}
\item{BF_alpha}{The Bayes factor for the existence of path alpha. A value greater than one indicates evidence that alpha exists, a value smaller than one indicates evidence that alpha does not exist.
}
\item{BF_beta}{The Bayes factor for the existence of path beta. A value greater than one indicates evidence that beta exists, a value smaller than one indicates evidence that beta does not exist.
}
\item{BF_tau_accent}{The Bayes factor for the existence of path tau_accent. A value greater than one indicates evidence that tau_accent exists, a value smaller than one indicates evidence that tau_accent does not exist.
}
\item{CI_ab}{The 95\% credible interval around the indirect effect "ab".}
\item{prob_alpha}{The posterior probability that the path alpha (the relation between the independent and the mediating variable) is not zero.
}
\item{prob_beta}{The posterior probability that the path beta (the relation between the mediating and the dependent variable after controlling for the independent variable) is not zero.
}
\item{prob_tau_accent}{The posterior probability that the path tau' (the relation between the independent and the dependent variable after controlling for the mediator) is not zero.
}
\item{alpha}{The posterior samples for alpha.
}
\item{beta}{The posterior samples for beta.
}
\item{tau_accent}{The posterior samples for tau_accent.
}
\item{alpha_times_beta}{The posterior distribution of alpha multiplied by the posterior distribution of beta. This is equivalent to the posterior distribution of the indirect effect "ab".}
\item{jagssamplesA}{The JAGS output for the MCMC estimation of path alpha. This object can be used to construct a traceplot.}
\item{jagssamplesTB}{The JAGS output for the MCMC estimation of path tau' and beta. This object can be used to construct a traceplot.}
}
\references{
Dickey, J. M., & Lientz, B. P. (1970). The weighted likelihood ratio, sharp hypotheses about chances, the order of a Markov chain. The Annals of Mathematical Statistics, 214-226.

Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g priors for Bayesian variable selection. Journal of the American Statistical Association, 103(481), 410-423.

Nuijten, M.B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (in preparation). A default Bayesian hypothesis test for mediation. 

Wetzels, R., & Wagenmakers, E.-J. (2012). A Default Bayesian Hypothesis Test for Correlations and Partial Correlations. Psychonomic Bulletin & Review, 19, 1057-1064.
}
\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers.
}


\section{Warning}{
In some cases the SDmethod \code{fit.st} will fail to converge. If so, jzs_medSD automatically switches to another optimization method (optim {stats}) with different starting values (mean and sd of the posterior sample). If the other optimization method does not converge either, you could try one of the other SDmethod options or see \code{\link{jzs_med}}.
} 

\seealso{
\code{\link{jzs_med}}
}

\examples{
\dontrun{

# simulated mediational data
a <- .5 
b <- .6
t_accent <- .3

X <- rnorm(50,0,1)
M <- a*X + rnorm(50,0,1)
Y <- t_accent*X + b*M + rnorm(50,0,1)

# run jzs_medSD
result <- jzs_medSD(independent=X,dependent=Y,mediator=M)

# print result
result

# inspect posterior distribution for the coefficients
hist(result$alpha,100,freq=FALSE,
     main="posterior distribution for alpha",xlab="posterior samples of alpha")
hist(result$beta,100,freq=FALSE,
     main="posterior distribution for beta",xlab="posterior samples of beta")
hist(result$tau_accent,100,freq=FALSE,
     main="posterior distribution for tau'",xlab="posterior samples of tau'")

# print a traceplot of the chains
plot(result$jagssamplesA)
plot(result$jagssamplesTB) 
# where the first chain is for tau' and the second chain for beta

# calculate and plot a 95\% credible interval for the 
# posterior mean of the indirect effect
result$CI_ab
plot(result$alpha_times_beta)

#----------------------------

# Firefighter data
data(Firefighters)

X <- Firefighters$x
Y <- Firefighters$y
M <- Firefighters$m

# run jzs_medSD
result <- jzs_medSD(independent=X,dependent=Y,mediator=M)

# NOTE:
# If jzs_medSD does not converge the first time you run it,
# run the exact same code again, usually it will converge the 
# second time.

# print result
result

# plot the result
plot(result)

# inspect posterior distribution for the coefficients
hist(result$alpha,100,freq=FALSE,
     main="posterior distribution for alpha",xlab="posterior samples of alpha")
hist(result$beta,100,freq=FALSE,
     main="posterior distribution for beta",xlab="posterior samples of beta")
hist(result$tau_accent,100,freq=FALSE,
     main="posterior distribution for tau'",xlab="posterior samples of tau'")

# print a traceplot of the chains
plot(result$jagssamplesA)
plot(result$jagssamplesTB) 
# where the first chain is for tau' and the second chain for beta

# calculate and plot a 95\% credible interval for the 
# posterior mean of the indirect effect
result$CI_ab
plot(result$alpha_times_beta)
}
}