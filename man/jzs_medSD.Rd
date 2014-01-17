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
jzs_mdSD returns a list containing visible (printed) and invisible components. The visible components are a data frame with the main results and the 95\% credible interval of the mediated effect (see next section). The invisible components contain additional information on the parameters, and can be used for plot functions etc.
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

\section{Invisible Output}{
\describe{

jzs_medSD also returns output that is not printed to the screen. This invisible output contains the following elements:

\item{alpha_samples}{The posterior samples of alpha.
}
\item{beta_samples}{The posterior samples of beta.
}
\item{tau_prime_samples}{The posterior samples of tau_accent.
}
\item{ab_samples}{The posterior distribution of alpha multiplied by the posterior distribution of beta. This is equivalent to the posterior distribution of the indirect effect "ab".}
\item{jagssamplesA}{The JAGS output for the MCMC estimation of path alpha. This object can be used to construct a traceplot.}
\item{jagssamplesTB}{The JAGS output for the MCMC estimation of path tau' and beta. This object can be used to construct a traceplot.}
}
}

\references{
Dickey, J. M., & Lientz, B. P. (1970). The weighted likelihood ratio, sharp hypotheses about chances, the order of a Markov chain. The Annals of Mathematical Statistics, 214-226.

Liang, F., Paulo, R., Molina, G., Clyde, M. A., & Berger, J. O. (2008). Mixtures of g priors for Bayesian variable selection. Journal of the American Statistical Association, 103(481), 410-423.

Nuijten, M.B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (2014). A default Bayesian hypothesis test for mediation. Manuscript submitted for publication.

Wetzels, R., & Wagenmakers, E.-J. (2012). A Default Bayesian Hypothesis Test for Correlations and Partial Correlations. Psychonomic Bulletin & Review, 19, 1057-1064.
}
\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers.
}


\section{Warning}{
In some cases the SDmethod \code{fit.st} will fail to converge. If so, jzs_medSD automatically switches to another optimization method (optim {stats}) with different starting values (mean and sd of the posterior sample). If the other optimization method does not converge either, you could either try to run the same code again (sometimes it will converge a second time), or you could try one of the other SDmethod options or try the analytical mediation test instead (see \code{\link{jzs_med}}). 
} 

\seealso{
\code{\link{jzs_med}}
}

\examples{
\dontrun{

# simulated mediational data
a <- .5 
b <- .6
t_prime <- .3

X <- rnorm(50,0,1)
M <- a*X + rnorm(50,0,1)
Y <- t_prime*X + b*M + rnorm(50,0,1)

# run jzs_medSD
result <- jzs_medSD(independent=X,dependent=Y,mediator=M)

# print result
result

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

#------------------------------------------

# load Firefighter data
data(Firefighters)

X <- Firefighters$x
M <- Firefighters$m
Y <- Firefighters$y

# run jzs_medSD
result <- jzs_medSD(independent=X,dependent=Y,mediator=M)

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