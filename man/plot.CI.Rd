\name{plot.CI}
\alias{plot.CI}
\title{
Plot the posterior distribution of the indirect effect alpha*beta
}
\description{
Plot the posterior distribution of the indirect effect alpha*beta including a 95\% credible interval around the mean of the posterior (see Nuijten et al. (2014); Yuan & MacKinnon, 2009).
}
\usage{
\method{plot}{CI}(x,\dots)
}
\arguments{
  \item{x}{
the posterior samples of alpha*beta as obtained from the output of \code{\link{jzs_medSD}}. This is an object of class \code{CI}.
}
  \item{\dots}{
additional arguments to be passed on to the plot method, such as graphical parameters (see \code{par}).
}
}
\references{
Nuijten, M.B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (2014). A default Bayesian hypothesis test for mediation. Manuscript submitted for publication. 

Yuan, Y., & MacKinnon, D. (2009). Bayesian mediation analysis. Psychological Methods, 14, 301-322.
}
\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers.
}
\seealso{
\code{\link{jzs_medSD}}
}
\examples{
# simulate mediational data
a <- .5 
b <- .6
t_prime <- .3

X <- rnorm(50,0,1)
M <- a*X + rnorm(50,0,1)
Y <- t_prime*X + b*M + rnorm(50,0,1)

# run jzs_medSD
res <- jzs_medSD(independent=X,dependent=Y,mediator=M)

# plot posterior distribution of a*b
plot(res$ab_samples)

# print the exact lower and upper boundary of the interval
res$CI_ab
}
