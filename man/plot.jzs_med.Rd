\name{plot.jzs_med}
\alias{plot.jzs_med}

\title{
Display the results of \code{\link{jzs_med}} in a figure.
}
\description{
This function displays the posterior probabilities of path alpha, beta, and tau' in a mediation schema and thus renders a clear view of the structure in the data.
}
\usage{
\method{plot}{jzs_med}(x,\dots)
}

\arguments{
  \item{x}{
the output of the jzs_med function.
}
  \item{\dots}{
additional arguments to be passed on to the plot method, such as graphical parameters (see \code{par}).  
}
}

\author{
Michele B. Nuijten <michele.nuijten@gmail.com>
}

\seealso{
 \code{\link{jzs_med}}
}
\examples{
# simulate mediational data
X <- rnorm(50,0,1)
M <- .5*X + rnorm(50,0,1)
Y <- .3*X + .6*M + rnorm(50,0,1)

# save jzs_med output
res <- jzs_med(independent=X,dependent=Y,mediator=M)

# plot results
plot(res)
}
