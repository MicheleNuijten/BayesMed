\name{plot.JZSMed}
\alias{plot.JZSMed}

\title{
Display the results of \code{\link{jzs_med}} in a figure.
}
\description{
This function displays the estimates and posterior probabilities of path alpha, beta, and tau' in a mediation schema and thus renders a clear view of the structure in the data.
}
\usage{
\method{plot}{JZSMed}(x,\dots)
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
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers.
}

\seealso{
 \code{\link{jzs_med}}
}
\examples{
# simulate mediational data
a <- .5 
b <- .6
t_prime <- .3

X <- rnorm(50,0,1)
M <- a*X + rnorm(50,0,1)
Y <- t_prime*X + b*M + rnorm(50,0,1)

# save jzs_medSD output
res <- jzs_medSD(independent=X,dependent=Y,mediator=M)

# plot results
plot(res$main_result)
}
