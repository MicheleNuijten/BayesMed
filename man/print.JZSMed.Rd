\name{print.jzs_med}
\alias{print.jzs_med}
\title{
Print jzs_med output.
}
\description{
Print the output of a jzs_med object.
}

\usage{
\method{print}{jzs_med}(x,\dots)
}
\arguments{
  \item{x}{
a JZSMedSD object.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}

\value{
The function returns a list with the following items:

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
}

\author{
Michele B. Nuijten <m.b.nuijten@uvt.nl>, Ruud Wetzels, Dora Matzke, Conor V. Dolan, and Eric-Jan Wagenmakers.
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

# run jzs_medSD
result <- jzs_medSD(independent=X,dependent=Y,mediator=M,SDmethod='dnorm')

# print result
result
}
}