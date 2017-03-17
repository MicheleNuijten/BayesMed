\name{Firefighters}
\alias{Firefighters}
\docType{data}
\title{
Data from a study of health promotion of firefighters
}
\description{
Data from a study of health promotion of firefighters (Elliot et al., 2007). These data are also used as an example in the Bayesian mediation paper of Yuan and MacKinnon (2009) and Nuijten et al. (2014).
}
\usage{data(Firefighters)}
\format{
  A data frame with 354 observations on the following 3 variables.
  \describe{
    \item{\code{y}}{Dependent variable: reported eating of fruits and vegetables}
    \item{\code{m}}{Mediating variable: change from baseline to followup in knowledge of the benefits of eating fruits and vegetables}
    \item{\code{x}}{Independent variable: randomized exposure to an intervention}
  }
}
\details{
The data are centered.
}
\source{
Elliot, D. L., Goldberg, L., Kuehl, K. S., Moe, E. L., Breger, R. K. R., Pickering, M. A. (2007). The PHLAME (Promoting Healthy Lifestyles: Alternative Models' Effects) Firefighter Study: Outcomes of Two Models of Behavior Change. JOEM, 49, 204-213.
}
\references{
Elliot, D. L., Goldberg, L., Kuehl, K. S., Moe, E. L., Breger, R. K. R., Pickering, M. A. (2007). The PHLAME (Promoting Healthy Lifestyles: Alternative Models' Effects) Firefighter Study: Outcomes of Two Models of Behavior Change. JOEM, 49, 204-213.

Nuijten, M. B., Wetzels, R., Matzke, D., Dolan, C. V., & Wagenmakers, E.-J. (2014). A default Bayesian hypothesis test for mediation. Behavior Research Methods. doi: 10.3758/s13428-014-0470-2

Yuan, Y., & MacKinnon, D. (2009). Bayesian mediation analysis. Psychological Methods, 14 , 301-322.

}
\examples{
\dontrun{
data(Firefighters)

# mediation analysis
result <- jzs_med(Firefighters$x,Firefighters$y,Firefighters$m)
result

### Note that the Bayes factor is 1.96, instead of 1.94 as reported 
### in the paper (Nuijten et al., 2014). 
### This is caused by rounding differences; the posterior probabilities are equal.

# underlying mediational model with path weights and posterior probabilities
plot(result$main_result)

# posterior distribution of indirect effect "ab" incl 95\% credible interval
plot(result$ab_samples)
}
}