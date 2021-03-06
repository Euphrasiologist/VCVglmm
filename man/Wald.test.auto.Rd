% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Wald.test.auto.R
\name{Wald.test.auto}
\alias{Wald.test.auto}
\title{Performing Wald tests on MCMCglmm fixed effects automatically}
\usage{
Wald.test.auto(mod)
}
\arguments{
\item{mod}{model of class MCMCglmm}
}
\description{
Performing Wald tests on MCMCglmm fixed effects automatically
}
\examples{
# needs aod
library(aod)
mod <-  MCMCglmm(y ~ x, random = ~ z)
# if x is a 4 levelled factor
Wald.test(mod = mod)
}
\keyword{MCMCglmm,}
\keyword{Wald}
\keyword{effects,}
\keyword{fixed}
\keyword{p-values,}
\keyword{test}
