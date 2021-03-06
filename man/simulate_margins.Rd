% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marginals.R
\name{simulate_margins}
\alias{simulate_margins}
\title{Generate Marginal Distributions}
\usage{
simulate_margins(model, n)
}
\arguments{
\item{model}{An object of the \code{cma_fit} class.}

\item{n}{An \code{integer} with the number of samples to be generated.}
}
\value{
An S3 \code{list} of the class \code{marginal}.
}
\description{
This function takes a fitted object from the \code{cma_fit} class and generate
new scenarios for the marginal distributions.
}
\examples{
x <- matrix(diff(log(EuStockMarkets)), ncol = 4)
colnames(x) <- colnames(EuStockMarkets)

# fit the student t
dist <- fit_t(x)
dist

# generate new margins from the fitted model
simulate_margins(dist, 10000)
}
