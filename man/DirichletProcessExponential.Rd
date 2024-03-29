% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dirichlet_process_exponential.R
\name{DirichletProcessExponential}
\alias{DirichletProcessExponential}
\title{Create a Dirichlet Mixture of Exponentials}
\usage{
DirichletProcessExponential(y, g0Priors = c(0.01, 0.01), alphaPriors = c(2, 4))
}
\arguments{
\item{y}{Data}

\item{g0Priors}{Base Distribution Priors \eqn{\alpha _0 , \beta _0)}}

\item{alphaPriors}{Alpha prior parameters. See \code{\link{UpdateAlpha}}.}
}
\value{
Dirichlet process object
}
\description{
This is the constructor function to produce a \code{dirichletprocess} object with a Exponential mixture kernel with unknown rate.
The base measure is a  Gamma distribution that is conjugate to the posterior distribution.
}
\details{
\eqn{G_0(\theta | \alpha _0, \beta_0) =  \mathrm{Gamma} \left(\theta | \alpha_0, \beta_0 \right)}
}
