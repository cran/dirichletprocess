% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostic_plots.R
\name{DiagnosticPlots}
\alias{DiagnosticPlots}
\alias{AlphaTraceplot}
\alias{AlphaPriorPosteriorPlot}
\alias{ClusterTraceplot}
\alias{LikelihoodTraceplot}
\title{Diagnostic plots for dirichletprocess objects}
\usage{
DiagnosticPlots(dpobj, gg = FALSE)

AlphaTraceplot(dpobj, gg = TRUE)

AlphaPriorPosteriorPlot(
  dpobj,
  prior_color = "#2c7fb8",
  post_color = "#d95f02",
  gg = TRUE
)

ClusterTraceplot(dpobj, gg = TRUE)

LikelihoodTraceplot(dpobj, gg = TRUE)
}
\arguments{
\item{dpobj}{A dirichletprocess object that was fit.}

\item{gg}{Logical; whether to create a ggplot or base R plot (if \code{gg =
FALSE}). For \code{DiagnosticPlots}, this means that the plots will be
given one-by-one, while base plots can be arranged in a grid.}

\item{prior_color}{For \code{AlphaPriorPosteriorPlot}, the color of the prior
function.}

\item{post_color}{For \code{AlphaPriorPosteriorPlot}, the color of the
posterior histogram.}
}
\value{
If \code{gg = TRUE}, a ggplot2 object. Otherwise, nothing is returned
  and a base plot is plotted.
}
\description{
Plot several diagnostic plots for dirichletprocess objects. Because the
dimension of the dirichletprocess mixture is constantly changing, it is not
simple to create meaningful plots of the sampled parameters. Therefore, the
plots focus on the likelihood, alpha, and the number of clusters.
}
\section{Functions}{
\itemize{
\item \code{AlphaTraceplot()}: Trace plot of alpha.

\item \code{AlphaPriorPosteriorPlot()}: Plot of the prior and posterior of alpha.

\item \code{ClusterTraceplot()}: Trace plot of the number of clusters.

\item \code{LikelihoodTraceplot()}: Trace plot of the likelihood of the data for
each iteration.

}}
\examples{
dp <- Fit(DirichletProcessGaussian(rnorm(10)), 100)
DiagnosticPlots(dp)

}
