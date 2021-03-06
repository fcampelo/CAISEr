% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_caiser.R
\name{plot.CAISEr}
\alias{plot.CAISEr}
\title{plot.CAISEr}
\usage{
\method{plot}{CAISEr}(
  x,
  y = NULL,
  ...,
  latex = FALSE,
  reorder = FALSE,
  show.text = TRUE,
  digits = 3,
  layout = NULL
)
}
\arguments{
\item{x}{list object of class \emph{CAISEr}.}

\item{y}{unused. Included for consistency with generic \code{plot} method.}

\item{...}{other parameters to be passed down to specific
plotting functions (currently unused)}

\item{latex}{logical: should labels be formatted for LaTeX? (useful for
later saving using library \code{TikzDevice})}

\item{reorder}{logical: should the comparisons be reordered alphabetically?}

\item{show.text}{logical: should text be plotted?}

\item{digits}{how many significant digits should be used in text?}

\item{layout}{optional parameter to override the layout of the plots (see
\code{gridExtra::arrangeGrobs()} for details. The default layout is
\code{lay = rbind(c(1,1,1,1,1,1), c(1,1,1,1,1,1), c(2,2,2,3,3,3))}}
}
\value{
list containing (1) a list of of \code{ggplot2} objects generated, and
(2) a list of data frames used for the creation of the plots.
}
\description{
S3 method for plotting \emph{CAISEr} objects output by \code{\link[=run_experiment]{run_experiment()}}.
}
