% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anova.R
\name{plot.avar}
\alias{plot.avar}
\title{Plot method for the ANOVA function}
\usage{
\method{plot}{avar}(x, plots = "TRUE", shiny = FALSE, custom = FALSE, ...)
}
\arguments{
\item{x}{Return value from \code{\link{pca}}}

\item{plots}{Plot to return.}

\item{shiny}{Did the function call originate inside a shiny app}

\item{custom}{Logical (TRUE, FALSE) to indicate if ggplot object (or list of ggplot objects) should be returned. This option can be used to customize plots (e.g., add a title, change x and y labels, etc.). See examples and \url{https://ggplot2.tidyverse.org/} for options.}

\item{...}{further arguments passed to or from other methods}
}
\description{
Plot method for the ANOVA function
}
\details{
Plotting interaction plot ...
}
