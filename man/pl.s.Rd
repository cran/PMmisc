% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pl.s.R
\name{pl.s}
\alias{pl.s}
\title{Plot scatter plots for a data frame}
\usage{
pl.s(x,dependent,l)
}
\arguments{
\item{x}{:a data frame, which includes the dependent variable}

\item{dependent}{:the dependent variable for scatter pl.s}

\item{l}{: the beginning label number in the title (default set to 1)}
}
\description{
Plotting scatter plots for a data frame, with titles and label numbers.
}
\examples{
pl.s(mtcars,"mpg",1)
}
