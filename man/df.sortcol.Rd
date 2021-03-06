% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df.sortcol.R
\name{df.sortcol}
\alias{df.sortcol}
\title{Sort a data frame by a column}
\usage{
df.sortcol(x,n,desc)
}
\arguments{
\item{x}{:a data frame}

\item{n}{:number column to sort}

\item{desc}{:the order of sorting, default set to TRUE; for ascending order set to FALSE}
}
\description{
Sort a data frame by a column of choice. The column of choice is specified by the number of the column.
}
\examples{
df.sortcol(mtcars,2,desc = TRUE)
}
