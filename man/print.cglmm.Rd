% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.cglmm}
\alias{print.cglmm}
\title{Print a brief summary of the \code{cglmm} model.}
\usage{
\method{print}{cglmm}(x, digits = getOption("digits"), ...)
}
\arguments{
\item{x}{A \code{cglmm} object.}

\item{digits}{Controls the number of digits displayed in the summary output.}

\item{...}{Additional, ignored arguments.}
}
\value{
\code{print(x)} returns \code{x} invisibly.
}
\description{
Print a brief summary of the \code{cglmm} model.
}
\examples{
# Single component cosinor model
cglmm(
  vit_d ~ amp_acro(time_col = time, group = "X", period = 12),
  data = vitamind
)
}
