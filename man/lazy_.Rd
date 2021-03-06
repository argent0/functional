% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functional.R
\name{lazy_}
\alias{lazy_}
\title{creates a lazy value}
\usage{
lazy_(e, msg = "")
}
\arguments{
\item{e}{the expression that should be computed only when required}
}
\description{
creates a lazy value
}
\details{
You should use \code{lazy}. The value is computed only once and reused upon succesive calls.

The value is computed only once and reused upon succesive calls.
}
\seealso{
lazy
}

