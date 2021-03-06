% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functional.R
\name{\%***\%}
\alias{\%***\%}
\title{(***) :: (a -> c) -> (a' -> c') -> ((a, a') -> (c, c'))}
\usage{
f \%***\% g
}
\arguments{
\item{f}{The function that is applyied to the first pair component}

\item{g}{The function that is applyied to the second pair component}
}
\value{
A function of arity one, whith an argument that must be a pair, that returns a pair. r((a,b)) = (f(a), g(b))
}
\description{
(***) :: (a -> c) -> (a' -> c') -> ((a, a') -> (c, c'))
}
\details{
This is the function instance's *** of Arrow.
}
\references{
Haskell's Control.Arrow
}

