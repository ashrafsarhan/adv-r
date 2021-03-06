% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/euclidean.R
\name{euclidean}
\alias{euclidean}
\title{Building a function to find the greatest common divisor of two numbers}
\usage{
euclidean(a, b)
}
\arguments{
\item{a}{an integer of original numbers}

\item{b}{an integer of original numbers}
}
\value{
a positive integer the greatest common divisor of two integers
}
\description{
This function calculates by using the method of Euclidian algorithm
}
\details{
This function runs a calculation by theory of the Euclidean algorithm
which calculates the greatest common divisor (GCD) of two natural numbers a and b.
The greatest common divisor g is the largest natural number that divides both a and b without leaving a remainder.
}
\examples{
euclidean(123612, 13892347912)
euclidean(123612, -13892347912)
euclidean(-100, 1000)
euclidean(100, 1000)
}
\author{
Yumeng Li, Mattias Karlsson, Ashraf Sarhan
}
\references{
\url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
}
\seealso{
\code{\link{dijkstra}}
}

