% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decision.R
\name{decision}
\alias{decision}
\title{Make decision}
\usage{
decision(p_value)
}
\arguments{
\item{p_value}{This is the p-value of the statistical test}
}
\value{
Decision in words as to whether the Null Hypothesis can be rejected
}
\description{
This function gives a decision as to whether there is enough evidence to reject the Null Hypothesis
}
\examples{
decision(p_value = 0.03)
}
