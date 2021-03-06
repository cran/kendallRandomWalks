% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulations_kendall_rw.R
\name{summarise_kendall_rw}
\alias{summarise_kendall_rw}
\title{Calculate some characteristic for every simulated instance.}
\usage{
summarise_kendall_rw(simulations, summary_function)
}
\arguments{
\item{simulations}{Object of class kendall_simulation.}

\item{summary_function}{Function that will be applied to each trajectory.}
}
\value{
data frame of class "kendall_summary".
}
\description{
Calculate some characteristic for every simulated instance.
}
