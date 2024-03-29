% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summariseDetectProbs.R
\name{summariseDetectProbs}
\alias{summariseDetectProbs}
\title{Summarise Detection Probabilities}
\usage{
summariseDetectProbs(dabom_mod = NULL, filter_ch = NULL, .cred_int_prob = 0.95)
}
\arguments{
\item{dabom_mod}{An MCMC.list containing some parameters that end with "_p"}

\item{filter_ch}{filtered capture history as returned by the `filterDetections()`
function in the `PITcleanr` package, which has then been verified by a user and all
blank or NA `user_keep_obs` entries have been completed.}
}
\value{
a dataframe with number of tags detected, mean, median, mode, se, cv and credible intervals of detection probability posteriors.
}
\description{
Extracts the MCMC posteriors of detection probabilities for a DABOM model, and summarizes the results.
}
\examples{
\dontrun{summariseDetectProbs()}
}
\author{
Kevin See
}
