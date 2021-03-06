% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createDABOMcapHist.R
\name{createDABOMcapHist}
\alias{createDABOMcapHist}
\title{Prep DABOM capture histories}
\usage{
createDABOMcapHist(
  filter_ch = NULL,
  parent_child = NULL,
  configuration = NULL,
  split_matrices = F
)
}
\arguments{
\item{filter_ch}{filtered capture history as returned by the `filterDetections()`
function in the `PITcleanr` package, which has then been verified by a user and all
blank or NA `user_keep_obs` entries have been completed.}

\item{parent_child}{data frame with at least `parent` and `child` columns.
Can be created with `buildParentChild()` function in the `PITcleanr` package.}

\item{configuration}{is a data frame which assigns node names to unique SiteID, AntennaID, and
site configuration ID combinations. One example can be built with the function `buildConfig`}

\item{split_matrices}{Should the wide capture history be split into separate matrices,
one for each group in `defineDabomColNms`? Default value is \code{FALSE}.}
}
\description{
Take the processed capture histories (in long format) and the groupings of node,
and develop a capture history in wide format. Optionally, split that capture history into
smaller dataframes by grouping.
}
\examples{
createDABOMcapHist()
}
\author{
Kevin See
}
