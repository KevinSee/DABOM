#' @title Estimate Mode of Posterior Distribution
#'
#' @description Estimates the mode of the posterior, by calculating the density of posterior samples and choosing x-value with the maximum y-value.
#'
#' @author Kevin See
#'
#' @param x numeric vector
#'
#' @export
#' @return NULL
#' @examples estMode(rgamma(1000, 5, 1/6))

estMode = function(x) {
  d = density(x)
  d$x[which.max(d$y)]

  # this is test comment

}

