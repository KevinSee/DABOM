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

estMode = function(x, na.rm = F) {
  d = density(x, na.rm = na.rm)
  d$x[which.max(d$y)]
}

