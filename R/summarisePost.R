#' @title Summarize Posteriorrs
#'
#' @description After the user has extracted posterior draws from an `mcmc` object and wrangled them into a long tibble, this function computes several summary statistics.
#'
#' @author Kevin See
#'
#' @param .data A data frame or data frame extension (e.g. a tibble) containing a column called `value`
#' @param value name of the column containing posterior values. Default is `value`.
#' @param ... <tidy-select> Columns to group by (e.g. `origin`, `param`, etc.)
#' #' @param .cred_int_prob A numeric scalar in the interval (0,1) giving what higest posterior density portion of the posterior the credible interval should cover. The default value is 95\%.

#'
#' @import dplyr moments coda
#' @export
#' @return NULL
#' @examples #summarisePost()

summarisePost <- function(.data,
                          value,
                          ...,
                          .cred_int_prob = 0.95){

  value <- enquo(value)

  sum_post <-
    .data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      mean = mean(!!value),
      median = median(!!value),
      mode = DABOM::estMode(!!value),
      sd = sd(!!value),
      skew = moments::skewness(!!value),
      kurtosis = moments::kurtosis(!!value),
      lower_ci = coda::HPDinterval(coda::as.mcmc(!!value), prob = .cred_int_prob)[,1],
      upper_ci = coda::HPDinterval(coda::as.mcmc(!!value), prob = .cred_int_prob)[,2],
      .groups = "drop") %>%
    dplyr::mutate(across(c(mean,
                           median,
                           mode,
                           sd,
                           lower_ci),
                         ~ if_else(. < 0, 0, .)))

  return(sum_post)
}
