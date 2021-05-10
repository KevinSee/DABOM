#' @title Summarise Detection Probabilities
#'
#' @description Extracts the MCMC posteriors of detection probabilities for a DABOM model, and summarises the results.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list, where the detection parameter names all end with "\code{_p}".
#'
#' @inheritParams createDABOMcapHist
#'
#' @param cred_int_prob A numeric scalar in the interval (0,1) giving what higest posterior density portion of the posterior the credible interval should cover. The default value is 95\%.

#'
#' @import dplyr tidyr coda stringr
#' @export
#' @return a dataframe with number of tags detected, mean, median, mode, se, cv and credible intervals of detection probability posteriors.
#' @examples \dontrun{summariseDetectProbs()}

summariseDetectProbs = function(dabom_mod = NULL,
                                filter_ch = NULL,
                                cred_int_prob = 0.95) {

  stopifnot(!is.null(dabom_mod) |
              !is.null(filter_ch))

  if(class(dabom_mod) != 'mcmc.list') dabom_mod = as.mcmc.list(dabom_mod)

  if (sum(filter_ch$user_keep_obs == "") > 0 | sum(is.na(filter_ch$user_keep_obs)) > 0 ) {
    stop("user_keep_obs must be defined for each observation.")
  }
  filter_ch = filter_ch %>%
    filter(user_keep_obs)

  # convert mcmc.list to tibble
  dabom_df = as.matrix(dabom_mod,
                       iters = T,
                       chains = T) %>%
    as_tibble() %>%
    tidyr::pivot_longer(-(CHAIN:ITER),
                        names_to = "param",
                        values_to = "value") %>%
    # tidyr::gather(param, value, -CHAIN, -ITER) %>%
    group_by(CHAIN, param) %>%
    mutate(iter = 1:n()) %>%
    ungroup() %>%
    select(chain = CHAIN,
           iter,
           param,
           value)

  # summarize detection parameters
  detect_df = dabom_df %>%
    filter(grepl('_p$', param)) %>%
    group_by(param) %>%
    summarise(mean = mean(value),
              median = median(value),
              mode = estMode(value),
              sd = sd(value),
              lowerCI = coda::HPDinterval(coda::as.mcmc(value), prob = cred_int_prob)[,1],
              upperCI = coda::HPDinterval(coda::as.mcmc(value), prob = cred_int_prob)[,2],
              .groups = "drop") %>%
    mutate(across(c(mean, median, mode, sd),
              ~ ifelse(. < 0, 0, .))) %>%
    rename(node = param) %>%
    mutate(node = stringr::str_replace(node, '_p$', ''))

  # add number of unique tags sighted at each node
  detect_summ = filter_ch %>%
    group_by(node) %>%
    summarise(n_tags = n_distinct(tag_code)) %>%
    full_join(detect_df,
              by = "node") %>%
    mutate(n_tags = ifelse(is.na(n_tags), 0, n_tags)) %>%
    mutate(mode = ifelse(mean == 1 & median == 1, 1, mode))

  return(detect_summ)

}
