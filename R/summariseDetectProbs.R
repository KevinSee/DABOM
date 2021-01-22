#' @title Summarise Detection Probabilities
#'
#' @description Extracts the MCMC posteriors of detection probabilities for a DABOM model, and summarises the results.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list, where the detection parameter names all end with "\code{_p}".
#'
#' @param capHist_proc Dataframe with the same format as that returned by \code{processCapHist_LGD}, under the name \code{ProcCapHist}. This is the data fed into DABOM as observations.
#'
#' @param cred_int_prob A numeric scalar in the interval (0,1) giving what higest posterior density portion of the posterior the credible interval should cover. The default value is 95\%.

#'
#' @import dplyr tidyr coda stringr
#' @export
#' @return a dataframe with number of tags detected, mean, median, mode, se, cv and credible intervals of detection probability posteriors.
#' @examples \dontrun{summariseDetectProbs()}

summariseDetectProbs = function(dabom_mod = NULL,
                                capHist_proc = NULL,
                                cred_int_prob = 0.95) {

  stopifnot(!is.null(dabom_mod) |
              !is.null(capHist_proc))

  if(class(dabom_mod) != 'mcmc.list') dabom_mod = as.mcmc.list(dabom_mod)

  if (sum(capHist_proc$UserProcStatus == "") > 0) {
    stop("UserProcStatus must be defined for each observation.")
  }
  capHist_proc = capHist_proc %>%
    filter(UserProcStatus)

  # convert mcmc.list to tibble
  dabom_df = as.matrix(dabom_mod,
                       iters = T,
                       chains = T) %>%
    as.data.frame() %>%
    tbl_df() %>%
    tidyr::gather(param, value, -CHAIN, -ITER) %>%
    group_by(CHAIN, param) %>%
    mutate(iter = 1:n()) %>%
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
              upperCI = coda::HPDinterval(coda::as.mcmc(value), prob = cred_int_prob)[,2]) %>%
    mutate(across(c(mean, median, mode, sd),
              ~ ifelse(. < 0, 0, .))) %>%
    rename(Node = param) %>%
    mutate(Node = stringr::str_replace(Node, '_p$', ''))

  # add number of unique tags sighted at each node
  detect_summ = capHist_proc %>%
    group_by(Node) %>%
    summarise(n_tags = n_distinct(TagID)) %>%
    full_join(detect_df) %>%
    mutate(n_tags = ifelse(is.na(n_tags), 0, n_tags)) %>%
    mutate(mode = ifelse(mean == 1 & median == 1, 1, mode))

  return(detect_summ)

}
