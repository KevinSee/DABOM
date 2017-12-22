#' @title Summarise Detection Probabilities
#'
#' @description Extracts the MCMC posteriors of detection probabilities for a DABOM model, and summarises the results.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list, where the detection parameter names all end with "\code{_p}".
#' @param capHist_proc Dataframe with the same format as that returned by \code{processCapHist_LGD}, under the name \code{ProcCapHist}. This is the data fed into DABOM as observations.
#' @param cred_int_prob A numeric scalar in the interval (0,1) giving what higest posterior density portion of the posterior the credible interval should cover. The default value is 95\\%.
#'
#' @import dplyr tidyr MCMCglmm coda stringr
#' @export
#' @return NULL
#' @examples summariseDetectProbs()

summariseDetectProbs = function(dabom_mod = NULL,
                                capHist_proc = NULL,
                                cred_int_prob = 0.95) {

  stopifnot(!is.null(dabom_mod) |
              !is.null(capHist_proc))

  dabom_mod = as.mcmc.list(dabom_mod)

  # estimate the credible interval for each parameter
  credInt = coda::HPDinterval(dabom_mod, prob = cred_int_prob) %>%
    as.data.frame() %>%
    dplyr::mutate(param = rownames(.)) %>%
    dplyr::rename(lowerCI = lower,
                  upperCI = upper) %>%
    dplyr::tbl_df() %>%
    dplyr::select(param, dplyr::everything())


  dabom_df = as.matrix(dabom_mod,
                       iters = T,
                       chains = T) %>%
    as.data.frame() %>%
    dplyr::tbl_df() %>%
    tidyr::gather(param, value, -CHAIN, -ITER) %>%
    dplyr::group_by(CHAIN, param) %>%
    dplyr::mutate(iter = 1:n()) %>%
    dplyr::select(chain = CHAIN,
                  iter,
                  param,
                  value)

  detect_df = dabom_df %>%
    dplyr::filter(grepl('_p$', param)) %>%
    dplyr::group_by(param) %>%
    dplyr::summarise(mean = mean(value),
                     median = median(value),
                     mode = MCMCglmm::posterior.mode(value),
                     sd = sd(value)) %>%
    dplyr::mutate_at(vars(mean, median, mode, sd),
                     funs(ifelse(. < 0, 0, .))) %>%
    dplyr::left_join(credInt) %>%
    dplyr::rename(Node = param) %>%
    dplyr::mutate(Node = stringr::str_replace(Node, '_p$', ''))

  # add number of unique tags sighted at each node
  detect_summ = capHist_proc %>%
    dplyr::group_by(Node) %>%
    dplyr::summarise(n_tags = n_distinct(TagID)) %>%
    full_join(detect_df)

  return(detect_summ)

}
