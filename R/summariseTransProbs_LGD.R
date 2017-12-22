#' @title Summarise Transition Probabilities - LGD
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and summarises the results. This function is specific to the Lower Granite version of DABOM.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list, where the detection parameter names all end with "\code{_p}".
#' @param cred_int_prob A numeric scalar in the interval (0,1) giving what higest posterior density portion of the posterior the credible interval should cover. The default value is 95%.
#'
#' @import dplyr tidyr MCMCglmm coda stringr
#' @export
#' @return NULL
#' @examples summariseTransProbs_LGD()

summariseTransProbs_LGD = function(dabom_mod = NULL,
                                   cred_int_prob = 0.95) {

  stopifnot(!is.null(dabom_mod))

  # estimate the credible interval for each parameter
  credInt = coda::HPDinterval(dabom_mod, prob = cred_int_prob) %>%
    as.data.frame() %>%
    dplyr::mutate(param = rownames(.)) %>%
    dplyr::rename(lowerCI = lower,
                  upperCI = upper) %>%
    dplyr::tbl_df() %>%
    dplyr::select(param, dplyr::everything()) %>%
    mutate(param = renameTransParams_LGD(param))

  trans_df = compileTransProbs_LGD(dabom_mod) %>%
    dplyr::group_by(param) %>%
    dplyr::summarise(mean = mean(value),
                     median = median(value),
                     mode = MCMCglmm::posterior.mode(value),
                     sd = sd(value)) %>%
    dplyr::mutate_at(vars(mean, median, mode, sd),
                     funs(ifelse(. < 0, 0, .))) %>%
    dplyr::left_join(credInt)

  return(trans_df)

}
