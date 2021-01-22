#' @title Summarise Transition Probabilities - LGD
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and summarises the results. This function is specific to the Lower Granite version of DABOM.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list, where the detection parameter names all end with "\code{_p}".
#' @param cred_int_prob A numeric scalar in the interval (0,1) giving what higest posterior density portion of the posterior the credible interval should cover. The default value is 95\%.
#' @inheritParams compileTransProbs_LGD
#'
#' @import dplyr tidyr coda stringr
#' @export
#' @return NULL

summariseTransProbs_LGD = function(dabom_mod = NULL,
                                   time_varying = TRUE,
                                   cred_int_prob = 0.95) {

  stopifnot(!is.null(dabom_mod))

  # estimate the credible interval for each parameter
  credInt = coda::HPDinterval(dabom_mod, prob = cred_int_prob) %>%
    as.data.frame() %>%
    mutate(param = rownames(.)) %>%
    rename(lowerCI = lower,
           upperCI = upper) %>%
    tbl_df() %>%
    select(param, everything()) %>%
    mutate(param = renameTransParams_LGD(param))

  trans_df = compileTransProbs_LGD(dabom_mod,
                                   time_varying = time_varying) %>%
    group_by(param) %>%
    summarise(mean = mean(value),
              median = median(value),
              mode = estMode(value),
              sd = sd(value)) %>%
    mutate_at(vars(mean, median, mode, sd),
              funs(ifelse(. < 0, 0, .))) %>%
    left_join(credInt)

  return(trans_df)

}
