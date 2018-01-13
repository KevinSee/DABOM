#' @title Compile Weekly Transition Probabilities - LGD
#'
#' @description Extracts the MCMC posteriors of time-varying transition probabilities for a DABOM model. The time-varying parameters should be set up so that they are organized with rows corresponding to weeks, and columns corresponding to branches.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list.
#' @param param_name A character vector of length one, containing the name of a parameter that is time-varying by week. Default value is '\code{p_pop_main}'.
#'
#' @import dplyr tidyr stringr
#' @export
#' @return NULL
#' @examples compileWeekTransProbs()

compileWeekTransProbs = function(dabom_mod = NULL,
                                 param_name = 'p_pop_main') {

  stopifnot(!is.null(dabom_mod))

  trans_df = as.matrix(dabom_mod,
                        iters = T,
                        chains = T) %>%
    as.data.frame() %>%
    tbl_df() %>%
    select(CHAIN, ITER, matches(param_name)) %>%
    tidyr::gather(param, prob, -CHAIN, -ITER) %>%
    mutate(param_name = stringr::str_split(param, '\\[', simplify = T)[,1]) %>%
    mutate(week = stringr::str_split(param, '\\,', simplify = T)[,1],
           branch = stringr::str_split(param, '\\,', simplify = T)[,2]) %>%
    mutate_at(vars(week, branch),
              funs(stringr::str_extract(., '[:digit:]+'))) %>%
    mutate_at(vars(week, branch),
              funs(as.integer)) %>%
    select(CHAIN, ITER, param = param_name, week, branch, prob)

  return(trans_df)

}
