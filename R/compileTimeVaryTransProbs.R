#' @title Compile Time-Varying Transition Probabilities
#'
#' @description Extracts the MCMC posteriors of time-varying transition probabilities for a DABOM model. The time-varying parameters should be set up so that they are organized as an array with the first dimension corresponding to the fish origin, the second to the model branch, and the third the model time strata.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr stringr
#' @export
#' @return NULL
#' @examples compileTimeVaryTransProbs()

compileTimeVaryTransProbs = function(dabom_mod = NULL,
                                 parent_child = NULL) {

  stopifnot(!is.null(dabom_mod),
            !is.null(parent_child))

  # make sure dabom_mod is mcmc.list
  if(class(dabom_mod) == 'jagsUI') dabom_mod = dabom_mod$samples

  stopifnot(!is.null(dabom_mod),
            class(dabom_mod) %in% c('mcmc', 'mcmc.list'))

  # determine root node (starting point)
  root_node = parent_child %>%
    PITcleanr::buildNodeOrder() %>%
    filter(node_order == 1) %>%
    pull(node)

  trans_df = as.matrix(dabom_mod,
                        iters = T,
                        chains = T) %>%
    as_tibble() %>%
    # pull out movement parameters from root_node
    select(CHAIN, ITER,
           matches(root_node)) %>%
    tidyr::pivot_longer(cols = -c(CHAIN, ITER),
                        names_to = "param",
                        values_to = "value") %>%
    mutate(origin = stringr::str_split(param, '\\[', simplify = T)[,2],
           origin = stringr::str_sub(origin, 1, 1)) %>%
    mutate(brnch_num = stringr::str_split(param, '\\,', simplify = T)[,2],
           strata_num = stringr::str_split(param, '\\,', simplify = T)[,3]) %>%
    mutate(across(c(brnch_num, strata_num),
                  ~ stringr::str_remove(., "\\]")),
           across(c(brnch_num, strata_num, origin),
                  as.integer)) %>%
    filter(!is.na(strata_num))

  return(trans_df)

}
