#' @title Compile Transition Probabilities - TUM
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and multiplies them appropriately. This function is specific to the Lower Granite version of DABOM.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#'
#' @import dplyr tidyr purrr
#' @export
#' @return NULL
#' @examples compileTransProbs_TUM()

compileTransProbs_TUM = function(dabom_mod = NULL,
                                 parent_child = NULL) {

  stopifnot(!is.null(dabom_mod),
            !is.null(parent_child))

  # make sure dabom_mod is mcmc.list
  if(class(dabom_mod) == 'jagsUI') dabom_mod = dabom_mod$samples

  stopifnot(!is.null(dabom_mod),
            class(dabom_mod) %in% c('mcmc', 'mcmc.list'))

  trans_mat = as.matrix(dabom_mod,
                        iters = T,
                        chains = T) %>%
    as_tibble() %>%
    # pull out movement parameters
    select(CHAIN, ITER,
           starts_with("p_pop_"),
           starts_with("phi_"))

  trans_df = trans_mat %>%
    tidyr::pivot_longer(cols = -c(CHAIN, ITER),
                        names_to = "param",
                        values_to = "value") %>%
    mutate(origin = stringr::str_split(param, '\\[', simplify = T)[,2],
           origin = stringr::str_sub(origin, 1, 1)) %>%
    mutate(parent = stringr::str_split(param, '\\[', simplify = T)[,1],
           parent = stringr::str_remove(parent, '^p_pop_'),
           parent = stringr::str_remove(parent, '^phi_'),
           brnch_num = stringr::str_split(param, '\\,', simplify = T)[,2],
           brnch_num = stringr::str_remove(brnch_num, '\\]')) %>%
    mutate_at(vars(brnch_num),
              list(as.numeric)) %>%
    mutate(across(brnch_num,
                  replace_na,
                  1)) %>%
    left_join(parent_child %>%
                group_by(parent) %>%
                arrange(child_rkm) %>%
                mutate(brnch_num = 1:n()) %>%
                select(parent, child, brnch_num),
              by = c("parent", "brnch_num")) %>%
    mutate(child = if_else(is.na(child),
                           paste0(parent, '_bb'),
                           child)) %>%
    select(CHAIN, ITER, origin, child, value) %>%
    tidyr::pivot_wider(names_from = "child",
                       values_from = "value") %>%
    # multiply some probabilities together
    rowwise() %>%
    mutate(across(PEU,
                  ~ . * PES)) %>%
    mutate(across(c(ICL_bb, LNF, ICM),
                  ~ . * ICL)) %>%
    mutate(across(ICU,
                  ~ . * ICM)) %>%
    mutate(across(CHU,
                  ~ . * CHL)) %>%
    mutate(across(NAU,
                  ~ . * NAL)) %>%
    ungroup() %>%
    mutate(iter = 1:n()) %>%
    tidyr::pivot_longer(cols = -c(CHAIN, ITER,
                                  iter, origin),
                        names_to = "param",
                        values_to = "value") %>%
    select(chain = CHAIN,
           iter,
           origin,
           param,
           value)

  return(trans_df)
}
