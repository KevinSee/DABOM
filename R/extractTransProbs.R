#' @title Extract Transition Probabilities
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr purrr
#' @importFrom PITcleanr buildNodeOrder
#' @export
#' @return NULL
#' @examples extractTransProbs()

extractTransProbs = function(dabom_mod = NULL,
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
           starts_with("psi_"),
           starts_with("phi_"))

  trans_df = trans_mat %>%
    tidyr::pivot_longer(cols = -c(CHAIN, ITER),
                        names_to = "param",
                        values_to = "value") %>%
    mutate(origin = stringr::str_split(param, '\\[', simplify = T)[,2],
           origin = stringr::str_sub(origin, 1, 1)) %>%
    mutate(parent = stringr::str_split(param, '\\[', simplify = T)[,1],
           parent = stringr::str_remove(parent, '^p_pop_'),
           parent = stringr::str_remove(parent, '^psi_'),
           parent = stringr::str_remove(parent, '^phi_'),
           brnch_num = stringr::str_split(param, '\\,', simplify = T)[,2],
           brnch_num = stringr::str_remove(brnch_num, '\\]')) %>%
    mutate_at(vars(brnch_num, origin),
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
                           child))

  root_site = PITcleanr::buildNodeOrder(parent_child) %>%
    filter(node_order == 1) %>%
    pull(node)

  # if time-varying, fix all initial transition probabilities > 0 to 1 for multiplication purposes
  tv = trans_df %>%
    filter(parent == root_site) %>%
    mutate(n_comma = stringr::str_count(param, "\\,"),
           n_comma = as.integer(n_comma)) %>%
    select(n_comma) %>%
    distinct() %>%
    mutate(tv = if_else(n_comma > 1, T, F)) %>%
    pull(tv)
  if(length(tv) == 0) {
    tv = FALSE
  }
  if(tv) {
    trans_df = trans_df %>%
      filter(parent == root_site) %>%
      mutate(across(value,
                    ~ if_else(. > 0, 1, .))) %>%
      select(CHAIN, ITER, origin, child, value) %>%
      distinct() %>%
      bind_rows(trans_df %>%
                  filter(parent != root_site) %>%
                  select(CHAIN, ITER, origin, child, value))
  } else {
    trans_df = trans_df %>%
      select(CHAIN, ITER, origin, child, value)
  }

  return(trans_df)
}
