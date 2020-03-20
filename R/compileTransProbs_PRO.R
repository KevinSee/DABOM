#' @title Compile Transition Probabilities - PRO
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and multiplies them appropriately. This function is specific to the Prosser dam version of DABOM.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#' @param parent_child dataframe created from \code{PITcleanr::createParentChildDf} or similar that contains at least ParentNode, ChildNode and river kilometer columns
#'
#' @import dplyr tidyr stringr
#' @export
#' @return NULL
#' @examples compileTransProbs_PRO()

compileTransProbs_PRO = function(dabom_mod = NULL,
                                 parent_child = NULL) {

  stopifnot(!is.null(dabom_mod),
            !is.null(parent_child))

  # create parent-child table by site (instead of node)
  site_parent_child = parent_child %>%
    mutate(ParentSite = stringr::str_remove(ParentNode, 'B0$'),
           ParentSite = stringr::str_remove(ParentSite, 'A0$'),
           ChildSite = stringr::str_remove(ChildNode, 'B0$'),
           ChildSite = stringr::str_remove(ChildSite, 'A0$')) %>%
    filter(ParentSite != ChildSite) %>%
    select(ParentSite, ChildSite, SiteType, RKM, nodeOrder) %>%
    distinct() %>%
    group_by(ParentSite) %>%
    mutate(brnch_num = 1:n()) %>%
    ungroup()

  # make sure dabom_mod is mcmc.list
  if(class(dabom_mod) == 'jagsUI') dabom_mod = dabom_mod$samples

  stopifnot(!is.null(dabom_mod),
            class(dabom_mod) %in% c('mcmc', 'mcmc.list'))

  trans_mat = as.matrix(dabom_mod,
                        iters = T,
                        chains = T) %>%
    as_tibble() %>%
    # pull out movement parameters
    select(CHAIN, ITER, starts_with("p_pop_"), starts_with("phi_"))

  trans_df = trans_mat %>%
    tidyr::gather(param, value, -CHAIN, -ITER) %>%
    mutate(origin = stringr::str_split(param, '\\[', simplify = T)[,2],
           origin = stringr::str_sub(origin, 1, 1)) %>%
    mutate(ParentSite = stringr::str_split(param, '\\[', simplify = T)[,1],
           ParentSite = stringr::str_remove(ParentSite, '^p_pop_'),
           ParentSite = stringr::str_remove(ParentSite, '^phi_'),
           brnch_num = stringr::str_split(param, '\\,', simplify = T)[,2],
           brnch_num = stringr::str_remove(brnch_num, '\\]')) %>%
    mutate_at(vars(brnch_num),
              list(as.numeric)) %>%
    mutate(brnch_num = if_else(is.na(brnch_num),
                               1, brnch_num)) %>%
    left_join(site_parent_child %>%
                group_by(ParentSite) %>%
                mutate(brnch_num = 1:n()) %>%
                select(ParentSite, ChildSite, brnch_num)) %>%
    mutate(ChildSite = if_else(is.na(ChildSite),
                               paste0(ParentSite, '_bb'),
                               ChildSite)) %>%
    select(CHAIN, ITER, origin, ChildSite, value) %>%
    tidyr::spread(ChildSite, value) %>%
    # multiply some probabilities together
    rowwise() %>%
    mutate_at(vars(SM1, TP2, TOP_bb),
              list(~ . * TOP)) %>%
    mutate_at(vars(AH1, LNR, LWC, ROZ, SUN_bb),
              list(~ . * SUN)) %>%
    mutate_at(vars(LMC, TAN, SWK, LMT, ROZ_bb),
              list(~ . * ROZ)) %>%
    mutate_at(vars(UMC),
              list(~ . * LMC)) %>%
    ungroup() %>%
    mutate(iter = 1:n()) %>%
    tidyr::gather(param, value, -CHAIN, -ITER, -iter, -origin) %>%
    select(chain = CHAIN,
           iter,
           origin,
           param,
           value)

  return(trans_df)
}
