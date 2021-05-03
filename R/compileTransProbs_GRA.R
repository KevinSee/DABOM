#' @title Compile Transition Probabilities - GRA
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
#' @examples compileTransProbs_GRA()

compileTransProbs_GRA = function(dabom_mod = NULL,
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
                           child))
  # if time-varying, fix all initial transition probabilities > 0 to 1 for multiplication purposes
  tv = trans_df %>%
    filter(parent == 'GRA') %>%
    mutate(n_comma = str_count(param, "\\,"),
           n_comma = as.integer(n_comma)) %>%
    select(n_comma) %>%
    distinct() %>%
    mutate(tv = if_else(n_comma > 1, T, F)) %>%
    pull(tv)
  if(tv) {
    trans_df = trans_df %>%
      filter(parent == "GRA") %>%
      mutate(across(value,
                    ~ if_else(. > 0, 1, .))) %>%
      select(CHAIN, ITER, origin, child, value) %>%
      distinct() %>%
      bind_rows(trans_df %>%
                  filter(parent != "GRA") %>%
                  select(CHAIN, ITER, origin, child, value))
  } else {
    trans_df = trans_df %>%
      select(CHAIN, ITER, origin, child, value)
  }

  trans_df = trans_df %>%
    tidyr::pivot_wider(names_from = "child",
                       values_from = "value") %>%
    # multiply some probabilities together
    rowwise() %>%
    mutate(across(MTR,
                  ~ . * LTR)) %>%
    mutate(across(UTR,
                  ~ . * MTR)) %>%
    mutate(across(TFH,
                  ~ . * UTR)) %>%
    mutate(across(c(ACM_bb, GEORGC, ASOTIC),
                  ~ . * ACM)) %>%
    mutate(across(ACB,
                  ~ . * ASOTIC)) %>%
    mutate(across(c(ACB_bb, CCA, AFC),
                  ~ . * ACB)) %>%
    mutate(across(c(LAP_bb, MIS, SWT),
                  ~ . * LAP)) %>%
    mutate(across(WEB,
                  ~ . * SWT)) %>%
    mutate(across(c(JUL_bb, KHS, PCM, HLM),
                  ~ . * JUL)) %>%
    mutate(across(c(KHS_bb, BIGBEC, LBEARC),
                  ~ . * KHS)) %>%
    mutate(across(c(HLM_bb, POTREF, POTRWF),
                  ~ . * HLM)) %>%
    mutate(across(FISTRP,
                  ~ . * LRL)) %>%
    mutate(across(JOSEPC,
                  ~ . * JOC)) %>%
    mutate(across(c(IR1_bb, HORS3C, CMP, LSHEEF, BSC, IR3),
                  ~ . * IR1)) %>%
    mutate(across(c(IR3_bb, FREEZC, CZY, MAHOGC, IR4),
                  ~ . * IR3)) %>%
    mutate(across(IML,
                  ~ . * IR4)) %>%
    mutate(across(IR5,
                  ~ . * IML)) %>%
    mutate(across(c(IR5_bb, GUMBTC, DRY2C),
                  ~ . * IR5)) %>%
    mutate(across(c(WR1_bb, BCANF, WR2),
                  ~ . * WR1)) %>%
    mutate(across(c(LOSTIW, WALH),
                  ~ . * WR2)) %>%
    mutate(across(c(UGR_bb, UGS, CCW),
                  ~ . * UGR)) %>%
    mutate(across(c(SFG_bb, ZEN, ESS, KRS),
                  ~ . * SFG)) %>%
    mutate(across(c(ESS_bb, JOHNSC, YPP),
                  ~ . * ESS)) %>%
    mutate(across(LAKEC,
                  ~ . * ZEN)) %>%
    mutate(across(STR,
                  ~ . * KRS)) %>%
    mutate(across(c(LLR_bb, KEN, LRW, HYC, AGC, WPC, BHC),
                  ~ . * LLR)) %>%
    mutate(across(c(LRW_bb, LLS, LBS, BTL, CAC, HEC, LB8, LCL),
                  ~ . * LRW)) %>%
    mutate(across(BTM,
                  ~ . * BTL)) %>%
    mutate(across(BTU,
                  ~ . * BTM)) %>%
    mutate(across(USI,
                  ~ . * USE)) %>%
    mutate(across(c(USI_bb, STL, PAHH, SALEFT, YFK, VC1, RFL),
                  ~ . * USI)) %>%
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
