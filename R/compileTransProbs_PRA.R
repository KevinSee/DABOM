#' @title Compile Transition Probabilities - PRA
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and multiplies them appropriately. This function is specific to the Priest Rapids version of DABOM.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr purrr
#' @export
#' @return NULL
#' @examples compileTransProbs_PRA()

compileTransProbs_PRA = function(dabom_mod = NULL,
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
                           child)) %>%
    select(CHAIN, ITER, origin, child, value) %>%
    tidyr::pivot_wider(names_from = "child",
                       values_from = "value") %>%
    # multiply some probabilities together
    rowwise() %>%
    mutate(across(c(RIA_bb, CLK, LWE, RRF),
              ~ . * RIA)) %>%
    mutate(across(c(LWE_bb, MCL, PES, CHM, ICL, TUM),
              ~ . * LWE)) %>%
    mutate(PEU = PEU * PES) %>%
    mutate(across(c(ICL_bb, LNF, ICM),
              ~ . * ICL)) %>%
    mutate(ICU = ICU * ICM) %>%
    mutate(across(c(TUM_bb, CHW, CHL, UWE),
              ~ . * TUM)) %>%
    mutate(CHU = CHU * CHL) %>%
    mutate(across(c(UWE_bb, NAL, LWN, WTL),
              ~ . * UWE)) %>%
    mutate(NAU = NAU * NAL) %>%
    mutate(across(any_of(RRF_bb, ENL, WEA, WEH, EBO),
              ~ . * RRF)) %>%
    mutate(across(c(ENL_bb, RCT, EHL, MAD, ENA),
              ~ . * ENL)) %>%
    mutate(ENF = ENF * ENA) %>%
    mutate(across(c(WEA_bb, LMR, OKL, FST),
              ~ . * WEA)) %>%
    mutate(across(c(LMR_bb, GLC, LBC, MRC),
              ~ . * LMR)) %>%
    mutate(across(c(MRC_bb, TWR, BVC, SCP, MSH, MRW, CRW),
              ~ . * MRC)) %>%
    mutate(WFC = WFC * MRW,
           CRU = CRU * CRW,
           # METH = MSH * METH,
           TWISPW = TWR * TWISPW) %>%
    mutate(across(c(OKL_bb, LLC, SA1, JOH, AEN, OMK, WAN, TNK, BPC, ANT, WHS, ZSL),
              ~ . * OKL)) %>%
    mutate(OBF = OBF * OMK,
           SA0 = SA0 * SA1) %>%
    mutate(across(c(ZSL_bb, TON, NMC, OKI, OKC),
              ~ . * ZSL)) %>%
    mutate(OKV = OKV * OKC) %>%
    mutate(across(c(PRV_bb, HST, MDR),
              ~ . * PRV)) %>%
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
