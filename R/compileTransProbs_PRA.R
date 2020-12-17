#' @title Compile Transition Probabilities - PRA
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
#' @examples compileTransProbs_PRA()

compileTransProbs_PRA = function(dabom_mod = NULL) {

  if(class(dabom_mod) == 'jagsUI') dabom_mod = dabom_mod$samples

  stopifnot(!is.null(dabom_mod),
            class(dabom_mod) %in% c('mcmc', 'mcmc.list'))

  trans_mat = as.matrix(dabom_mod,
                        iters = T,
                        chains = T) %>%
    as.data.frame() %>%
    tbl_df() %>%
    # remove detection parameters
    select(-matches('_p$'))

  trans_w = trans_mat %>%
    select(CHAIN, ITER, matches('\\[1'))
  trans_h = trans_mat %>%
    select(CHAIN, ITER, matches('\\[2'))
  # change names of paramters
  names(trans_w) = renameTransParams_PRA(names(trans_w))
  names(trans_h) = renameTransParams_PRA(names(trans_h))


  # multiply some probabilities together
  trans_list = list('Natural' = trans_w,
                    'Hatchery' = trans_h) %>%
    purrr::map(.f = function(x) {
      x %>%
        rowwise() %>%
        mutate_at(vars(RIA_bb, past_LWE, past_RRF),
                  funs(. * past_RIA)) %>%
        mutate_at(vars(LWE_bb, past_MCL, past_PES, past_CHM, past_ICL, past_TUM),
                  funs(. * past_LWE)) %>%
        mutate(past_PEU = past_PEU * past_PES) %>%
        mutate_at(vars(ICL_bb, past_LNF, past_ICM),
                  funs(. * past_ICL)) %>%
        mutate(past_ICU = past_ICU * past_ICM) %>%
        mutate_at(vars(TUM_bb, past_CHW, past_CHL, past_UWE),
                  funs(. * past_TUM)) %>%
        mutate(past_CHU = past_CHU * past_CHL) %>%
        mutate_at(vars(UWE_bb, past_NAL, past_LWN, past_WTL),
                  funs(. * past_UWE)) %>%
        mutate(past_NAU = past_NAU * past_NAL) %>%
        mutate_at(vars(RRF_bb, past_ENL, past_WEA, past_WEH),
                  funs(. * past_RRF)) %>%
        mutate_at(vars(ENL_bb, past_RCT, past_EHL, past_MAD, past_ENA),
                  funs(. * past_ENL)) %>%
        mutate(past_ENF = past_ENF * past_ENA) %>%
        mutate_at(vars(WEA_bb, past_LMR, past_OKL, past_FST),
                  funs(. * past_WEA)) %>%
        mutate_at(vars(LMR_bb, past_GLC, past_LBC, past_MRC),
                  funs(. * past_LMR)) %>%
        mutate_at(vars(MRC_bb, past_TWR, past_BVC, past_SCP, past_MSH, past_MRW, past_CRW),
                  funs(. * past_MRC)) %>%
        mutate(past_WFC = past_WFC * past_MRW,
               past_CRU = past_CRU * past_CRW,
               past_METH = past_MSH * past_METH,
               past_TWISPW = past_TWR * past_TWISPW) %>%
        mutate_at(vars(OKL_bb, past_LLC, past_SA1, past_JOH, past_AEN, past_OMK, past_WAN, past_TNK, past_BPC, past_ANT, past_WHS, past_ZSL),
                  funs(. * past_OKL)) %>%
        mutate(past_OBF = past_OBF * past_OMK,
               past_SA0 = past_SA0 * past_SA1) %>%
        mutate_at(vars(ZSL_bb, past_TON, past_NMC, past_OKI, past_OKC),
                  funs(. * past_ZSL)) %>%
        mutate(past_OKV = past_OKV * past_OKC) %>%
        mutate_at(vars(BelowJD1, past_JD1, past_TMF, past_PRV, past_ICH, past_PRO, past_RSH, past_PRH),
                  funs(. * dwnStrm)) %>%
        mutate_at(vars(PRV_bb, past_HST, past_MDR),
                  funs(. * past_PRV)) %>%
        ungroup()
    })

  trans_df = trans_list %>%
    purrr::map_df(.id = 'Origin',
                  .f = function(x) {
                    x %>%
                      mutate(iter = 1:n()) %>%
                      tidyr::gather(param, value, -CHAIN, -ITER, -iter) %>%
                      select(chain = CHAIN,
                             iter,
                             param,
                             value)
                  })


  return(trans_df)
}
