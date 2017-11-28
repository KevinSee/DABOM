#' @title Compile Transition Probabilities - LGD
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and multiplies them appropriately. This function is specific to the Lower Granite version of DABOM.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#'
#' @import dplyr tidyr
#' @export
#' @return NULL
#' @examples compileTransProbs_LGD()

compileTransProbs_LGD = function(dabom_mod = NULL) {

  stopifnot(!is.null(dabom_mod))

  trans_mat = as.matrix(dabom_mod,
                        iters = T,
                        chains = T) %>%
    as.data.frame() %>%
    dplyr::tbl_df() %>%
    # remove detection parameters
    select(-matches('_p$'))

  # change names of paramters
  names(trans_mat) = renameTransParams_LGD(names(trans_mat))

  # multiply some probabilities together
  trans_df = trans_mat %>%
    dplyr::rowwise() %>%
    dplyr::mutate(past_MTR = Tucannon * past_MTR,
                  past_UTR = past_MTR * past_UTR,
                  past_TUCH = past_UTR * past_TUCH) %>%
    dplyr::mutate_at(vars(Asotin_bb, GEORGC, past_ASOTIC),
                     funs(. * Asotin)) %>%
    dplyr::mutate(past_ACB = past_ASOTIC * past_ACB) %>%
    dplyr::mutate_at(vars(ACB_bb, past_CCA, past_AFC),
                     funs(. * past_ACB)) %>%
    dplyr::mutate_at(vars(Lapwai_bb, past_MIS, past_SWT),
                     funs(. * Lapwai)) %>%
    dplyr::mutate(past_WEB = past_SWT * past_WEB) %>%
    dplyr::mutate_at(vars(Potlatch_bb, past_KHS, past_PCM, past_HLM),
                     funs(. * Potlatch)) %>%
    dplyr::mutate_at(vars(BIGBEC, LBEARC),
                     funs(. * past_KHS)) %>%
    dplyr::mutate_at(vars(POTREF, POTRWF),
                     funs(. * past_HLM)) %>%
    dplyr::mutate(past_JOSEPC = JosephCreek * past_JOSEPC) %>%
    dplyr::mutate_at(vars(Imnaha_bb, HORS3C, past_CMP, LSHEEF, past_BSC, past_IR3),
                     funs(. * ImnahaRiver)) %>%
    dplyr::mutate_at(vars(IR3_bb, FREEZC, past_CZY, MAHOGC, past_IR4),
                     funs(. * past_IR3)) %>%
    dplyr::mutate(past_IML = past_IR4 * past_IML,
                  past_IR5 = past_IML * past_IR5) %>%
    dplyr::mutate_at(vars(IR5_bb, GUMBTC, DRY2C),
                     funs(. * past_IR5)) %>%
    dplyr::mutate_at(vars(Wallowa_bb, BCANF, LOSTIW, WALH),
                     funs(. * Wallowa)) %>%
    dplyr::mutate_at(vars(GrandeRonde_bb, CATHEW, GRANDW),
                     funs(. * GrandeRonde)) %>%
    dplyr::mutate_at(vars(SFSalmon_bb, past_ZEN, past_ESS, past_KRS),
                     funs(. * SFSalmon)) %>%
    dplyr::mutate(past_LAKEC = past_ZEN * past_LAKEC,
                  past_JOHNSC = past_ESS * past_JOHNSC,
                  past_STR = past_KRS * past_STR) %>%
    dplyr::mutate_at(vars(Lemhi_bb:past_LRW),
                     funs(. * Lemhi)) %>%
    dplyr::mutate(past_HBC = past_HYC * past_HBC) %>%
    dplyr::mutate_at(vars(LRW_bb:past_18M),
                     funs(. * past_LRW)) %>%
    dplyr::mutate(past_BTM = past_BTC * past_BTM,
                  past_BTU = past_BTM * past_BTU) %>%
    dplyr::mutate(past_USI = UpperSalmon * past_USI) %>%
    dplyr::mutate_at(vars(USI_bb:past_STL),
                     funs(. * past_USI)) %>%
    tidyr::gather(param, value, -CHAIN, -ITER) %>%
    dplyr::group_by(CHAIN, param) %>%
    dplyr::mutate(iter = 1:n()) %>%
    dplyr::select(chain = CHAIN,
                  iter,
                  param,
                  value)

  return(trans_df)
}
