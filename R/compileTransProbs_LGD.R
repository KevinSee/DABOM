#' @title Compile Transition Probabilities - LGD
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and multiplies them appropriately. This function is specific to the Lower Granite version of DABOM.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#' @param time_varying Should the initial movement probabilities be time-varying? Default value is \code{TRUE}
#'
#' @import dplyr tidyr
#' @export
#' @return NULL
#' @examples compileTransProbs_LGD()

compileTransProbs_LGD = function(dabom_mod = NULL,
                                 time_varying = TRUE) {

  stopifnot(!is.null(dabom_mod))

  trans_mat = as.matrix(dabom_mod,
                        iters = T,
                        chains = T) %>%
    as.data.frame() %>%
    tbl_df() %>%
    # remove detection parameters
    select(-matches('_p$'))

  # drop first branch tranistions if time-varying
  if(time_varying) {
    first_week_pos = grep('p_pop_main\\[1,', names(trans_mat))

    # force initial transitions to all be 1, for multiplication purposes
    trans_mat = trans_mat %>%
      mutate_at(vars(first_week_pos),
                funs(if_else(. == 0, 0, 1)))

    trans_mat = trans_mat %>%
      select(-matches('p_pop_main'), first_week_pos)
  }

  # change names of paramters
  names(trans_mat) = renameTransParams_LGD(names(trans_mat))

  # multiply some probabilities together
  trans_df = trans_mat %>%
    rowwise() %>%
    mutate(past_MTR = Tucannon * past_MTR,
                  past_UTR = past_MTR * past_UTR,
                  past_TUCH = past_UTR * past_TUCH) %>%
    rename(past_TUCH_TFH = past_TUCH) %>%
    mutate_at(vars(Asotin_bb, GEORGC, past_ASOTIC),
                     funs(. * Asotin)) %>%
    mutate(past_ACB = past_ASOTIC * past_ACB) %>%
    mutate_at(vars(ACB_bb, past_CCA, past_AFC),
                     funs(. * past_ACB)) %>%
    mutate_at(vars(Lapwai_bb, past_MIS, past_SWT),
                     funs(. * Lapwai)) %>%
    mutate(past_WEB = past_SWT * past_WEB) %>%
    mutate_at(vars(Potlatch_bb, past_KHS, past_PCM, past_HLM),
                     funs(. * Potlatch)) %>%
    mutate_at(vars(KHS_bb, BIGBEC, LBEARC),
                     funs(. * past_KHS)) %>%
    mutate_at(vars(HLM_bb, POTREF, POTRWF),
                     funs(. * past_HLM)) %>%
    mutate(past_FISTRP = Lochsa * past_FISTRP) %>%
    mutate(past_JOSEPC = JosephCreek * past_JOSEPC) %>%
    mutate_at(vars(ImnahaRiver_bb, HORS3C, past_CMP, LSHEEF, past_BSC, past_IR3),
                     funs(. * ImnahaRiver)) %>%
    mutate_at(vars(IR3_bb, FREEZC, past_CZY, MAHOGC, past_IR4),
                     funs(. * past_IR3)) %>%
    mutate(past_IML = past_IR4 * past_IML,
                  past_IR5 = past_IML * past_IR5) %>%
    mutate_at(vars(IR5_bb, GUMBTC, DRY2C),
                     funs(. * past_IR5)) %>%
    mutate_at(vars(Wallowa_bb, BCANF, past_WR2),
                     funs(. * Wallowa)) %>%
    mutate_at(vars(LOSTIW, WALH),
              funs(. * past_WR2)) %>%
    mutate_at(vars(GrandeRonde_bb, CATHEW, GRANDW),
                     funs(. * GrandeRonde)) %>%
    mutate_at(vars(SFSalmon_bb, past_ZEN, past_ESS, past_KRS),
                     funs(. * SFSalmon)) %>%
    mutate_at(vars(ESS_bb, JOHNSC, past_YPP),
                     funs(. * past_ESS)) %>%
    mutate(past_LAKEC = past_ZEN * past_LAKEC,
                  JOHNSC = past_ESS * JOHNSC,
                  past_STR = past_KRS * past_STR) %>%
    mutate_at(vars(Lemhi_bb:past_LRW),
                     funs(. * Lemhi)) %>%
    mutate_at(vars(LRW_bb:past_HEC),
                     funs(. * past_LRW)) %>%
    mutate(past_BTM = past_BTC * past_BTM,
                  past_BTU = past_BTM * past_BTU) %>%
    mutate(past_USI = UpperSalmon * past_USI) %>%
    mutate_at(vars(USI_bb:past_STL),
                     funs(. * past_USI)) %>%
    tidyr::gather(param, value, -CHAIN, -ITER) %>%
    group_by(CHAIN, param) %>%
    mutate(iter = 1:n()) %>%
    ungroup() %>%
    select(chain = CHAIN,
                  iter,
                  param,
                  value)

  return(trans_df)
}
