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
    as_tibble() %>%
    # remove detection parameters
    select(-matches('_p$'))

  if("deviance" %in% names(trans_mat)) {
    trans_mat = trans_mat %>%
      select(-deviance)
  }

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
    mutate(MTR = LTR * MTR,
           UTR = MTR * UTR,
           TUCH = UTR * TUCH) %>%
    rename(TUCH = TUCH) %>%
    mutate_at(vars(ACM_bb, GEORGC, ASOTIC),
              funs(. * ACM)) %>%
    mutate(ACB = ASOTIC * ACB) %>%
    mutate_at(vars(ACB_bb, CCA, AFC),
              funs(. * ACB)) %>%
    mutate_at(vars(LAP_bb, MIS, SWT),
              funs(. * LAP)) %>%
    mutate(WEB = SWT * WEB) %>%
    mutate_at(vars(JUL_bb, KHS, PCM, HLM),
              funs(. * JUL)) %>%
    mutate_at(vars(KHS_bb, BIGBEC, LBEARC),
              funs(. * KHS)) %>%
    mutate_at(vars(HLM_bb, POTREF, POTRWF),
              funs(. * HLM)) %>%
    mutate(FISTRP = LRL * FISTRP) %>%
    mutate(JOSEPC = JOC * JOSEPC) %>%
    mutate_at(vars(IR1_bb, HORS3C, CMP, LSHEEF, BSC, IR3),
              funs(. * IR1)) %>%
    mutate_at(vars(IR3_bb, FREEZC, CZY, MAHOGC, IR4),
              funs(. * IR3)) %>%
    mutate(IML = IR4 * IML,
           IR5 = IML * IR5) %>%
    mutate_at(vars(IR5_bb, GUMBTC, DRY2C),
              funs(. * IR5)) %>%
    mutate_at(vars(WR1_bb, BCANF, WR2),
              funs(. * WR1)) %>%
    mutate_at(vars(LOSTIW, WALH),
              funs(. * WR2)) %>%
    mutate_at(vars(UGR_bb, CATHEW, GRANDW),
              funs(. * UGR)) %>%
    mutate_at(vars(SFG_bb, ZEN, ESS, KRS),
              funs(. * SFG)) %>%
    mutate_at(vars(ESS_bb, JOHNSC, YPP),
              funs(. * ESS)) %>%
    mutate(LAKEC = ZEN * LAKEC,
           STR = KRS * STR) %>%
    mutate_at(vars(LLR_bb:LRW),
              funs(. * LLR)) %>%
    mutate_at(vars(LRW_bb:HEC),
              funs(. * LRW)) %>%
    mutate(BTM = BTC * BTM,
           BTU = BTM * BTU) %>%
    mutate(USI = USE * USI) %>%
    mutate_at(vars(USI_bb:STL),
              funs(. * USI)) %>%
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
