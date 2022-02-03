#' @title Compile Transition Probabilities - ASOTIC
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and multiplies them appropriately. This function is specific to the Asotin version of DABOM.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr purrr
#' @export
#' @return NULL
#' @examples compileTransProbs_ASOTIC()

compileTransProbs_ASOTIC = function(dabom_mod = NULL,
                                    parent_child = NULL) {

  trans_df <- extractTransProbs(dabom_mod,
                                parent_child)

  # multiply them appropriately
  trans_df = trans_df %>%
    tidyr::pivot_wider(names_from = "child",
                       values_from = "value") %>%
    # multiply some probabilities together
    rowwise() %>%
    mutate(across(c(ACB_bb, CCA, AFC),
                  ~ . * ACB)) %>%
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
