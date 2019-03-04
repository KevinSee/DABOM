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

compileTransProbs_TUM = function(dabom_mod = NULL) {

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
  names(trans_w) = renameTransParams_TUM(names(trans_w))
  names(trans_h) = renameTransParams_TUM(names(trans_h))


  # multiply some probabilities together
  trans_list = list('Natural' = trans_w,
                    'Hatchery' = trans_h) %>%
    purrr::map(.f = function(x) {
      x %>%
        rowwise() %>%
        mutate(past_PEU = past_PEU * past_PES) %>%
        mutate_at(vars(ICL_bb, past_LNF, past_ICM),
                  funs(. * past_ICL)) %>%
        mutate(past_ICU = past_ICU * past_ICM) %>%
        mutate(past_CHU = past_CHU * past_CHL) %>%
        mutate(past_NAU = past_NAU * past_NAL) %>%
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
