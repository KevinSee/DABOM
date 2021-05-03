#' @title Estimate Escapement - LGD
#'
#' @description Combines estimates of total escapement from STADEM with transition probabilities from DABOM to generate estimates of escapement above various detection sites. Currently only works for wild fish.
#'
#' @author Kevin See
#'
#' @param dabom_mod The result of DABOM. An MCMC.list, where the detection parameter names all end with "\code{_p}".
#' @param stadem_mod The result of STADEM. An MCMC.list, or \code{jagsUI} object that can be coerced to an MCMC.list.
#' @param stadem_param_nm Character vector of length one, with the name of the parameter corresponding to total escapement past Lower Granite Dam.
#' @param bootstrap_samp The number of samples to be drawn from the posteriors of the STADEM model and the DABOM model.
#'
#' @import dplyr tidyr stringr
#' @export
#' @return NULL
#' @examples #calcBranchEscape_GRA()

calcBranchEscape_GRA = function(dabom_mod = NULL,
                              stadem_mod = NULL,
                              stadem_param_nm = 'X.new.wild',
                              bootstrap_samp = 2000) {

  stopifnot(!is.null(dabom_mod) ,
            !is.null(stadem_mod))


  if(class(stadem_mod) == 'jagsUI') {
    stadem_mod = stadem_mod$samples
  }

  stadem_df = as.matrix(stadem_mod,
                        iters = T,
                        chains = T) %>%
    as_tibble() %>%
    select(CHAIN, ITER, matches(stadem_param_nm)) %>%
    group_by(CHAIN) %>%
    mutate(ITER = 1:n()) %>%
    ungroup() %>%
    tidyr::pivot_longer(cols = -c(CHAIN, ITER),
                        names_to = "param",
                        values_to = "value") %>%
    mutate(strata_num = stringr::str_extract(param, '[:digit:]+'),
           strata_num = as.integer(strata_num)) %>%
    group_by(param) %>%
    mutate(iter = 1:n()) %>%
    ungroup() %>%
    select(iter, strata_num, tot_escape = value)


  move_prob = compileTimeVaryTransProbs(dabom_mod,
                                        parent_child) %>%
    filter(origin == 1)

  escape_post = move_prob %>%
    group_by(param) %>%
    sample_n(size = bootstrap_samp,
             replace = T) %>%
    mutate(iter = 1:n()) %>%
    ungroup() %>%
    left_join(stadem_df %>%
                group_by(strata_num) %>%
                sample_n(size = bootstrap_samp,
                         replace = T) %>%
                mutate(iter = 1:n()) %>%
                ungroup(),
              by = c("strata_num", "iter")) %>%
    mutate(brnch_escp = tot_escape * value) %>%
    group_by(origin, brnch_num, iter) %>%
    summarize(across(brnch_escp,
                     sum),
              .groups = "drop")

  return(escape_post)
}
