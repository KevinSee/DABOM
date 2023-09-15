#' @title Estimate Escapement - LGD
#'
#' @description Combines estimates of total escapement from STADEM with transition probabilities from DABOM to generate estimates of escapement above various detection sites. Currently only works for wild fish.
#'
#' @author Kevin See
#'
#' @inheritParams calcBranchEscape_GRA
#' @inheritParams createDABOMcapHist
#' @param summ_results Should the resulting posteriors be summarized or returned as posterior samples? Default value is \code{TRUE}.
#' @param cred_int_prob A numeric scalar in the interval (0,1) giving what highest posterior density portion of the posterior the credible interval should cover. The default value is 95\%.
#'
#' @import dplyr tidyr stringr
#' @export
#' @return NULL
#' @examples #calcTribEscape_GRA()

calcTribEscape_GRA = function(dabom_mod = NULL,
                              stadem_mod = NULL,
                              stadem_param_nm = 'X.new.wild',
                              bootstrap_samp = 2000,
                              parent_child = NULL,
                              summ_results = T,
                              cred_int_prob = 0.95) {

  stopifnot(!is.null(dabom_mod) ,
            !is.null(stadem_mod),
            !is.null(parent_child))

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

  # get movement probabilities of time-varying branches
  trans_df = compileTransProbs(dabom_mod,
                               parent_child)

  # what is the starting point?
  root_site = parent_child %>%
    PITcleanr::buildNodeOrder() %>%
    filter(node_order == 1) %>%
    pull(node)

  # is initial branch time-varying?
  tv = dabom_mod %>%
    as.matrix() %>%
    as_tibble() %>%
    select(matches(root_site)) %>%
    names() %>%
    enframe(value = "param") %>%
    mutate(n_comma = stringr::str_count(param, "\\,"),
           n_comma = as.integer(n_comma)) %>%
    select(n_comma) %>%
    distinct() %>%
    mutate(tv = if_else(n_comma > 1, T, F)) %>%
    pull(tv)

  if(tv) {
    # figure out which initial branch each site (parameter) is part of
    param_branch = parent_child %>%
      buildNodeOrder() %>%
      rename(site_code = node) %>%
      add_column(brnch_num = NA_integer_)

    branch_site = parent_child %>%
      filter(parent == root_site) %>%
      group_by(parent) %>%
      arrange(child_rkm) %>%
      mutate(brnch_num = 1:n()) %>%
      ungroup() %>%
      select(bottom_site = child,
             brnch_num)


    for(i in 1:nrow(branch_site)) {
      param_branch %<>%
        mutate(brnch_num = if_else(stringr::str_detect(path, branch_site$bottom_site[i]),
                                   branch_site$brnch_num[i],
                                   brnch_num))
    }
    param_branch %<>%
      mutate(brnch_num = if_else(node_order == 1,
                                 max(branch_site$brnch_num) + as.integer(1),
                                 brnch_num)) %>%
      rename(in_path = path) %>%
      separate_rows(in_path, sep = " ") %>%
      mutate(site_code = if_else(site_code == in_path,
                                 paste0(site_code, "_bb"),
                                 site_code)) %>%
      select(param = site_code, node_order, brnch_num) %>%
      distinct() %>%
      arrange(brnch_num, node_order, param)

    # calculate posterior samples of escapement
    set.seed(5)
    branch_post = trans_df %>%
      filter(stringr::str_detect(param, paste0("psi_", root_site), negate = T)) %>%
      select(-chain) %>%
      group_by(param, origin) %>%
      dplyr::sample_n(size = bootstrap_samp,
                      replace = T) %>%
      mutate(iter = 1:n()) %>%
      ungroup() %>%
      left_join(param_branch,
                by = "param") %>%
      inner_join(calcBranchEscape_GRA(dabom_mod,
                                      stadem_mod,
                                      stadem_param_nm = stadem_param_nm,
                                      bootstrap_samp = bootstrap_samp) %>%
                   left_join(parent_child %>%
                               filter(parent == root_site) %>%
                               group_by(parent) %>%
                               arrange(child_rkm) %>%
                               mutate(brnch_num = 1:n()) %>%
                               ungroup() %>%
                               select(child, brnch_num) %>%
                               rename(bottom_site = child),
                             by = "brnch_num") %>%
                   mutate(across(bottom_site,
                                 replace_na,
                                 paste0(root_site, "_bb"))),
                 by = c("iter", "origin", "brnch_num")) %>%
      mutate(escp = brnch_escp * value) %>%
      arrange(origin,
              brnch_num,
              node_order,
              param,
              iter)

  } else if(!tv) {

    my_origin = if_else(stringr::str_detect(stadem_param_nm, "hatch"), 2, 1)
    set.seed(5)

    branch_post = stadem_df %>%
      add_column(origin = my_origin) %>%
      group_by(iter, origin) %>%
      summarize(across(tot_escape,
                       sum),
                .groups = "drop") %>%
      dplyr::sample_n(size = bootstrap_samp,
                      replace = T) %>%
      mutate(iter = 1:n()) %>%
      left_join(trans_df %>%
                  select(-chain) %>%
                  group_by(origin, param) %>%
                  dplyr::sample_n(size = bootstrap_samp,
                                  replace = T) %>%
                  mutate(iter = 1:n()) %>%
                  ungroup(),
                by = c("iter", "origin")) %>%
      mutate(escp = tot_escape * value) %>%
      arrange(origin,
              param,
              iter)

  }


  if(!summ_results) {
    return(branch_post)
  }

  if(summ_results) {

    # estimate the credible interval for each parameter
    cred_int = branch_post %>%
      select(iter, origin, param, escp) %>%
      tidyr::pivot_wider(names_from = "param",
                  values_from = "escp") %>%
      select(-iter, -origin) %>%
      coda::as.mcmc() %>%
      coda::HPDinterval(prob = cred_int_prob) %>%
      as.data.frame() %>%
      as_tibble(rownames = "param") %>%
      rename(lowerCI = lower,
             upperCI = upper)

    escape_summ = branch_post %>%
      group_by(param) %>%
      summarise(mean = mean(escp),
                median = median(escp),
                mode = estMode(escp),
                sd = sd(escp),
                cv = sd / mean,
                .groups = "drop") %>%
      mutate(across(c(mean, median, mode, sd),
                    ~ if_else(. < 0, 0, .))) %>%
      left_join(cred_int,
                by = 'param') %>%
      mutate(across(c(mean, median, mode),
                    round)) %>%
      mutate(across(c(sd, lowerCI, upperCI),
                    round,
                    digits = 1)) %>%
      mutate(across(cv,
                    round,
                    digits = 3))

    return(escape_summ)
  }

}
