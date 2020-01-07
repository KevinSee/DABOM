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
#' @param node_order output of function \code{createNodeOrder}.
#' @param summ_results Should the resulting posteriors be summarised or returned as posterior samples? Default value is \code{TRUE}.
#' @param pt_est_nm Determines whether to use the mean, median of mode of posterior samples as the point estimate. Returns all 3 if \code{NULL}, which is default.
#' @param cred_int_prob A numeric scalar in the interval (0,1) giving what higest posterior density portion of the posterior the credible interval should cover. The default value is 95\%.
#' @inheritParams compileTransProbs_LGD
#'
#' @import dplyr tidyr stringr
#' @export
#' @return NULL
#' @examples #calcTribEscape_LGD()

calcTribEscape_LGD = function(dabom_mod = NULL,
                              stadem_mod = NULL,
                              stadem_param_nm = 'X.new.wild',
                              bootstrap_samp = 2000,
                              node_order = NULL,
                              summ_results = T,
                              pt_est_nm = NULL,
                              time_varying = TRUE,
                              cred_int_prob = 0.95) {

  stopifnot(!is.null(dabom_mod) ,
            !is.null(stadem_mod),
            !is.null(node_order))

  stopifnot(pt_est_nm %in% c('mean', 'median', 'mode') | is.null(pt_est_nm))

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
    tidyr::gather(param, value, -CHAIN, -ITER) %>%
    mutate(week = stringr::str_extract(param, '[:digit:]+'),
           week = as.integer(week)) %>%
    group_by(param) %>%
    mutate(iter = 1:n()) %>%
    ungroup() %>%
    select(iter, week, tot_escape = value)

  stadem_summ = stadem_df %>%
    group_by(week) %>%
    summarise(mean = mean(tot_escape),
              median = median(tot_escape),
              mode = estMode(tot_escape),
              sd = sd(tot_escape)) %>%
    mutate_at(vars(mean, median, mode, sd),
              list(~if_else(. < 0, 0, .))) %>%
    mutate(var = sd^2)


  if(time_varying) {
    init_trans = compileWeekTransProbs(dabom_mod,
                                       'p_pop_main')

    tmp = init_trans %>%
      mutate(param = paste(param, branch, sep = '_')) %>%
      mutate(param = forcats::fct_reorder(param, branch)) %>%
      select(-branch) %>%
      tidyr::spread(param, prob)
    names(tmp) = renameTransParams_LGD(names(tmp))
    init_trans = tmp %>%
      tidyr::gather(branch, prob, -(CHAIN:week))
    rm(tmp)

    # get total escapement posteriors to each initial branch
    branch_escape_list = stadem_df %>%
      group_by(week) %>%
      sample_n(size = bootstrap_samp,
               replace = T) %>%
      mutate(iter = 1:n()) %>%
      ungroup() %>%
      left_join(init_trans %>%
                  select(week, branch, prob) %>%
                  group_by(week, branch) %>%
                  sample_n(size = bootstrap_samp,
                           replace = T) %>%
                  mutate(iter = 1:n()) %>%
                  ungroup(),
                by = c('iter', 'week')) %>%
      mutate(branch_escape = tot_escape * prob) %>%
      group_by(iter, branch) %>%
      summarise_at(vars(branch_escape),
                   list(sum)) %>%
      ungroup() %>%
      split(list(.$branch))
  }

  # get rest of transition probabilities
  trans_df = compileTransProbs_LGD(dabom_mod,
                                   time_varying = time_varying) %>%
    select(-chain) %>%
    group_by(param) %>%
    mutate(iter = 1:n()) %>%
    ungroup() %>%
    arrange(param, iter) %>%
    tidyr::spread(param, value) %>%
    sample_n(size = bootstrap_samp,
             replace = T) %>%
    mutate(iter = 1:n()) %>%
    ungroup()


  if(time_varying) {
    site_list = createNodeList(node_order) %>%
      purrr::map(.f = function(x) {
        x = gsub('B0$', '', x)
        x = gsub('A0$', '', x)
        x = gsub('^X', '', x)
        return(unique(x))
      })
    # add name of tributary to site_list
    for(i in 1:length(site_list)) {
      site_list[[i]] = c(names(site_list)[i],
                         site_list[[i]])
    }

    site_list$Main_bb = 'Main_bb'

    # this functionality relies on specific format of naming in the compileTransProbs_LGD() function
    trib_list = site_list %>%
      purrr::map(.f = function(x) {
        y = trans_df %>%
          select(iter,
                 one_of(x),
                 one_of(paste0('past_', x)),
                 one_of(paste0(x, '_bb')))
      })


    # add total escapement to transition probabilities within each main branch
    for(brch in names(trib_list)) {
      trib_list[[brch]] = trib_list[[brch]] %>%
        left_join(branch_escape_list[[brch]] %>%
                    select(-branch),
                  by = 'iter')
    }


    # combine all tributaries into single dataframe and transform transition probabilities into escapement
    escape_post = trib_list %>%
      purrr::map_df(.id = 'branch',
                    .f = function(x) {
                      x %>%
                        mutate_at(vars(-iter, -branch_escape),
                                  list(~ . * branch_escape)) %>%
                        select(-branch_escape) %>%
                        tidyr::gather(area, escape, -iter)
                    })
  }

  if(!time_varying) {
    escape_post = stadem_df %>%
      select(-week) %>%
      sample_n(size = bootstrap_samp,
               replace = T) %>%
      mutate(iter = 1:n()) %>%
      left_join(trans_df) %>%
      mutate_at(vars(-iter, -tot_escape),
                list(~ . * tot_escape)) %>%
      select(-tot_escape) %>%
      tidyr::gather(area, escape, -iter)
  }


  if(!summ_results) {
    return(escape_post)
  }

  if(summ_results) {

    if("branch" %in% names(escape_post)) {
      escape_post = escape_post %>%
        select(-branch)
    }

    # estimate the credible interval for each parameter
    credInt = escape_post %>%
      # select(-branch) %>%
      spread(area, escape) %>%
      select(-iter) %>%
      coda::as.mcmc() %>%
      coda::HPDinterval(prob = cred_int_prob) %>%
      as.data.frame() %>%
      mutate(area = rownames(.)) %>%
      rename(lowerCI = lower,
             upperCI = upper) %>%
      tbl_df() %>%
      select(area, everything())

    escape_summ = escape_post %>%
      group_by(area) %>%
      summarise(mean = mean(escape),
                median = median(escape),
                mode = estMode(escape),
                sd = sd(escape),
                cv = sd / mean) %>%
      mutate_at(vars(mean, median, mode, sd),
                list(~ ifelse(. < 0, 0, .))) %>%
      left_join(credInt,
                by = 'area') %>%
      mutate_at(vars(mean, median, mode),
                list(round)) %>%
      mutate_at(vars(sd, lowerCI, upperCI),
                list(round),
                digits = 1) %>%
      mutate_at(vars(cv),
                list(round),
                digits = 3)

    if(!is.null(pt_est_nm)) {
      names(escape_summ)[match(pt_est_nm, names(escape_summ))] = 'estimate'
      escape_summ = escape_summ %>%
        select(area, estimate, sd:upperCI)
    }

    return(escape_summ)
  }


}
