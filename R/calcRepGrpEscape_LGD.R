#' @title Estimate Escapement for Reporting Groups - LGD
#'
#' @description Combines estimates of total escapement from STADEM with transition probabilities from DABOM to generate estimates of escapement within pre-defined reporting groups. Currently only works for wild fish.
#'
#' @author Kevin See
#'
#' @inheritParams calcTribEscape_LGD
#' @inheritParams defineRepGrps
#'
#' @import dplyr tidyr
#' @importFrom coda as.mcmc
#' @importFrom coda HPDinterval
#' @export
#' @return NULL
#' @examples #calcRepGrpEscape_LGD()

calcRepGrpEscape_LGD = function(dabom_mod = NULL,
                                stadem_mod = NULL,
                                stadem_param_nm = 'X.new.wild',
                                bootstrap_samp = 2000,
                                node_order = NULL,
                                summ_results = T,
                                pt_est_nm = NULL,
                                cred_int_prob = 0.95,
                                spp = c('Chinook', 'Steelhead')) {

  stopifnot(!is.null(dabom_mod) ,
            !is.null(stadem_mod),
            !is.null(node_order))

  spp = match.arg(spp)

  report_df = defineRepGrps(spp,
                            node_order) %>%
    mutate(area = ifelse(NodeOrder == 2,
                         as.character(Group),
                         ifelse(nchar(NodeSite) > 3,
                                lowNode,
                                paste0('past_', NodeSite)))) %>%
    select(ReportGrp, lowNode, area)

  escape_post = calcTribEscape_LGD(dabom_mod,
                                   stadem_mod,
                                   node_order = node_order,
                                   summ_results = F)

  # estimate the credible interval for each reporting group
  credInt = report_df %>%
    left_join(escape_post) %>%
    group_by(ReportGrp, iter) %>%
    summarise_at(vars(escape),
                        funs(sum),
                        na.rm = T) %>%
    ungroup() %>%
    group_by(ReportGrp) %>%
    filter(n_distinct(iter) > 1) %>%
    ungroup() %>%
    tidyr::spread(ReportGrp, escape) %>%
    select(-iter) %>%
    coda::as.mcmc() %>%
    coda::HPDinterval(prob = cred_int_prob) %>%
    as.data.frame() %>%
    mutate(ReportGrp = rownames(.)) %>%
    rename(lowerCI = lower,
                  upperCI = upper) %>%
    tbl_df() %>%
    select(ReportGrp, everything())



  report_summ = report_df %>%
    left_join(escape_post) %>%
    group_by(ReportGrp, iter) %>%
    summarise_at(vars(escape),
                        funs(sum),
                        na.rm = T) %>%
    ungroup() %>%
    group_by(ReportGrp) %>%
    filter(n_distinct(iter) > 1) %>%
    summarise(mean = mean(escape),
                     median = median(escape),
                     mode = estMode(escape),
                     sd = sd(escape)) %>%
    mutate_at(vars(mean, median, mode, sd),
                     funs(ifelse(. < 0, 0, .))) %>%
    full_join(credInt) %>%
    full_join(report_df %>%
                       select(ReportGrp) %>%
                       distinct()) %>%
    ungroup() %>%
    mutate_at(vars(mean, median, mode),
              funs(round)) %>%
    mutate_at(vars(sd, lowerCI, upperCI),
              funs(round),
              digits = 1) %>%
    mutate_at(vars(cv),
              funs(round),
              digits = 3)

  if(!is.null(pt_est_nm) & pt_est_nm %in% c('mean', 'median', 'mode')) {
    names(report_summ)[match(pt_est_nm, names(report_summ))] = 'estimate'
    report_summ = report_summ %>%
      select(ReportGrp, estimate, sd:upperCI)
  }

  return(report_summ)

}

