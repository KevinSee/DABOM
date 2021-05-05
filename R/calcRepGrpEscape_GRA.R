#' @title Estimate Escapement for Reporting Groups - GRA
#'
#' @description Combines estimates of total wild escapement at Lower Granite Dam (from STADEM) with transition probabilities from DABOM to generate estimates of escapement to pre-defined reporting groups.
#'
#' @author Kevin See
#'
#' @inheritParams calcTribEscape_GRA
#' @param spp Species, either "Chinook" or "Steelhead"
#'
#' @import dplyr tidyr
#' @return NULL
#' @examples #calcRepGrpEscape_GRA()

calcRepGrpEscape_GRA = function(dabom_mod = NULL,
                                stadem_mod = NULL,
                                stadem_param_nm = 'X.new.wild',
                                bootstrap_samp = 2000,
                                parent_child = NULL,
                                cred_int_prob = 0.95,
                                spp = c('Chinook', 'Steelhead')) {

  stopifnot(!is.null(dabom_mod) ,
            !is.null(stadem_mod),
            !is.null(node_order))

  spp = match.arg(spp)

  # combine weekly escapement with other transition probabilities
  escp_df = calcTribEscape_GRA(dabom_mod,
                               stadem_mod,
                               parent_child = parent_child,
                               summ_results = T,
                               cred_int_prob = cred_int_prob)

  # get estimates for reporting groups
  rep_grp_sites = defineRepGrps(spp) %>%
    left_join(escp_df,
              by = c("low_site" = "param"))

  rep_grp = rep_grp_sites %>%
    group_by(report_grp) %>%
    summarize(across(c(mean, median, mode, ends_with("CI")),
                     sum,
                     na.rm = T),
              .groups = "drop") %>%
    left_join(rep_grp_sites %>%
                group_by(report_grp) %>%
                summarize(across(sd,
                                 ~ sqrt(sum(.^2)),
                                 na.rm = T),
                          .groups = "drop"),
              by = "report_grp") %>%
    mutate(cv = sd / mean) %>%
    select(report_grp:mode, sd, cv, everything())

  return(rep_grp)
}

