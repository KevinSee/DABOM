#' @title Summarise Detection Probabilities
#'
#' @description Extracts the MCMC posteriors of detection probabilities for a DABOM model, and summarizes the results.
#'
#' @author Kevin See
#'
#' @inheritParams extractDetectPost
#' @inheritParams summarisePost
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr coda stringr
#' @export
#' @return a dataframe with number of tags detected, mean, median, mode, se, cv and credible intervals of detection probability posteriors.
#' @examples \dontrun{summariseDetectProbs()}

summariseDetectProbs = function(dabom_mod = NULL,
                                filter_ch = NULL,
                                .cred_int_prob = 0.95) {

  # extract detection posterior draws
  detect_df <-
    extractDetectPost(dabom_mod)

  # summarize detection posterior draws
  summ_detect <-
    summarisePost(detect_df,
                  value,
                  node,
                  .cred_int_prob = .cred_int_prob)

  stopifnot(!is.null(filter_ch))

  if (sum(filter_ch$user_keep_obs == "") > 0 | sum(is.na(filter_ch$user_keep_obs)) > 0 ) {
    stop("user_keep_obs must be defined for each observation.")
  }

  filter_ch = filter_ch %>%
    filter(user_keep_obs)

  # add number of unique tags sighted at each node
  detect_summ <-
    filter_ch %>%
    dplyr::group_by(node) %>%
    dplyr::summarise(n_tags = n_distinct(tag_code)) %>%
    dplyr::full_join(summ_detect,
                     dplyr::join_by(node)) %>%
    dplyr::mutate(
      dplyr::across(n_tags,
                    ~ tidyr::replace_na(., 0))) |>
    dplyr::mutate(
      dplyr::across(mode,
                    ~ dplyr::if_else(mean == 1 & median == 1, 1, .)))

  return(detect_summ)

}
