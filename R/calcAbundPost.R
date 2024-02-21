#' @title Calculate Abundance Posteriors
#'
#' @description Combines estimates of total escapement from STADEM with transition probabilities from DABOM to generate estimates of escapement above various detection sites.
#'
#' @author Kevin See
#'
#' @param move_post
#' @param abund_post
#' @param bootstrap_samp The number of samples to be drawn from the posteriors of the STADEM model and the DABOM model.
#' @inheritParams compileTransProbs
#'
#' @import dplyr tidyr stringr
#' @export
#' @return NULL
#' @examples #calcAbundPost()

calcAbundPost = function(move_post = NULL,
                         abund_post = NULL,
                         bootstrap_samp = 2000,
                         .move_vars = c("origin",
                                        "param"),
                         .abund_vars = c("origin"),
                         time_vary_param_nm = NULL) {

  stopifnot(!is.null(move_post),
            !is.null(abund_post))

  if(length(intersect(.move_vars, .abund_vars)) > 0) {
    common_vars <-
      intersect(.move_vars, .abund_vars)

    # stopifnot(identical(move_post |>
    #                       select(all_of(common_vars)) |>
    #                       distinct(),
    #
    #                     abund_post |>
    #                       select(all_of(common_vars)) |>
    #                       distinct()))

    if(sum(!common_vars %in% names(move_post)) > 0) {
      warning(paste(paste(common_vars[!common_vars %in% names(move_post)], collapse = ", "),
                    "are not in `move_post`.\n"))
      stop()
    }

    if(sum(!common_vars %in% names(abund_post)) > 0) {
      warning(paste(paste(common_vars[!common_vars %in% names(abund_post)], collapse = ", "),
                    "are not in `abund_post`.\n"))
      stop()
    }
  } else {
    common_vars <- NULL
  }


  n_chain_move = max(move_post$chain)
  n_chain_abund = max(abund_post$chain)
  if(n_chain_move != n_chain_abund) {
    stop()
  }




  set.seed(17)
  total_post <-
    move_post |>
    dplyr::select(chain, iter) |>
    dplyr::distinct() |>
    dplyr::group_by(chain) |>
    dplyr::slice_sample(n = bootstrap_samp,
                        replace = T) |>
    dplyr::mutate(new_iter = 1:n()) |>
    dplyr::ungroup() |>
    dplyr::left_join(move_post,
                     by = join_by(chain, iter),
                     relationship = "many-to-many") |>
    dplyr::select(-iter) |>
    dplyr::rename(iter = new_iter) |>
    dplyr::left_join(
      abund_post |>
        dplyr::select(chain, iter) |>
        dplyr::distinct() |>
        dplyr::group_by(chain) |>
        dplyr::slice_sample(n = bootstrap_samp,
                            replace = T) |>
        dplyr::mutate(new_iter = 1:n()) |>
        dplyr::ungroup() |>
        dplyr::left_join(abund_post,
                         by = join_by(chain, iter),
                         relationship = "many-to-many") |>
        dplyr::select(-iter) |>
        dplyr::rename(iter = new_iter)) |>
    dplyr::filter(!is.na(value),
                  !is.na(tot_abund)) |>
    dplyr::mutate(abund = value * tot_abund) |>
    dplyr::relocate(iter,
                    .after = chain)


#   set.seed(17)
#   total_post <-
#     move_post |>
#     dplyr::select(chain,
#                   {{ .move_vars }},
#                   {{ time_vary_param_nm }},
#                   value) |>
#     dplyr::group_by(chain,
#                     dplyr::pick({{ .move_vars }}),
#                     dplyr::pick({{ time_vary_param_nm }})) %>%
#     dplyr::slice_sample(n = bootstrap_samp,
#                         replace = T) %>%
#     dplyr::mutate(iter = 1:n()) %>%
#     dplyr::ungroup() |>
#     dplyr::left_join(abund_post |>
#                        dplyr::select(chain,
#                                      {{ .abund_vars }},
#                                      {{ time_vary_param_nm }},
#                                      tot_abund) |>
#                        dplyr::group_by(chain,
#                                        dplyr::pick({{ .abund_vars }}),
#                                        dplyr::pick({{ time_vary_param_nm }})) %>%
#                        dplyr::slice_sample(n = bootstrap_samp,
#                                            replace = T) %>%
#                        dplyr::mutate(iter = 1:n()) %>%
#                        dplyr::ungroup()) |>
#     dplyr::filter(!is.na(value),
#                   !is.na(tot_abund)) |>
#     dplyr::mutate(abund = value * tot_abund) |>
#     dplyr::relocate(iter,
#                     .after = chain)

  if(!is.null(time_vary_param_nm)) {
    total_post <-
      total_post |>
      dplyr::group_by(chain,
                      iter,
                      dplyr::pick({{ .move_vars }})) |>
      dplyr::summarize(
        dplyr::across(abund,
                      sum),
        .groups ="drop")
  }

  return(total_post)
}
