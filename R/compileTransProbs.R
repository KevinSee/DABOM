#' @title Compile Transition Probabilities
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and multiplies them appropriately.
#'
#' @author Kevin See
#'
#' @param trans_post posterior draws of transition probabilities compiled by the `extractTransPost` function
#' @inheritParams extractTransPost
#' @param time_vary_only Should only time-varying parameters be compiled? Default is `FALSE`, meaning time-varying parameters are excluded from results. If set to `TRUE`, non-time-varying parameters are excluded from results.
#' @param time_vary_param_nm column name describing the time-varying strata for each location. Default value is `strata_num`.
#'
#' @import dplyr tidyr purrr stringr coda
#' @importFrom PITcleanr buildPaths
#' @export
#' @return NULL
#' @examples #compileTransProbs()

compileTransProbs = function(trans_post = NULL,
                             parent_child = NULL,
                             time_vary_only = FALSE,
                             time_vary_param_nm = "strata_num") {


  if(!time_vary_param_nm %in% names(trans_post) & time_vary_only) {
    warning(paste0("To compile time-varying parameters, a column named `",
                   time_vary_param_nm,
                   "` must be included in the trans_post object."))
    stop()
  }

  # add black boxes above phi parameters
  pc = parent_child |>
    dplyr::bind_rows(parent_child |>
                       dplyr::select(matches("parent")) |>
                       dplyr::distinct() |>
                       dplyr::left_join(parent_child |>
                                          dplyr::select(matches("parent")) |>
                                          dplyr::mutate(child = paste0(parent, "_bb")) |>
                                          rlang::set_names(function(x) {
                                            stringr::str_replace(x, "parent_", "child_")
                                          }),
                                        by = dplyr::join_by(parent)) |>
                       dplyr::distinct() |>
                       dplyr::select(dplyr::any_of(names(parent_child))))


  # get root site from parent-child table
  root_site <- parent_child |>
    dplyr::filter(!parent %in% child) |>
    dplyr::select(parent) |>
    dplyr::distinct() |>
    dplyr::pull(parent)

  # determine paths between sites
  site_paths <- PITcleanr::buildPaths(parent_child = pc,
                                      direction = "u") |>
    dplyr::filter(end_loc != root_site) |>
    dplyr::mutate(path_no_root = stringr::str_remove(path, root_site),
                  dplyr::across(path_no_root,
                                stringr::str_trim)) |>
    dplyr::mutate(path_vec = stringr::str_split(path_no_root, " "))


  # # if time_vary_only == FALSE, set initial time-varying parameters to 1
  # if(!time_vary_only & {time_vary_param_nm} %in% names(trans_post)) {
  #     trans_post <-
  #       trans_post |>
  #       dplyr::mutate(
  #         dplyr::across(value,
  #                       ~ dplyr::if_else(!is.na(.data[[time_vary_param_nm]]),
  #                                        1, .)))
  # }

  # if time_vary_only == FALSE, drop time-varying parameters from trans_post
  if(!time_vary_only & {time_vary_param_nm} %in% names(trans_post)) {
    trans_post <-
      trans_post |>
      dplyr::filter(is.na(.data[[time_vary_param_nm]]))
  }

  # if time_vary_only == TRUE, only pull out parameters with time-varying strata
  if(time_vary_only) {

    trans_post <-
      trans_post |>
      dplyr::filter(!is.na(.data[[time_vary_param_nm]]))

    # restrict the sites in site_paths to those whose parent had a time-varying parameter
    site_paths <-
      site_paths |>
      dplyr::filter(end_loc %in% unique(trans_post$child))
  }

  if(!{time_vary_param_nm} %in% names(trans_post)) {
    trans_post <-
      trans_post |>
      add_column("{time_vary_param_nm}" := NA_real_)
  }

  trans_comp <-
    site_paths |>
    mutate(trans = map(path_vec,
                       .progress = TRUE,
                       .f = function(x) {
                         trans_post |>
                           dplyr::filter(child %in% x) |>
                           dplyr::group_by(chain,
                                           iter,
                                           origin,
                                           dplyr::pick({{time_vary_param_nm}})) |>
                           dplyr::summarize(
                             dplyr::across(value,
                                           ~ prod(.)),
                             .groups = "drop")
                       })) |>
    tidyr::unnest(trans) |>
    dplyr::select(chain,
                  iter,
                  origin,
                  param = end_loc,
                  {time_vary_param_nm},
                  value) |>
    dplyr::arrange(chain,
                   iter,
                   param,
                   "{time_vary_param_nm}",
                   origin)




  if(sum(!is.na(trans_comp[,{time_vary_param_nm}])) == 0) {
    trans_comp <-
      trans_comp |>
      select(-{time_vary_param_nm})
  }

  return(trans_comp)
}
