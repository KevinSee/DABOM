#' @title Extract Transition Posteriors
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list containing some parameters that begin with "p_pop", "psi" or "phi"
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr stringr
#' @importFrom PITcleanr getNodeInfo
#' @export
#' @return NULL
#' @examples extractTransPost()

extractTransPost = function(dabom_mod = NULL,
                            parent_child = NULL,
                            configuration = NULL) {

  stopifnot(!is.null(dabom_mod),
            !is.null(parent_child),
            !is.null(configuration))

  # make sure dabom_mod is mcmc.list
  if(inherits(dabom_mod, "jagsUI")) {
    dabom_mod = dabom_mod$samples
  }

  stopifnot(!is.null(dabom_mod),
            inherits(dabom_mod, c('mcmc', 'mcmc.list')))

  node_info <-
    PITcleanr::getNodeInfo(parent_child,
                           configuration) |>
    dplyr::select(parent = parent_site,
                  child = site_code,
                  brnch_num = child_num) |>
    dplyr::distinct() |>
    dplyr::arrange(parent,
                   brnch_num)


  trans_mat <-
    dabom_mod |>
    as.matrix(iters = T,
              chains = T) |>
    dplyr::as_tibble() |>
    # pull out movement parameters
    dplyr::select(chain = CHAIN,
                  iter = ITER,
                  starts_with("p_pop_"),
                  starts_with("psi_"),
                  starts_with("phi_"))

  trans_df <-
    trans_mat |>
    tidyr::pivot_longer(cols = -c(chain, iter),
                        names_to = "param",
                        values_to = "value") |>
    dplyr::mutate(origin = stringr::str_split(param, '\\[', simplify = T)[,2],
                  origin = stringr::str_sub(origin, 1, 1)) |>
    dplyr::mutate(parent = stringr::str_split(param, '\\[', simplify = T)[,1],
                  parent = stringr::str_remove(parent, '^p_pop_'),
                  parent = stringr::str_remove(parent, '^psi_'),
                  parent = stringr::str_remove(parent, '^phi_'),
                  brnch_num = stringr::str_split(param, '\\,', simplify = T)[,2],
                  brnch_num = stringr::str_remove(brnch_num, '\\]')) |>
    # added code to introduce time-varying strata if available
    dplyr::mutate(strata_num = stringr::str_split(param, '\\,', simplify = T)[,3],
                  strata_num = stringr::str_remove(strata_num, '\\]'),
                  dplyr::across(strata_num,
                                as.integer)) |>
    dplyr:: mutate(
      dplyr::across(
        c(brnch_num,
          origin),
        as.numeric)) |>
    dplyr::mutate(
      dplyr::across(
        brnch_num,
        ~ tidyr::replace_na(., 1))) |>
    dplyr::left_join(node_info,
                     by = dplyr::join_by(parent, brnch_num)) |>
    dplyr::mutate(child = dplyr::if_else(is.na(child),
                                         paste0(parent, '_bb'),
                                         child))

  # add black boxes upstream of phi locations
  trans_df <-
    trans_df |>
    dplyr::bind_rows(trans_df |>
                       dplyr::filter(stringr::str_detect(param, "^phi")) |>
                       dplyr::mutate(child = paste0(parent, "_bb"),
                                     value = 1 - value)) |>
    dplyr::select(chain,
                  iter,
                  param,
                  value,
                  origin,
                  child,
                  everything()) |>
    dplyr::group_by(chain,
                    param) |>
    dplyr::mutate(iter = 1:n()) |>
    dplyr::ungroup() |>
    dplyr::arrange(chain,
                   iter,
                   param,
                   child)

  return(trans_df)
}
