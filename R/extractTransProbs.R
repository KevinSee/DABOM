#' @title Extract Transition Probabilities
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr purrr
#' @importFrom PITcleanr buildNodeOrder
#' @export
#' @return NULL
#' @examples extractTransProbs()

extractTransProbs = function(dabom_mod = NULL,
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

  node_info = PITcleanr::getNodeInfo(parent_child,
                                     configuration) |>
    select(parent = parent_site,
           child = site_code,
           brnch_num = child_num) |>
    distinct() |>
    arrange(parent,
            brnch_num)


  trans_mat = as.matrix(dabom_mod,
                        iters = T,
                        chains = T) %>%
    as_tibble() %>%
    # pull out movement parameters
    select(CHAIN, ITER,
           starts_with("p_pop_"),
           starts_with("psi_"),
           starts_with("phi_"))

  trans_df = trans_mat %>%
    tidyr::pivot_longer(cols = -c(CHAIN, ITER),
                        names_to = "param",
                        values_to = "value") %>%
    dplyr::mutate(origin = stringr::str_split(param, '\\[', simplify = T)[,2],
                  origin = stringr::str_sub(origin, 1, 1)) %>%
    dplyr::mutate(parent = stringr::str_split(param, '\\[', simplify = T)[,1],
                  parent = stringr::str_remove(parent, '^p_pop_'),
                  parent = stringr::str_remove(parent, '^psi_'),
                  parent = stringr::str_remove(parent, '^phi_'),
                  brnch_num = stringr::str_split(param, '\\,', simplify = T)[,2],
                  brnch_num = stringr::str_remove(brnch_num, '\\]')) %>%
    # added code to introduce time-varying strata if available
    dplyr::mutate(strata_num = stringr::str_split(param, '\\,', simplify = T)[,3],
                  strata_num = stringr::str_remove(strata_num, '\\]'),
                  dplyr::across(strata_num,
                                as.integer)) %>%
    dplyr:: mutate(
      dplyr::across(
        c(brnch_num,
          origin),
        as.numeric)) %>%
    dplyr::mutate(
      dplyr::across(
        brnch_num,
        ~ replace_na(., 1))) %>%
    dplyr::left_join(node_info,
                     by = join_by(parent, brnch_num)) %>%
    dplyr::mutate(child = dplyr::if_else(is.na(child),
                                         paste0(parent, '_bb'),
                                         child))

  # add black boxes upstream of phi locations
  trans_df <-
    trans_df |>
    dplyr::bind_rows(trans_df |>
                       dplyr::filter(str_detect(param, "^phi")) |>
                       dplyr::mutate(child = paste0(parent, "_bb"),
                                     value = 1 - value)) |>
    dplyr::arrange(CHAIN,
                   ITER,
                   param,
                   child)

  # root_site = PITcleanr::buildNodeOrder(parent_child) %>%
  #   filter(node_order == 1) %>%
  #   pull(node)

  # # get root site from parent-child table
  # root_site <- parent_child |>
  #   dplyr::filter(!parent %in% child) |>
  #   dplyr::select(parent) |>
  #   dplyr::distinct() |>
  #   dplyr::pull(parent)
  #
  # # if time-varying, fix all initial transition probabilities > 0 to 1 for multiplication purposes
  # tv = trans_df %>%
  #   filter(parent == root_site) %>%
  #   mutate(n_comma = stringr::str_count(param, "\\,"),
  #          n_comma = as.integer(n_comma)) %>%
  #   select(n_comma) %>%
  #   distinct() %>%
  #   mutate(tv = if_else(n_comma > 1, T, F)) %>%
  #   pull(tv)
  # if(length(tv) == 0) {
  #   tv = FALSE
  # }
  # if(tv) {
  #   trans_df <-
  #     trans_df %>%
  #     filter(parent == root_site) %>%
  #     mutate(across(value,
  #                   ~ if_else(. > 0, 1, .))) %>%
  #     select(CHAIN, ITER, origin, child, value) %>%
  #     distinct() %>%
  #     bind_rows(trans_df %>%
  #                 filter(parent != root_site) %>%
  #                 select(CHAIN, ITER, origin, child, value))
  # } else {
  #   trans_df = trans_df %>%
  #     select(CHAIN, ITER, origin, child, value)
  # }

  return(trans_df)
}
