#' @title Compile Transition Probabilities
#'
#' @description Extracts the MCMC posteriors of transition probabilities for a DABOM model, and multiplies them appropriately.
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr tidyr purrr stringr coda
#' @importFrom PITcleanr buildPaths
#' @importFrom PITcleanr getNodeInfo
#' @export
#' @return NULL
#' @examples #compileTransProbs()

compileTransProbs = function(dabom_mod = NULL,
                             parent_child = NULL,
                             configuration = NULL) {

  stopifnot(!is.null(dabom_mod),
            !is.null(parent_child),
            !is.null(configuration))

  # make sure dabom_mod is mcmc.list
  if(class(dabom_mod) == 'jagsUI') dabom_mod = dabom_mod$samples

  stopifnot(!is.null(dabom_mod),
            class(dabom_mod) %in% c('mcmc', 'mcmc.list'))

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
    dplyr::as_tibble() %>%
    # pull out movement parameters
    dplyr::select(CHAIN, ITER,
                  dplyr::starts_with("p_pop_"),
                  dplyr::starts_with("psi_"),
                  dplyr::starts_with("phi_"))

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
    # dplyr::left_join(parent_child %>%
    #                    dplyr::group_by(parent) %>%
    #                    dplyr::arrange(child_rkm) %>%
    #                    dplyr::mutate(brnch_num = 1:n()) %>%
    #                    dplyr::select(parent, child, brnch_num),
    #                  by = c("parent", "brnch_num")) %>%
    dplyr::mutate(child = dplyr::if_else(is.na(child),
                                         paste0(parent, '_bb'),
                                         child))

  trans_df <- trans_df |>
    dplyr::bind_rows(trans_df |>
                       dplyr::filter(str_detect(param, "^phi")) |>
                       dplyr::mutate(child = paste0(parent, "_bb"),
                                     value = 1 - value)) |>
    dplyr::arrange(CHAIN,
                   ITER,
                   param,
                   child)

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


  # to add a progress bar for the following map function
  pb <- progress::progress_bar$new(format = "[:bar] :current/:total (:percent) remaining: :eta",
                                   total = nrow(site_paths),
                                   show_after = 2)

  for(i in 1:nrow(site_paths)) {
    pb$tick()
    if(i == 1) {
      trans_comp <- trans_df |>
        dplyr::group_by(CHAIN,
                        ITER,
                        origin) |>
        dplyr::filter(child %in% site_paths$path_vec[i][[1]]) |>
        dplyr::summarize(
          dplyr::across(value,
                        ~ prod(.)),
          .groups = "drop") |>
        dplyr::mutate(param = site_paths$end_loc[i])
    } else {
      trans_comp <- trans_comp |>
        dplyr::bind_rows(trans_df |>
                           dplyr::group_by(CHAIN,
                                           ITER,
                                           origin) |>
                           dplyr::filter(child %in% site_paths$path_vec[i][[1]]) |>
                           dplyr::summarize(
                             dplyr::across(value,
                                           ~ prod(.)),
                             .groups = "drop") |>
                           dplyr::mutate(param = site_paths$end_loc[i]))
    }
  }

  trans_comp <- trans_comp |>
    dplyr::group_by(CHAIN, origin, param) |>
    dplyr::mutate(iter = 1:n()) |>
    dplyr::ungroup() |>
    dplyr::select(chain = CHAIN,
                  iter,
                  origin,
                  param,
                  value) |>
    dplyr::arrange(chain,
                   iter,
                   param,
                   origin)

  return(trans_comp)
}
