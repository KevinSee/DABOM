#' @title Extract Detection Posteriors
#'
#' @description Extracts the MCMC posteriors of detection probabilities for a DABOM model
#'
#' @author Kevin See
#'
#' @param dabom_mod An MCMC.list containing some parameters that end with "_p"
#'
#' @import dplyr tidyr stringr
#' @importFrom PITcleanr getNodeInfo
#' @export
#' @return NULL
#' @examples #extractDetectPost()

extractDetectPost = function(dabom_mod = NULL) {

  stopifnot(!is.null(dabom_mod))

  # make sure dabom_mod is mcmc.list
  if(inherits(dabom_mod, "jagsUI")) {
    dabom_mod = dabom_mod$samples
  }

  stopifnot(!is.null(dabom_mod),
            inherits(dabom_mod, c('mcmc', 'mcmc.list')))

  detect_mat <-
    dabom_mod |>
    as.matrix(iters = T,
              chains = T) |>
    dplyr::as_tibble() |>
    # pull out movement parameters
    dplyr::select(chain = CHAIN,
                  iter = ITER,
                  ends_with("_p"))

  detect_df <-
    detect_mat |>
    tidyr::pivot_longer(cols = -c(chain, iter),
                        names_to = "param",
                        values_to = "value") |>
    dplyr::mutate(node = stringr::str_remove(param, "_p$")) |>
    dplyr::relocate(node,
                    .after = param)
  return(detect_df)
}
