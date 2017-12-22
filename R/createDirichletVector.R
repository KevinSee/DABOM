#' @title Create Dirichlet vector
#'
#' @description Construct appropriate Dirichlet vector to pass to JAGS, based on which branches had observed tags, and which did not.
#'
#' @author Kevin See
#'
#' @param n_branches How many paths could a tag travel from this branching node?
#' @param init_vals The initial values, based on observations
#' @param initial_one Should the first value of the Dirichlet vector be fixed to 1? Default value is \code{FALSE}.
#' @param final_one Should the last value of the Dirichlet vector be fixed to 1? Default value is \code{FALSE}.
#'
#' @export
#' @return NULL
#' @examples createDirichletVector(3)

createDirichletVector = function(n_branches = NULL,
                                 init_vals = NULL,
                                 initial_one = T,
                                 final_one = F) {

  stopifnot(!is.null(n_branches) |
              !is.null(init_vals))

  dirch_vec = rep(1, n_branches)
  dirch_vec[!1:n_branches %in% names(init_vals)] = 0

  # always keep the first box as a 1
  if(initial_one) dirch_vec[1] = 1
  # always keep the last box as a 1
  if(final_one) dirch_vec[n_branches] = 1

  return(dirch_vec)
}
