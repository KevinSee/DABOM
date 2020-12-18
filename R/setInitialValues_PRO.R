#' @title PRO DABOM Initial Values
#'
#' @description Construct appropriate initial values for Prosser version of DABOM
#'
#' @author Kevin See
#'
#' @param dabom_list output of \code{createDABOMcapHist} with parameter split_matrics set to \code{TRUE}.
#' @param model_file file path to JAGS text file.
#' @param parent_child parent-child table, output of \code{createParentChildDf} function.
#'
#' @import dplyr
#' @export
#' @return NULL
#' @examples setInitialValues_PR)()

setInitialValues_PRO = function(dabom_list = NULL,
                                model_file = NULL,
                                parent_child = NULL) {

  # how many tags?
  n_fish = nrow(dabom_list[[1]])

  # what node does model start with?
  root_node = parent_child %>%
    filter(nodeOrder == 1) %>%
    pull(ParentNode)

  n_branch_list = setBranchNums(parent_child) %>%
    rlang::set_names(nm = str_remove(names(.), 'n_pops_'))

  n_branch_list[!grepl(root_node, names(n_branch_list))] = n_branch_list[!grepl(root_node, names(n_branch_list))] %>%
    map(.f = function(x) x + 1)

  # read in JAGS model file
  mod_conn = file(model_file, open = 'rt')
  mod_file = readLines(mod_conn)
  close(mod_conn)
  rm(mod_conn)

  init_a = stringr::str_trim(mod_file[grep('a_', mod_file)])
  init_a = init_a[grepl('^a_', init_a)]
  init_a = init_a %>%
    stringr::str_split('\\[') %>%
    map_chr(.f = function(x) x[1])

  if(sum(!str_remove(init_a, '^a_') %in% names(n_branch_list)) > 0) {
    stop()
  }

  init_z = stringr::str_trim(mod_file[grep('z_', mod_file)])
  init_z = init_z[grepl('^z_', init_z)]
  init_z = init_z %>%
    stringr::str_split('\\[') %>%
    map_chr(.f = function(x) x[1])

  z_list = init_z %>%
    as.list() %>%
    rlang::set_names() %>%
    rlang::set_names(nm = str_remove(names(.), '^z_')) %>%
    map(.f = function(x) {
      rep(0, n_fish)
    })

  # first lets create inits matrices
  a_list = n_branch_list %>%
    purrr::map(.f = function(x) {
      array(0,
            dim = c(n_fish, x))
    })

  # parent_child %>%
  #   filter(ParentNode == root_node,
  #          ChildNode != root_node)

  # initial branching detects
  a_list[["PRO"]][,1] = dabom_list$Downstream %>%
    select(matches('BelowJD1')) %>%
    apply(1, max)

  a_list[["PRO"]][,2] = dabom_list$Downstream %>%
    select(matches('^JD1')) %>%
    apply(1, max)

  a_list[["PRO"]][,3] = dabom_list$Downstream %>%
    select(matches('MCN')) %>%
    apply(1, max)

  a_list[["PRO"]][,4] = dabom_list$Downstream %>%
    select(matches('ICH')) %>%
    apply(1, max)

  a_list[["PRO"]][,5] = dabom_list$Status %>%
    apply(1, max)

  a_list[["PRO"]][,6] = dabom_list$Toppenish %>%
    apply(1, max)

  a_list[["PRO"]][,7] = dabom_list$Sunnyside %>%
    apply(1, max)

  a_list[["PRO"]][,8] = dabom_list$Downstream %>%
    select(matches('PRA')) %>%
    apply(1, max)

  # initial black box
  a_list[["PRO"]][,9] = abs(apply(a_list[['PRO']], 1, max, na.rm = T) - 1) #not seen anywhere

  # above TOP
  # not there
  a_list[["TOP"]][,ncol(a_list[["TOP"]])] = abs(a_list[["PRO"]][,6] - 1)
  # SM1
  a_list[["TOP"]][,1] = dabom_list$Toppenish %>%
    select(matches('SM1')) %>%
    apply(1, max)
  # TP2
  a_list[["TOP"]][,2] = dabom_list$Toppenish %>%
    select(matches('TP2')) %>%
    apply(1, max)
  # TOP bb
  a_list[["TOP"]][,3] = if_else(rowSums(a_list[["TOP"]]) == 0, 1, 0)

  # above SUN
  # not there
  a_list[["SUN"]][,ncol(a_list[["SUN"]])] = abs(a_list[["PRO"]][,7] - 1)
  # AH1
  a_list[["SUN"]][,1] = dabom_list$Sunnyside %>%
    select(matches('AH1')) %>%
    apply(1, max)
  # LNR
  a_list[["SUN"]][,2] = dabom_list$Sunnyside %>%
    select(matches('LNR')) %>%
    apply(1, max)
  # LWC
  a_list[["SUN"]][,3] = dabom_list$Sunnyside %>%
    select(matches('LWC')) %>%
    apply(1, max)
  # ROZ
  a_list[["SUN"]][,4] = dabom_list$Sunnyside %>%
    select(ROZB0:LMTA0) %>%
    apply(1, max)
  # SUN bb
  a_list[["SUN"]][,5] = if_else(rowSums(a_list[["SUN"]]) == 0, 1, 0)

  # above ROZ
  # not there
  a_list[["ROZ"]][,ncol(a_list[["ROZ"]])] = abs(a_list[["SUN"]][,4] - 1)
  # LMC
  a_list[["ROZ"]][,1] = dabom_list$Sunnyside %>%
    select(matches('LMC')) %>%
    apply(1, max)
  # TAN
  a_list[["ROZ"]][,2] = dabom_list$Sunnyside %>%
    select(matches('TAN')) %>%
    apply(1, max)
  # SWK
  a_list[["ROZ"]][,3] = dabom_list$Sunnyside %>%
    select(matches('SWK')) %>%
    apply(1, max)
  # LMT
  a_list[["ROZ"]][,4] = dabom_list$Sunnyside %>%
    select(matches('LMT')) %>%
    apply(1, max)
  # ROZ bb
  a_list[["ROZ"]][,5] = if_else(rowSums(a_list[["ROZ"]]) == 0, 1, 0)

  # UMC
  z_list[["LMC"]] = dabom_list$Sunnyside %>%
    select(matches('UMC')) %>%
    apply(1, max)

  # compile for JAGS
  names(a_list) = paste0('a_', names(a_list))
  names(z_list) = paste0('z_', names(z_list))

  jags_inits <- function() {
    y = c(purrr::map(a_list,
                     .f = function(x) {
                       x %*% seq(1, ncol(x)) %>%
                         as.vector
                     }),
          purrr::map(z_list,
                     .f = as.vector))
    return(y)
  }

  return(jags_inits)
}
