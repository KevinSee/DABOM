#' @title Write DABOM JAGS model
#'
#' @description This writes the overall JAGS model for a generic DABOM as a text file. It can then be modified depending on the observations for a particular valid tag list.
#'
#' @author Kevin See
#'
#' @param file_name name (with file path) to save the model as
#'
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr stringr PITcleanr
#' @export
#' @return NULL
#' @examples writeDABOM()

writeDABOM = function(file_name = NULL,
                      parent_child = NULL,
                      configuration = NULL) {

  if(is.null(file_name)) file_name = 'DABOM.txt'

  # determine starting point (root_site)
  root_site = PITcleanr::buildNodeOrder(parent_child) %>%
    filter(node_order == 1) %>%
    pull(node)

  # add nodes to parent-child table
  pc_nodes = addParentChildNodes(parent_child,
                                 configuration = configuration)


  # how many child sites does each parent site have?
  n_child_df = parent_child %>%
    group_by(parent, parent_rkm) %>%
    summarise(across(child,
                     list(n_child = n_distinct),
                     .names = "{.fn}"),
              .groups = "drop") %>%
    arrange(parent_rkm)

  # get the column names of the capture history matrix
  col_nms = defineDabomColNms(root_site = root_site,
                              parent_child = parent_child,
                              configuration = configuration) %>%
    unlist() %>%
    as.vector()


  # how many nodes does each site have, what are their names and what column are they contained in?
  n_nodes = configuration %>%
    # filter(node %in% unique(c(pc_nodes$parent, pc_nodes$child))) %>%
    filter(node %in% pc_nodes$child) %>%
    mutate(node_site = if_else(nchar(node) > 3 & (grepl("A0$", node) | grepl("B0$", node)),
                               str_remove(str_remove(node, "B0$"), "A0$"),
                               node)) %>%
    group_by(site_code = node_site) %>%
    summarise(n_nodes = n_distinct(node),
              nodes = list(unique(node)),
              .groups = "drop") %>%
    unnest(cols = nodes) %>%
    rowwise() %>%
    mutate(matrix_col = if_else(nodes %in% col_nms,
                                grep(nodes, col_nms),
                                NA_integer_)) %>%
    ungroup() %>%
    arrange(matrix_col)

  #--------------------------------------------
  # write JAGS model
  #--------------------------------------------
  # open a connection to new model file
  mod_conn = file(file_name, open = 'w')
  write("model {\n",
        mod_conn)

  # detection priors
  write("\n# Priors for detection probabilities \n",
        mod_conn,
        append = T)
  n_nodes %>%
    filter(site_code != root_site) %>%
    mutate(prior = paste0(nodes, "_p ~ dbeta(1, 1);")) %>%
    pull(prior) %>%
    paste(collapse = "\n") %>%
    write(file = mod_conn,
          append = T)


  # setting priors and matrices for transition probabilities
  write("\n# Priors for transition probabilities \n",
        mod_conn,
        append = T)
  for(i in 1:nrow(n_child_df)) {
    site = n_child_df$parent[i]
    # cat(paste(site, ": \n"))
    childs = pc %>%
      filter(parent == site)
    n_branch = nrow(childs)

    node_df = n_nodes %>%
      filter(site_code == site)

    site_pc = pc %>%
      filter(child == site) %>%
      select(parent) %>%
      left_join(pc,
                by = "parent") %>%
      arrange(child_rkm)
    if(nrow(site_pc) == 0) {
      child_num = NA
    } else {
      child_num = site_pc %>%
        mutate(child_num = 1:n()) %>%
        filter(child == site) %>%
        pull(child_num)
      parent_site = pc %>%
        filter(child == site) %>%
        pull(parent)
    }

    if(n_branch == 1) {
      paste0("phi_", site, "[1] ~ dbeta(1, 1); \n",
             "phi_", site, "[2] ~ dbeta(1, 1); \n") %>%
        write(file = mod_conn,
              append = T)

      paste0("\n for (i in 1:n_fish) {
        eta_", site, "[i] ~ dbern(eta_", parent_site, "[i, ", child_num, "] * phi_", site, "[fishOrigin[i]])
      }") %>%
        write(file = mod_conn,
              append = T)
    } else {

      paste0("psi_", site, "[1, 1:n_branch_", site, "] ~ ddirch(", site, "_dirch_vec[1,]); \n",
             "psi_", site, "[2, 1:n_branch_", site, "] ~ ddirch(", site, "_dirch_vec[2,]); \n",
             "\n",
             "omega_", site, "[1, 1:n_branch_", site, "] <- zero_vec[1:(n_branch_", site, ")]; \n",
             "omega_", site, "[1, (n_branch_", site, " + 1)] <- 1; \n",
             "\n",
             "omega_", site, "[2, 1:n_branch_", site, "] <- psi_", site, "[1,]; \n",
             "omega_", site, "[2, (n_branch_", site, " + 1)] <- 0; \n",
             "\n",
             "omega_", site, "[3, 1:n_branch_", site, "] <- psi_", site, "[2,]; \n",
             "omega_", site, "[3, (n_branch_", site, " + 1)] <- 0; \n") %>%
        write(file = mod_conn,
              append = T)

      if(!is.na(child_num)) {
        paste0("\n for (i in 1:n_fish) {
      a_", site, "[i] ~ dcat( omega_", site, "[(eta_", parent_site, "[i,", child_num, "] * fishOrigin[i] + 1), 1:(n_pops_", site, "+1)] )
      for (j in 1:n_branch_", site, ")	{
        eta_", site, "[i,j] <- equals(a_", site, "[i],j) # equals(x,y) is a test for equality, returns [1,0]
      }\n") %>%
          write(file = mod_conn,
                append = T)
      } else {
        paste0("\n for (i in 1:n_fish) {
        a_", site, "[i] ~ dcat( omega_", site, "[fishOrigin[i], 1:(n_pops_", site, ")] )
        for (j in 1:n_branch_", site, ")	{
          eta_", site, "[i,j] <- equals(a_", site, "[i],j) # equals(x,y) is a test for equality, returns [1,0]
        }\n") %>%
          write(file = mod_conn,
                append = T)
      }
    }
  }

  # detection
  write("\n\n # Were tags observed? \n
        for (i in 1:n_fish) {\n",
        file = mod_conn,
        append = T)
  for(site in unique(n_nodes$site_code)) {
    node_df = n_nodes %>%
      filter(site_code == site)

    # find parent site
    parent_site = node_df %>%
      select(child = site_code) %>%
      distinct() %>%
      left_join(pc,
                by = "child") %>%
      pull(parent)

    dwn_site_pc = pc %>%
      filter(parent == parent_site) %>%
      arrange(child_rkm)

    if(nrow(dwn_site_pc) == 1) {
      for(j in 1:nrow(node_df)) {
        paste0("cap_hist[i,", node_df$matrix_col[j], "] ~ dbern( ", node_df$nodes[j], "_p * eta_", parent_site, "[i] );\n") %>%
          write(file = mod_conn,
                append = T)
      }
    } else {
      child_num = dwn_site_pc %>%
        mutate(id = 1:n()) %>%
        filter(child == node_df$site_code[1]) %>%
        pull(id)

      for(j in 1:nrow(node_df)) {
        paste0("cap_hist[i,", node_df$matrix_col[j], "] ~ dbern( ", node_df$nodes[j], "_p * eta_", parent_site, "[i,", child_num, "] );\n") %>%
          write(file = mod_conn,
                append = T)
      }

    }
  }

  write("}\n }",
        file = mod_conn,
        append = T)

  close(mod_conn)

}
