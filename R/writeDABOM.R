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

  # how many nodes does each site have, what are their names and what column are they contained in?
  node_info = getNodeInfo(parent_child,
                          configuration)

  # how many child sites does each parent site have?
  parent_info = parent_child %>%
    group_by(parent, parent_rkm) %>%
    mutate(n_child = n_distinct(child))

  #--------------------------------------------
  # write JAGS model
  #--------------------------------------------
  # open a connection to new model file
  mod_conn = file(file_name, open = 'w')
  write("model {\n",
        mod_conn)

  # detection priors
  write("# Priors for detection probabilities \n",
        mod_conn,
        append = T)
  node_info %>%
    filter(site_code != root_site) %>%
    mutate(prior = paste0("\t ", node, "_p ~ dbeta(1, 1);")) %>%
    pull(prior) %>%
    paste(collapse = "\n") %>%
    write(file = mod_conn,
          append = T)


  # setting priors and matrices for transition probabilities
  write("\n# Priors for transition probabilities \n",
        mod_conn,
        append = T)
  for(site in unique(parent_info$parent)) {
    n_branch = parent_info %>%
      filter(parent == site) %>%
      pull(n_child) %>%
      unique()

    if(n_branch == 1) {
      move_prior_text = paste0("\t phi_", site, "[1] ~ dbeta(1, 1); \n",
                               "\t phi_", site, "[2] ~ dbeta(1, 1); \n")
    } else {

      move_prior_text = paste0("\t psi_", site, "[1, 1:n_branch_", site, "] ~ ddirch(", site, "_dirch_vec[1,]); \n",
                               "\t psi_", site, "[2, 1:n_branch_", site, "] ~ ddirch(", site, "_dirch_vec[2,]); \n",
                               "\n",
                               "\t omega_", site, "[1, 1:n_branch_", site, "] <- zero_vec[1:(n_branch_", site, ")]; \n",
                               "\t omega_", site, "[1, (n_branch_", site, " + 1)] <- 1; \n",
                               "\n",
                               "\t omega_", site, "[2, 1:n_branch_", site, "] <- psi_", site, "[1,]; \n",
                               "\t omega_", site, "[2, (n_branch_", site, " + 1)] <- 0; \n",
                               "\n",
                               "\t omega_", site, "[3, 1:n_branch_", site, "] <- psi_", site, "[2,]; \n",
                               "\t omega_", site, "[3, (n_branch_", site, " + 1)] <- 0; \n")
    }
    write(move_prior_text,
          file = mod_conn,
          append = T)
  }

  # Where is each fish?
  write(paste0("# Where is each fish? \n\n",
               "\t for(i in 1:n_fish) { \n",
        "\n"),
        mod_conn,
        append = T)
  for(site in unique(parent_info$parent)) {
    n_branch = parent_info %>%
      filter(parent == site) %>%
      pull(n_child) %>%
      unique()

    parent_site = node_info %>%
      filter(site_code == site) %>%
      pull(parent_site) %>%
      unique()

    child_num = node_info %>%
      filter(site_code == site) %>%
      pull(child_num) %>%
      unique()

    if(n_branch == 1) {
      fish_pos_text = paste0("\t\t eta_", site, "[i] ~ dbern(eta_", parent_site, "[i, ", child_num, "] * phi_", site, "[fish_type[i]]) \n")
      write(fish_pos_text,
            file = mod_conn,
            append = T)
    }

    if(n_branch > 1) {
      if(length(child_num) > 0) {
        fish_pos_text = paste0("\t\t a_", site, "[i] ~ dcat( omega_", site, "[(eta_", parent_site, "[i,", child_num, "] * fish_type[i] + 1), 1:(n_branch_", site, "+1)] ) \n",
                               "\t\t\t for (j in 1:n_branch_", site, ")	{ \n",
                               "\t\t\t\t eta_", site, "[i,j] <- equals(a_", site, "[i],j) # equals(x,y) is a test for equality, returns [1,0] \n",
                               "\t\t\t }\n")
      } else {
      fish_pos_text = paste0("\t\t a_", site, "[i] ~ dcat( omega_", site, "[fish_type[i] + 1, 1:(n_branch_", site, "+1)] ) \n",
                             "\t\t\t for (j in 1:n_branch_", site, ")	{ \n",
                             "\t\t\t\t eta_", site, "[i,j] <- equals(a_", site, "[i],j) # equals(x,y) is a test for equality, returns [1,0] \n",
                             "\t\t\t }\n")
      }
      write(fish_pos_text,
            file = mod_conn,
            append = T)
    }
  }

  write("\t} # end the n_fish loop \n",
        file = mod_conn,
        append = T)

  # detection
  write(paste0("\n# Were tags observed? \n\n",
               "\t for (i in 1:n_fish) {\n"),
        file = mod_conn,
        append = T)
  for(site in unique(node_info$site_code)) {
    node_df = node_info %>%
      filter(site_code == site)

    # find parent site
    parent_site = node_df %>%
      select(child = site_code) %>%
      distinct() %>%
      left_join(parent_child,
                by = "child") %>%
      pull(parent)

    dwn_site_pc = pc %>%
      filter(parent == parent_site) %>%
      arrange(child_rkm)

    if(nrow(dwn_site_pc) == 1) {
      for(j in 1:nrow(node_df)) {
        paste0("\t\t cap_hist[i,", node_df$matrix_col[j], "] ~ dbern( ", node_df$node[j], "_p * eta_", parent_site, "[i] );\n") %>%
          write(file = mod_conn,
                append = T)
      }
    } else {
      child_num = dwn_site_pc %>%
        mutate(id = 1:n()) %>%
        filter(child == node_df$site_code[1]) %>%
        pull(id)

      for(j in 1:nrow(node_df)) {
        paste0("\t\t cap_hist[i,", node_df$matrix_col[j], "] ~ dbern( ", node_df$node[j], "_p * eta_", parent_site, "[i,", child_num, "] );\n") %>%
          write(file = mod_conn,
                append = T)
      }

    }
  }

  write(paste0("\t}  # end the n_fish loop \n",
               "}"),
        file = mod_conn,
        append = T)

  close(mod_conn)

}
