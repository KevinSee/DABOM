#' @title Write DABOM JAGS model
#'
#' @description This writes the overall JAGS model for a generic DABOM as a text file. It can then be modified depending on the observations for a particular valid tag list.
#'
#' @author Kevin See
#'
#' @param file_name name (with file path) to save the model as
#' @param time_varying Should the initial movement probabilities be time-varying? Default value is `FALSE`
#'
#' @inheritParams createDABOMcapHist
#'
#' @import dplyr stringr PITcleanr
#' @export
#' @return NULL
#' @examples writeDABOM()

writeDABOM = function(file_name = NULL,
                      parent_child = NULL,
                      configuration = NULL,
                      time_varying = FALSE) {

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
    mutate(n_child = n_distinct(child)) %>%
    ungroup()

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
    suppressWarnings(rm(n_branch))
  }

  # Where is each fish?
  write(paste0("# Where is each fish? \n\n",
               "\t for(i in 1:n_fish) { \n",
               "\n"),
        mod_conn,
        append = T)
  for(site in unique(parent_info$parent)) {
    # how many branches past this site?
    n_branch = parent_info %>%
      filter(parent == site) %>%
      pull(n_child) %>%
      unique()

    # what is the parent site?
    parent_site = node_info %>%
      filter(site_code == site) %>%
      pull(parent_site) %>%
      unique()

    # what is the branch number of this site from its parent?
    child_num = node_info %>%
      filter(site_code == site) %>%
      pull(child_num) %>%
      unique()

    # how many children does parent site have?
    if(length(parent_site) > 0) {
      parent_n_child = parent_info %>%
        filter(parent == parent_site) %>%
        pull(n_child) %>%
        unique()
    } else {
      parent_n_child = NA_integer_
    }

    if(n_branch == 1) {
      if(parent_n_child > 1) {
        fish_pos_text = paste0("\t\t eta_", site, "[i] ~ dbern(eta_", parent_site, "[i, ", child_num, "] * phi_", site, "[fish_type[i]]) \n")
      } else if(parent_n_child == 1) {
        fish_pos_text = paste0("\t\t eta_", site, "[i] ~ dbern(eta_", parent_site, "[i] * phi_", site, "[fish_type[i]]) \n")
      }
    }

    if(n_branch > 1) {
      if(length(child_num) > 0) {
        if(parent_n_child == 1) {
          a_line = paste0("\t\t a_", site, "[i] ~ dcat( omega_", site, "[(eta_", parent_site, "[i] * fish_type[i] + 1), 1:(n_branch_", site, "+1)] ) \n")
        } else {
          a_line = paste0("\t\t a_", site, "[i] ~ dcat( omega_", site, "[(eta_", parent_site, "[i,", child_num, "] * fish_type[i] + 1), 1:(n_branch_", site, "+1)] ) \n")
        }
        fish_pos_text = paste0(a_line,
                               "\t\t\t for (j in 1:n_branch_", site, ")	{ \n",
                               "\t\t\t\t eta_", site, "[i,j] <- equals(a_", site, "[i],j) # equals(x,y) is a test for equality, returns [1,0] \n",
                               "\t\t\t }\n")
      } else {
        fish_pos_text = paste0("\t\t a_", site, "[i] ~ dcat( omega_", site, "[fish_type[i] + 1, 1:(n_branch_", site, "+1)] ) \n",
                               "\t\t\t for (j in 1:n_branch_", site, ")	{ \n",
                               "\t\t\t\t eta_", site, "[i,j] <- equals(a_", site, "[i],j) # equals(x,y) is a test for equality, returns [1,0] \n",
                               "\t\t\t }\n")
      }
    }
    write(fish_pos_text,
          file = mod_conn,
          append = T)

    suppressWarnings(rm(n_branch,
                        parent_site,
                        child_num,
                        parent_n_child,
                        a_line,
                        fish_pos_text))
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

    dwn_site_pc = parent_child %>%
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
    suppressWarnings(rm(node_df,
                        parent_site,
                        dwn_site_pc,
                        child_num))
  }

  write(paste0("\t}  # end the n_fish loop \n",
               "}"),
        file = mod_conn,
        append = T)

  close(mod_conn)

  if(time_varying) {
    mod_conn_init = file(file_name, open = 'r+')
    # read in text file
    mod_file_org = readLines(mod_conn_init)
    # save a version of original
    mod_file = mod_file_org

    new_prior_text = paste0("\t # Set up time-varying movement probabilities for the initial branches\n",
                            "\t # i indexes the origin of fish (wild/hatchery)\n",
                            "\t # j indexes the branch\n",
                            "\t # t indexes the time strata (e.g. week)\n\n",
                            "\t # prior on log odds ratio for initial week \n",
                            "\t for(i in 1:2) { \n",
                            "\t\t for(j in 1:(n_branch_", root_site, " - 1)) { \n",
                            "\t\t # somewhat informative, but fairly vague prior \n",
                            "\t\t\t lambda[i,j,1] ~ dnorm(-2, 1/16); \n",
                            "\t\t\t exp_lambda[i,j,1] <- exp(lambda[i,j,1]) * ", root_site, "_dirch_vec[i,j];\n",
                            "\t\t }\n",
                            "\t\t # set black box as baseline \n",
                            "\t\t for(t in 1:(n_strata)) { \n",
                            "\t\t\t lambda[i,n_branch_", root_site, ", t] <- 0; \n",
                            "\t\t\t exp_lambda[i,n_branch_", root_site, ", t] <- exp(lambda[i,n_branch_", root_site, ", t]) * ", root_site, "_dirch_vec[i,n_branch_", root_site, "]; \n",
                            "\t\t\t # get sum of all lambda's \n",
                            "\t\t\t sum_exp_lambda[i,t] <- sum(exp_lambda[i,,t]); \n",
                            "\t\t }\n",
                            "\t }\n",
                            "\n",
                            "\t # extract initial movement probabilities for week 1 \n",
                            "\t for(i in 1:2) { \n",
                            "\t\t for(j in 1:n_branch_", root_site, ") { \n",
                            "\t\t\t psi_", root_site, "[i,j,1] <- ifelse(", root_site, "_dirch_vec[i,j] == 0, 0, exp_lambda[i,j,1] / sum_exp_lambda[i,1]); \n",
                            "\t\t }\n",
                            "\t }\n",
                            "\t # variation in time-varying random walk movement probabilities \n",
                            "\t sigma_rw ~ dunif(0,10); \n",
                            "\t tau_rw <- pow(sigma_rw, -2); \n",
                            "\n",
                            "\t for(i in 1:2) { \n",
                            "\t\t for(t in 2:(n_strata)) { \n",
                            "\t\t\t for(j in 1:(n_branch_", root_site, " - 1)) { \n",
                            "\t\t\t\t epsilon[i,j,t] ~ dnorm(0, tau_rw); \n",
                            "\t\t\t\t # set lambda to any main bin that saw NO fish to 0 \n",
                            "\t\t\t\t lambda[i,j,t] <- ifelse(", root_site, "_dirch_vec[i,j] == 0, 0, lambda[i,j,t - 1] + epsilon[i,j,t]); \n",
                            "\t\t\t\t exp_lambda[i,j,t] <- exp(lambda[i,j,t]) * ", root_site, "_dirch_vec[i,j]; \n",
                            "\t\t\t }\n",
                            "\t\t\t for (j in 1:(n_branch_", root_site, ")) { \n",
                            "\t\t\t\t psi_", root_site, "[i,j,t] <- (exp_lambda[i,j,t] / sum_exp_lambda[i,t]); \n",
                            "\t\t\t }\n",
                            "\t\t }\n",
                            "\t }\n",
                            "\n",
                            "\t for(t in 1:n_strata) { \n",
                            "\t\t omega_", root_site, "[1, 1:n_branch_", root_site, ", t] <- zero_vec[1:(n_branch_", root_site, ")]; \n",
                            "\t\t omega_", root_site, "[1, (n_branch_", root_site, " + 1), t] <- 1; \n",
                            "\n",
                            "\t\t omega_", root_site, "[2, 1:n_branch_", root_site, ", t] <- psi_", root_site, "[1,,t]; \n",
                            "\t\t omega_", root_site, "[2, (n_branch_", root_site, " + 1), t] <- 0; \n",
                            "\n",
                            "\t\t omega_", root_site, "[3, 1:n_branch_", root_site, ", t] <- psi_", root_site, "[2,,t]; \n",
                            "\t\t omega_", root_site, "[3, (n_branch_", root_site, " + 1), t] <- 0; \n",
                            "\t }\n")

    # where in the model file are the priors that need to be replace?
    line_range = range(str_which(mod_file, paste0('psi_', root_site)))
    # drop all those lines but the first (including one addtional line at the end)
    mod_file = mod_file[-c((line_range[1]+1):(line_range[2]+1))]
    # replace first line with new text
    mod_file[line_range[1]] = new_prior_text

    # in the "where are the fish" section, be sure to index the dam_strata
    mod_file[str_which(mod_file, paste0("a_", root_site, "\\[i\\] ~ dcat"))] = paste0("\t\t a_", root_site, "[i] ~ dcat( omega_", root_site, "[fish_type[i] + 1, 1:(n_branch_", root_site, "+1), dam_strata[i]] )")

    # overwrite text file
    writeLines(mod_file, mod_conn_init)
    close(mod_conn_init)
    rm(mod_conn_init)

  }

}
