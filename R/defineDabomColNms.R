#' @title Define Column Names for DABOM matrix
#'
#' @description based on a parent-child table, this returns a vector of nodes to help create a consistent
#' series of DABOM input matrices.
#'
#' @author Kevin See
#'
#' @param root_site determines which version of DABOM the user is running.
#' @param parent_child parent-child table. Could be created from `buildParentChild()` from `PITcleanr` package.
#' @param configuration configuration file. Could be created from `buildConfig()` from `PITcleanr` package.
#' @param second_node Default option is `TRUE` which defines the `bottom_sites` as the second order nodes above the `root_site`, (i.e., `node_order == 2`). If `FALSE`, some sites are hard-coded depending on what the `root_site` is set to.  Default is `TRUE`; otherwise `bottom_sites` should be defined within the `defineDabomColNms()`
#' function
#'
#' @import dplyr purrr PITcleanr
#' @importFrom magrittr %<>%
#' @export
#' @return NULL
#' @examples defineDabomColNms()

defineDabomColNms = function(root_site = NA,
                             parent_child,
                             configuration,
                             second_node = TRUE) {

  # root_site = match.arg(root_site)

  site_order = PITcleanr::buildNodeOrder(parent_child)

  if( second_node ) {

    # use second order nodes
    bottom_sites = site_order %>%
      filter(node_order == 2) %>%
      pull(node) |>
      as.list()

    names(bottom_sites) = bottom_sites
  }

  if( !second_node ) {

    if(root_site == "GRA") {

      # define bottom sites by hand
      bottom_sites = list(Tucannon = "LTR",
                          Penawawa = "PENAWC",
                          Almota = "ALMOTC",
                          Alpowa = "ALPOWC",
                          Asotin = "ACM",
                          TenMileCreek = "TENMC2",
                          Lapwai = "LAP",
                          Potlatch = "JUL",
                          JosephCreek = c("JOC", "JOSEPC"),
                          CowCreek = "COC",
                          ImnahaRiver = "IR1",
                          Lolo = "LC1",
                          SFClearwater = "SC1",
                          Wenaha = "WEN",
                          ClearCreek = c("CLC", "KOOS"),
                          Lochsa = "LRL",
                          Selway = "SW1",
                          LookingGlass = "LOOKGC",
                          Wallowa = "WR1",
                          GrandeRonde = "UGR",
                          RapidRiver = "RAPH",
                          SFSalmon = "SFG",
                          Panther = "PCA",
                          BigCreek = "TAY",
                          NFSalmon = "NFS",
                          CarmenCreek = "CRC",
                          Lemhi = "LLR",
                          UpperSalmon = "USE",
                          BearValley = "BRC")

    } else if(root_site == "PRA") {
      bottom_sites = list(BelowPriest = c("JDA", "ICH", "RSH", "PRH", "JD1", "PRO", "TMF", "PRV"),
                          Wenatchee = "LWE",
                          Entiat = c("ENL", "WEH", "EBO"),
                          Methow = "LMR",
                          Okanogan = "OKL")
    } else if(root_site == "TUM") {
      bottom_sites = list(Peshastin = "PES",
                          Icicle = "ICL",
                          Chiwaukum = "CHW",
                          Chiwawa = "CHL",
                          Nason = "NAL",
                          WhiteRiver = "WTL",
                          LittleWenatchee = "LWN")
    } else if(root_site == "PRO") {
      bottom_sites = list(Downstream = c("JDA", "ICH", "JD1", "PRA", "MCN"),
                          Status = "SAT",
                          Toppenish = "TOP",
                          Sunnyside = "SUN")

    } else {
      bottom_sites = list(Start = root_site)
    }
  }


  site_node_list = bottom_sites %>%
    map(.f = function(x) {
      x %>%
        as.list() %>%
        map_df(.f = function(y) {
          site_order %>%
            filter(grepl(y, path)) %>%
            select(node)
        })
    }) %>%
    map(.f = function(x) {
      # x %>%
      #   rename(child = node) %>%
      #   left_join(parent_child,
      #             by = "child") %>%
      #   distinct() %>%
      #   filter(!is.na(parent)) %>%
      #   PITcleanr::addParentChildNodes(configuration) %>%
      #   select(node = child,
      #          # node_hydro = child_hydro,
      #          node_rkm = child_rkm) %>%
      #   arrange(node_rkm,
      #           desc(node))

      pc <-
        x %>%
        rename(child = node) %>%
        left_join(parent_child,
                  by = "child") %>%
        distinct() %>%
        relocate(child,
                 .after = parent) %>%
        filter(!is.na(parent)) %>%
        PITcleanr::addParentChildNodes(configuration)

      pc %>%
        left_join(PITcleanr::buildNodeOrder(pc, direction = "u"),
                  by = join_by(child == node)) %>%
        arrange(path,
                node_order,
                desc(child)) |>
        select(node = child)

    }) %>%
    map(.f = function(x) {
      pull(x, node)
    })

  if(root_site == "PRA") {
    site_node_list$Wenatchee %<>%
      c("RIA", "CLK", .)

    site_node_list$Entiat %<>%
      c("RRF", .)

    site_node_list$Methow %<>%
      c("WEA", .)

    site_node_list$Okanogan %<>%
      c(., "FST")
  }

  site_node_list %<>%
    map(~ factor(., levels = .))

  return(site_node_list)

}
