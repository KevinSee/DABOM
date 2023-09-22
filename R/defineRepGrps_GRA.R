#' @title Define Report Groups - GRA
#'
#' @description Define which detection sites fall into which report groups, and what the lowest array is. These reporting groups differ by species. These are specific to the Lower Granite version of DABOM.
#'
#' @author Kevin See
#'
#' @param spp Species, either "Chinook" or "Steelhead"
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples defineRepGrps_GRA("Chinook")

defineRepGrps_GRA = function(spp = c('Chinook', 'Steelhead')) {

  stopifnot(!is.null(node_order))

  spp = match.arg(spp)

  if(spp == 'Chinook') {

    grp_list = list('Upper Salmon River' = 'USE',
                     'Sawtooth Hatchery Weir' = 'STL',
                     'Valley Creek' = 'VC1D',
                     'Yankee Fork Salmon River' = 'YFKD',
                     'East Fork Salmon River' = 'SALEFT',
                     'Pahsimeroi River' = 'PAHH',
                     'Lemhi River' = 'LLRD',
                     'Carmen Creek' = 'CRCD',
                     'North Fork Salmon River' = 'NFSD',
                     'Bear Valley Creek' = NA,
                     'Big Creek' = 'TAYD',
                     'South Fork Salmon River' = 'SFG',
                     'McCall Hatchery Weir' = 'STR',
                     'South Fork Salmon River mainstem' = 'KRS',
                     'EFSF Salmon River' = 'ESSD',
                     'Secesh River' = 'ZEND',
                     'Rapid River' = 'RAPH',
                     'Lolo River' = 'LC1D',
                     'South Fork Clearwater' = 'SC1D',
                     'Clear Creek' = 'CLCD',
                     'Imnaha River' = 'IR1D',
                     'Cow Creek' = 'COCD',
                     'Big Sheep Creek' = c('BSCD', 'LSHEEF'),
                     'Grande Ronde River upper mainstem' = 'UGR',
                     'Catherine Creek' = 'CCWD',
                     'Wallowa River' = 'WR1',
                     'Lookingglass Creek' = 'LOOKGC',
                     'Asotin Creek' = 'ACMD',
                     'Tucannon River' = 'LTR',
                     'Tucannon Hatchery Weir' = 'TFHD')
  }

  if(spp == 'Steelhead') {

    grp_list = list('Upper Salmon River' = 'USE',
                     'Sawtooth Hatchery Weir' = 'STL',
                     'Upper Salmon River mainstem' = c('YFKD', 'VC1D', 'STL'),
                     'Valley Creek' = 'VC1D',
                     'Yankee Fork Salmon River' = 'YFKD',
                     'East Fork Salmon River' = 'SALEFT',
                     'Pahsimeroi River' = 'PAHH',
                     'Lemhi River' = 'LLRD',
                     'Carmen Creek' = 'CRCD',
                     'Big Creek' = 'TAYD',
                     'South Fork Salmon River' = 'SFG',
                     'South Fork Salmon River mainstem' = c('KRS', 'ESSD'),
                     'Secesh River' = 'ZEND',
                     'Rapid River' = 'RAPH',
                     'Fish Creek' = 'FISTRP',
                     'Lolo Creek' = 'LC1D',
                     'South Fork Clearwater' = 'SC1D',
                     'Clear Creek' = 'CLCD',
                     'Potlatch River' = 'JUL',
                     'Potlatch above HLM' = 'HLMD',
                     'Potlatch above KHS' = 'KHSD',
                     'Lapwai Creek' = 'LAPD',
                     'Imnaha River' = 'IR1D',
                     'Cow Creek' = 'COCD',
                     'Big Sheep Creek' = 'BSCD',
                     'Grande Ronde River upper mainstem' = 'UGR',
                     'Lookingglass Creek' = 'LOOKGC',
                     'Wallowa River' = 'WR1',
                     'Joseph Creek' = 'JOCD',
                     'Asotin Creek' = 'ACMD',
                     'Tenmile Creek' = 'TENMC2',
                     'Alpowa Creek' = 'ALPOWC',
                     'Tucannon River' = 'LTR',
                     'Tucannon Hatchery Weir' = 'TFHD')
  }

  report_df = grp_list %>%
    map_df(.id = "report_grp",
           .f = function(x) tibble(low_node = x)) %>%
    mutate(low_site = if_else(stringr::str_count(low_node) >= 5 &
                                (stringr::str_detect(low_node, "D$") | stringr::str_detect(low_node, "U$")),
                              stringr::str_remove(stringr::str_remove(low_node, "U$"), "D$"),
                              low_node))


  # report_df = report_df %>%
  #   left_join(node_order,
  #             by = c('lowNode' = 'Node'))

  return(report_df)

}

