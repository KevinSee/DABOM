#' @title Define Report Groups - GRA
#'
#' @description Define which detection sites fall into which report groups, and what the lowest array is. These reporting groups differ by species.
#'
#' @author Kevin See
#'
#' @param spp Species, either "Chinook" or "Steelhead"
#'
#' @import dplyr stringr
#' @export
#' @return NULL
#' @examples defineRepGrps()

defineRepGrps = function(spp = c('Chinook', 'Steelhead')) {

  stopifnot(!is.null(node_order))

  spp = match.arg(spp)

  if(spp == 'Chinook') {

    grp_list = list('Upper Salmon River' = 'USE',
                     'Sawtooth Hatchery Weir' = 'STL',
                     'Valley Creek' = 'VC1B0',
                     'Yankee Fork Salmon River' = 'YFKB0',
                     'East Fork Salmon River' = 'SALEFT',
                     'Pahsimeroi River' = 'PAHH',
                     'Lemhi River' = 'LLRB0',
                     'Carmen Creek' = 'CRCB0',
                     'North Fork Salmon River' = 'NFSB0',
                     'Bear Valley Creek' = NA,
                     'Big Creek' = 'TAYB0',
                     'South Fork Salmon River' = 'SFG',
                     'McCall Hatchery Weir' = 'STR',
                     'South Fork Salmon River mainstem' = 'KRS',
                     'EFSF Salmon River' = 'ESSB0',
                     'Secesh River' = 'ZENB0',
                     'Rapid River' = 'RAPH',
                     'Lolo River' = 'LC1B0',
                     'South Fork Clearwater' = 'SC1B0',
                     'Clear Creek' = 'CLCB0',
                     'Imnaha River' = 'IR1B0',
                     'Cow Creek' = 'COCB0',
                     'Big Sheep Creek' = c('BSCB0', 'LSHEEF'),
                     'Grande Ronde River upper mainstem' = 'UGR',
                     'Catherine Creek' = 'CCWB0',
                     'Wallowa River' = 'WR1',
                     'Lookingglass Creek' = 'LOOKGC',
                     'Asotin Creek' = 'ACMB0',
                     'Tucannon River' = 'LTR',
                     'Tucannon Hatchery Weir' = 'TFHB0')
  }

  if(spp == 'Steelhead') {

    grp_list = list('Upper Salmon River' = 'USE',
                     'Sawtooth Hatchery Weir' = 'STL',
                     'Upper Salmon River mainstem' = c('YFKB0', 'VC1B0', 'STL'),
                     'Valley Creek' = 'VC1B0',
                     'Yankee Fork Salmon River' = 'YFKB0',
                     'East Fork Salmon River' = 'SALEFT',
                     'Pahsimeroi River' = 'PAHH',
                     'Lemhi River' = 'LLRB0',
                     'Carmen Creek' = 'CRCB0',
                     'Big Creek' = 'TAYB0',
                     'South Fork Salmon River' = 'SFG',
                     'South Fork Salmon River mainstem' = c('KRS', 'ESSB0'),
                     'Secesh River' = 'ZENB0',
                     'Rapid River' = 'RAPH',
                     'Fish Creek' = 'FISTRP',
                     'Lolo Creek' = 'LC1B0',
                     'South Fork Clearwater' = 'SC1B0',
                     'Clear Creek' = 'CLCB0',
                     'Potlatch River' = 'JUL',
                     'Potlatch above HLM' = 'HLMB0',
                     'Potlatch above KHS' = 'KHSB0',
                     'Lapwai Creek' = 'LAPB0',
                     'Imnaha River' = 'IR1B0',
                     'Cow Creek' = 'COCB0',
                     'Big Sheep Creek' = 'BSCB0',
                     'Grande Ronde River upper mainstem' = 'UGR',
                     'Lookingglass Creek' = 'LOOKGC',
                     'Wallowa River' = 'WR1',
                     'Joseph Creek' = 'JOCB0',
                     'Asotin Creek' = 'ACMB0',
                     'Tenmile Creek' = 'TENMC2',
                     'Alpowa Creek' = 'ALPOWC',
                     'Tucannon River' = 'LTR',
                     'Tucannon Hatchery Weir' = 'TFHB0')
  }

  report_df = grp_list %>%
    map_df(.id = "report_grp",
           .f = function(x) tibble(low_node = x)) %>%
    mutate(low_site = if_else(stringr::str_count(low_node) >= 5 &
                                (stringr::str_detect(low_node, "B0$") | stringr::str_detect(low_node, "A0$")),
                              stringr::str_remove(stringr::str_remove(low_node, "A0$"), "B0$"),
                              low_node))


  # report_df = report_df %>%
  #   left_join(node_order,
  #             by = c('lowNode' = 'Node'))

  return(report_df)

}

