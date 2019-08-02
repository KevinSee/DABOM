#' @title Write Lower Granite Dam JAGS model
#'
#' @description This writes the overall JAGS model for LGR DABOM as a text file. It can then be modified depending on the observations for a particular valid tag list.
#'
#' @author Kevin See
#'
#' @param file_name name (with file path) to save the model as
#' @param time_varying Should the initial movement probabilities be time-varying? Default value is \code{TRUE}
#'
#' @export
#' @return NULL
#' @examples writeJAGSmodel_LGD()

writeDABOM_LGD = function(file_name = NULL,
                          time_varying = TRUE) {

  if(is.null(file_name)) file_name = 'LGD_DABOM.txt'

  model_code = '
model{

  # Set up array detection efficiency priors

  #--------------------------------
  # SW Washington sites
  #--------------------------------
  ACMA0_p ~ dbeta(1,1)
  ACMB0_p ~ dbeta(1,1)
  GEORGC_p <- 1 # assume perfect detection
  ASOTIC_p ~ dbeta(1,1)
  ACBA0_p ~ dbeta(1,1)
  ACBB0_p ~ dbeta(1,1)
  AFCA0_p ~ dbeta(1,1)
  AFCB0_p ~ dbeta(1,1)
  CCAA0_p ~ dbeta(1,1)
  CCAB0_p ~ dbeta(1,1)

  LTR_p ~ dbeta(1,1)
  MTR_p ~ dbeta(1,1)
  UTR_p ~ dbeta(1,1)
  TUCH_p <- 1 # assume perfect detection

  ALMOTC_p <- 1 # assume perfect detection
  ALPOWC_p <- 1 # assume perfect detection
  PENAWC_p <- 1 # assume perfect detection
  TENMC2_p <- 1 # assume perfect detection

  #--------------------------------
  # Clearwater sites
  #--------------------------------
  LAPA0_p ~ dbeta(1,1)
  LAPB0_p ~ dbeta(1,1)
  SWTA0_p ~ dbeta(1,1)
  SWTB0_p ~ dbeta(1,1)
  WEBA0_p ~ dbeta(1,1)
  WEBB0_p ~ dbeta(1,1)
  MISA0_p ~ dbeta(1,1)
  MISB0_p ~ dbeta(1,1)

  JUL_p ~ dbeta(1,1)
  HLMA0_p ~ dbeta(1,1)
  HLMB0_p ~ dbeta(1,1)
  POTREF_p <- 1 # assume perfect detection
  POTRWF_p <- 1 # assume perfect detection
  KHSA0_p ~ dbeta(1,1)
  KHSB0_p ~ dbeta(1,1)
  LBEARC_p <- 1 # assume perfect detection
  BIGBEC_p <- 1 # assume perfect detection
  PCMA0_p ~ dbeta(1,1)
  PCMB0_p ~ dbeta(1,1)

  LC1_p ~ dbeta(1,1)
  LC2_p ~ dbeta(1,1)

  SC1_p ~ dbeta(1,1)
  SC2B0_p ~ dbeta(1,1)
  SC2A0_p ~ dbeta(1,1)

  CLC_p ~ dbeta(1,1)
  KOOS_p ~ dbeta(1,1)

  LRL_p ~ dbeta(1,1)
  LRU_p ~ dbeta(1,1)
  FISTRP_p <- 1 # assume perfect detection

  SW1_p ~ dbeta(1,1)
  SW2_p ~ dbeta(1,1)

  #--------------------------------
  # NE Oregon sites
  #--------------------------------
  JOCA0_p ~ dbeta(1,1)
  JOCB0_p ~ dbeta(1,1)
  JOSEPC_p ~ dbeta(1,1)

  WR1_p ~ dbeta(1,1)
  BCANF_p <- 1 # assume perfect detection
  WR2B0_p ~ dbeta(1,1)
  WR2A0_p ~ dbeta(1,1)
  WALH_p <- 1 # assume perfect detection
  LOSTIW_p <- 1 # assume perfect detection


  LOOKGC_p <- 1 # assume perfect detection

  UGR_p ~ dbeta(1,1)
  CCWA0_p ~ dbeta(1,1)
  CCWB0_p ~ dbeta(1,1)
  CATHEW_p ~ dbeta(1,1)

  UGSB0_p ~ dbeta(1,1)
  UGSA0_p ~ dbeta(1,1)
  GRANDW_p <- 1 # assume perfect detection

  COCA0_p ~ dbeta(1,1)
  COCB0_p ~ dbeta(1,1)

  IR1_p ~ dbeta(1,1)
  IR2_p ~ dbeta(1,1)
  BSCA0_p ~ dbeta(1,1)
  BSCB0_p ~ dbeta(1,1)
  CMPA0_p ~ dbeta(1,1)
  CMPB0_p ~ dbeta(1,1)
  HORS3C_p <- 1 # assume perfect detection
  LSHEEF_p <- 1 # assume perfect detection
  IR3A0_p ~ dbeta(1,1)
  IR3B0_p ~ dbeta(1,1)
  CZYA0_p ~ dbeta(1,1)
  CZYB0_p ~ dbeta(1,1)
  FREEZC_p <- 1 # assume perfect detection
  IR4A0_p ~ dbeta(1,1)
  IR4B0_p ~ dbeta(1,1)
  MAHOGC_p <- 1 # assume perfect detection
  IMLA0_p ~ dbeta(1,1)
  IMLB0_p ~ dbeta(1,1)
  IMNAHW_p ~ dbeta(1,1)
  IR5A0_p ~ dbeta(1,1)
  IR5B0_p ~ dbeta(1,1)
  GUMBTC_p <- 1 # assume perfect detection
  DRY2C_p <- 1 # assume perfect detection

  WENB0_p ~ dbeta(1,1)
  WENA0_p ~ dbeta(1,1)

  #--------------------------------
  # Salmon branch sites
  #--------------------------------
  RAPH_p <- 1 # assume perfect detection

  SFG_p ~ dbeta(1,1)
  ESSA0_p ~ dbeta(1,1)
  ESSB0_p ~ dbeta(1,1)
  JOHNSC_p <- 1 # assume perfect detection
  KRS_p ~ dbeta(1,1)
  STR_p <- 1 # assume perfect detection
  ZENA0_p ~ dbeta(1,1)
  ZENB0_p ~ dbeta(1,1)
  LAKEC_p <- 1 # assume perfect detection

  PCAB0_p ~ dbeta(1,1)
  PCAA0_p ~ dbeta(1,1)

  TAYA0_p ~ dbeta(1,1)
  TAYB0_p ~ dbeta(1,1)

  NFSA0_p ~ dbeta(1,1)
  NFSB0_p ~ dbeta(1,1)

  CRCA0_p ~ dbeta(1,1)
  CRCB0_p ~ dbeta(1,1)

  LLRA0_p ~ dbeta(1,1)
  LLRB0_p ~ dbeta(1,1)
  AGCA0_p ~ dbeta(1,1)
  AGCB0_p ~ dbeta(1,1)
  KENA0_p ~ dbeta(1,1)
  KENB0_p ~ dbeta(1,1)
  HYCA0_p ~ dbeta(1,1)
  HYCB0_p ~ dbeta(1,1)
  LRWA0_p ~ dbeta(1,1)
  LRWB0_p ~ dbeta(1,1)
  WPCA0_p ~ dbeta(1,1)
  WPCB0_p ~ dbeta(1,1)
  BHCA0_p ~ dbeta(1,1)
  BHCB0_p ~ dbeta(1,1)
  LLSA0_p ~ dbeta(1,1)
  LLSB0_p ~ dbeta(1,1)
  BTLA0_p ~ dbeta(1,1)
  BTLB0_p ~ dbeta(1,1)
  BTMA0_p ~ dbeta(1,1)
  BTMB0_p ~ dbeta(1,1)
  BTUA0_p ~ dbeta(1,1)
  BTUB0_p ~ dbeta(1,1)
  LCLA0_p ~ dbeta(1,1)
  LCLB0_p ~ dbeta(1,1)
  LB8A0_p ~ dbeta(1,1)
  LB8B0_p ~ dbeta(1,1)
  LBSA0_p ~ dbeta(1,1)
  LBSB0_p ~ dbeta(1,1)
  CACA0_p ~ dbeta(1,1)
  CACB0_p ~ dbeta(1,1)
  HECA0_p ~ dbeta(1,1)
  HECB0_p ~ dbeta(1,1)

  USE_p ~ dbeta(1,1)
  USI_p ~ dbeta(1,1)
  VC1_p ~ dbeta(1,1)
  VC2_p ~ dbeta(1,1)
  YFKA0_p ~ dbeta(1,1)
  YFKB0_p ~ dbeta(1,1)
  STL_p <- 1 # assume perfect detection
  SALEFT_p <- 1 # assume perfect detection
  PAHH_p <- 1 # assume perfect detection

  BRC_p <- 1 # assume perfect detection


  ##################################################################
  # Set up initial branching structure after leaving Lower Granite
  ##################################################################
  # 27 bins: 	Tucannon (1), Penawawa (2), Almota (3), Alpowa (4), Asotin (5), Ten Mile Cr. (6), Lapwai (7), Potlatch (8), Joseph Creek (9), Cow Creek (10), Imnaha (11), Lolo (12), SF Clearwater (13), Clear Creek (14), Lochsa (15), Selway (16), Lookingglass Creek (17), Wallowa (18),  Grande Ronde (19), Rapid River (20), South Fork Salmon (21), Big Creek (22), North Fork Salmon (23), Carmen Creek (24), Lemhi (25), Upper Salmon (26), Not Seen (27)

  # set probability to any main bin that saw NO fish to 0
  p_pop_main ~ ddirch(main_dirch_vec) # Dirichlet for probs for going to n.pops bins

  for(i in 1:n.fish) {
    a[i] ~ dcat( p_pop_main )
  }

  # expand the dcat variable into a matrix of zeros and ones
  for (i in 1:n.fish) {
    for (j in 1:n.pops.main) {
      catexp[i,j] <- equals(a[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }
  }

  ####################################################
  #   Now we deal with Tucanon
  ####################################################
  # We use catexp[i,1] as an on/off switch for presence/absence of fish in Tucanon
  # We model the prob of fish moving to consecutive upstream bins in the Tucanon as follows...
  # P(being in bin) = P(migrating from next lowest bin) * present in lower bin (1/0)

  # priors for transition probabilities
  phi_mtr ~ dbeta(1,1)  # probability of moving past MTR
  phi_utr ~ dbeta(1,1)  # probability of moving past UTR
  phi_tuch ~ dbeta(1,1)  # probability of moving past TUCH

  for ( i in 1:n.fish ) {
    #----------------------------
    # TRUE STATE part in Tucannon
    #----------------------------
    # did the fish make it past MTR?
    z_mtr[i] ~ dbern(catexp[i,1] * phi_mtr)

    # did the fish make it past UTR?
    z_utr[i] ~ dbern(z_mtr[i] * phi_utr)

    # did the fish make it past TUCH & TFH?
    z_tuch[i] ~ dbern(z_utr[i] * phi_tuch)

    #----------------------------
    # OBSERVATION part in Tucanon
    #----------------------------

    Tucannon[i,1] ~ dbern(LTR_p * catexp[i,1]) # did they go past LTR?

    Tucannon[i,2] ~ dbern(MTR_p * z_mtr[i]) # did they go past MTR?

    Tucannon[i,3] ~ dbern(UTR_p * z_utr[i]) # did they go past UTR?

    Tucannon[i,4] ~ dbern(TUCH_p * z_tuch[i]) # did they go past TUCH & TFH?

  }  # ends the ifish loop started at the top of this section

  ####################################################
  #   Now we deal with Penawawa, Almota, Alpowa
  ####################################################
  # only have to worry about observation piece
  for ( i in 1:n.fish ) {

    Penawawa[i,1] ~ dbern(PENAWC_p * catexp[i,2])
    Almota[i,1] ~ dbern(ALMOTC_p * catexp[i,3])
    Alpowa[i,1] ~ dbern(ALPOWC_p * catexp[i,4])

  }

  ####################################################
  #   Now we deal with Asotin
  ####################################################
  # Initial branch
  # 4 bins: mainstem (1), GEORGC (2), past Adult trap, ASOTIC (3), and not seen (4)

  phi_acb ~ dbeta(1,1) # prob of migrating past ACB

  p_pop_Asotin[1:n.pops.Asotin[1]] ~ ddirch(aso_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_Asotin[1,1:n.pops.Asotin[1]] <- zero_vec[1:(n.pops.Asotin[1])] # when not in trib, 0 prob of being in sub areas
  pMat_Asotin[1,(n.pops.Asotin[1]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_Asotin[2,1:n.pops.Asotin[1]] <- p_pop_Asotin # when in trib, >0 probs of being in sub areas
  pMat_Asotin[2,(n.pops.Asotin[1]+1)] <- 0 #set the "not there" bin to prob = 0

  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in lower Asotin
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_Aso[i] ~ dcat( pMat_Asotin[(catexp[i,5]+1),1:(n.pops.Asotin[1]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Asotin[1]+1))	{ # now expand the dcat into matrix of zeros and ones
      catexp_Aso[i,j] <- equals(a_Aso[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

    #------------------------------------
    # OBSERVATION part in lower Asotin
    #------------------------------------
    #first do main stem (if it is seen anywhere in mainstem OR tribs in Asotin -- thus the max statement)
    # first array (ACM)
    Asotin[i,2] ~ dbern(ACMB0_p * max(catexp_Aso[i,1:(n.pops.Asotin[1])]))
    Asotin[i,3] ~ dbern(ACMA0_p * max(catexp_Aso[i,1:(n.pops.Asotin[1])]))

    # George Creek (GEORGC)
    Asotin[i,6] ~ dbern(GEORGC_p * catexp_Aso[i,2])

    # Past adult trap (ASOTIC)
    Asotin[i,1] ~ dbern(ASOTIC_p * catexp_Aso[i,3])

    # Past ACB
    z_acb[i] ~ dbern(catexp_Aso[i,3] * phi_acb)
    Asotin[i,4] ~ dbern(ACBB0_p * z_acb[i])
    Asotin[i,5] ~ dbern(ACBA0_p * z_acb[i])


  }

  # Upper branch
  # 4 bins: mainstem (1), CCA (2), AFC (3), and not seen (4)

  p_pop_AsoUpp[1:n.pops.Asotin[2]] ~ ddirch(asoUp_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_AsoUpp[1,1:n.pops.Asotin[2]] <- zero_vec[1:(n.pops.Asotin[2])] # when not in trib, 0 prob of being in sub areas
  pMat_AsoUpp[1,(n.pops.Asotin[2]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_AsoUpp[2,1:n.pops.Asotin[2]] <- p_pop_AsoUpp # when in trib, >0 probs of being in sub areas
  pMat_AsoUpp[2,(n.pops.Asotin[2]+1)] <- 0 #set the "not there" bin to prob = 0

  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in upper Asotin
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_AsoUp[i] ~ dcat( pMat_AsoUpp[(z_acb[i]+1),1:(n.pops.Asotin[2]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Asotin[2]+1))  { # now expand the dcat into matrix of zeros and ones
      catexp_AsoUp[i,j] <- equals(a_AsoUp[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

    #------------------------------------
    # OBSERVATION part in upper Asotin
    #------------------------------------
    #first do main stem (if it is seen anywhere in mainstem OR tribs in Asotin -- thus the max statement)
    # CCA
    Asotin[i,7] ~ dbern(CCAB0_p * catexp_AsoUp[i,2])
    Asotin[i,8] ~ dbern(CCAA0_p * catexp_AsoUp[i,2])

    # AFC
    Asotin[i,9] ~ dbern(AFCB0_p * catexp_AsoUp[i,3])
    Asotin[i,10] ~ dbern(AFCA0_p * catexp_AsoUp[i,3])

  } # ends the ifish loop started at the top of this section

  ####################################################
  #   Now we deal with Ten Mile Creek
  ####################################################
  # only have to worry about observation piece
  for ( i in 1:n.fish ) {

    TenMileCreek[i,1] ~ dbern(TENMC2_p * catexp[i,6])

  }

  ####################################################
  #   Now we deal with Lapwai
  ####################################################
  # 4 bins: mainstem (1), MIS (2), SWT (3), and not seen (4)

  p_pop_Lapwai[1:n.pops.Lapwai] ~ ddirch(lap_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_Lapwai[1,1:n.pops.Lapwai] <- zero_vec[1:(n.pops.Lapwai)] # when not in trib, 0 prob of being in sub areas
  pMat_Lapwai[1,(n.pops.Lapwai+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_Lapwai[2,1:n.pops.Lapwai] <- p_pop_Lapwai # when in trib, >0 probs of being in sub areas
  pMat_Lapwai[2,(n.pops.Lapwai+1)] <- 0 #set the "not there" bin to prob = 0

  phi_web ~ dbeta(1,1)  # probability of moving past WEB

  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in Lapwai
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_Lap[i] ~ dcat( pMat_Lapwai[(catexp[i,7]+1),1:(n.pops.Lapwai+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Lapwai+1))	{ # now expand the dcat into matrix of zeros and ones
    catexp_Lap[i,j] <- equals(a_Lap[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

    z_web[i] ~ dbern(catexp_Lap[i,2] * phi_web)
    #------------------------------------
    # OBSERVATION part in Lapwai
    #------------------------------------
    #first do main stem (if it is seen anywhere in mainstem OR tribs in Lapwai -- thus the max statement)
    #first array (LAP)
    Lapwai[i,1] ~ dbern(LAPB0_p * max(catexp_Lap[i,1:(n.pops.Lapwai)]))
    Lapwai[i,2] ~ dbern(LAPA0_p * max(catexp_Lap[i,1:(n.pops.Lapwai)]))

    # Sweetwater Creek array (SWT)...
    Lapwai[i,3] ~ dbern(SWTB0_p * catexp_Lap[i, 2])
    Lapwai[i,4] ~ dbern(SWTA0_p * catexp_Lap[i, 2])

    Lapwai[i,5] ~ dbern(WEBB0_p * z_web[i])
    Lapwai[i,6] ~ dbern(WEBA0_p * z_web[i])

    # Mission Creek array (MIS)...
    Lapwai[i,7] ~ dbern(MISB0_p * catexp_Lap[i, 3])
    Lapwai[i,8] ~ dbern(MISA0_p * catexp_Lap[i, 3])


  } # ends the ifish loop started at the top of this section

  ####################################################
  #   Now we deal with Potlatch
  ####################################################
  # 5 bins: mainstem (1), KHS (2), PCM (3), HLM (4), and not there (5)

  p_pop_Potlatch[1:n.pops.Potlatch[1]] ~ ddirch(pot_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_Potlatch[1,1:n.pops.Potlatch[1]] <- zero_vec[1:(n.pops.Potlatch[1])] # when not in trib, 0 prob of being in sub areas
  pMat_Potlatch[1,(n.pops.Potlatch[1]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_Potlatch[2,1:n.pops.Potlatch[1]] <- p_pop_Potlatch # when in trib, >0 probs of being in sub areas
  pMat_Potlatch[2,(n.pops.Potlatch[1]+1)] <- 0 #set the "not there" bin to prob = 0

  # 3 bins above KHS: mainstem (1), BIGBEC (2) and LBEARC (3)

  p_pop_KHS[1:n.pops.Potlatch[2]] ~ ddirch(khs_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_KHS[1,1:n.pops.Potlatch[2]] <- zero_vec[1:(n.pops.Potlatch[2])] # when not in trib, 0 prob of being in sub areas
  pMat_KHS[1,(n.pops.Potlatch[2]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_KHS[2,1:n.pops.Potlatch[2]] <- p_pop_KHS # when in trib, >0 probs of being in sub areas
  pMat_KHS[2,(n.pops.Potlatch[2]+1)] <- 0 #set the "not there" bin to prob = 0

  # 3 bins above HLM: mainstem (1), POTREF (2) and POTRWF (3)

  p_pop_HLM[1:n.pops.Potlatch[3]] ~ ddirch(hlm_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_HLM[1,1:n.pops.Potlatch[3]] <- zero_vec[1:(n.pops.Potlatch[3])] # when not in trib, 0 prob of being in sub areas
  pMat_HLM[1,(n.pops.Potlatch[3] + 1)] <- 1 #set the "not there" bin to prob = 1
  pMat_HLM[2,1:n.pops.Potlatch[3]] <- p_pop_HLM # when in trib, >0 probs of being in sub areas
  pMat_HLM[2,(n.pops.Potlatch[3] + 1)] <- 0 #set the "not there" bin to prob = 0

  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in Potlatch
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_Pot[i] ~ dcat( pMat_Potlatch[(catexp[i,8]+1),1:(n.pops.Potlatch[1]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Potlatch[1]+1))	{ # now expand the dcat into matrix of zeros and ones
      catexp_Pot[i,j] <- equals(a_Pot[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

    # for fish that went past KHS, what branch did they take next?
    a_KHS[i] ~ dcat( pMat_KHS[(catexp_Pot[i,2]+1),1:(n.pops.Potlatch[2]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Potlatch[2]+1))  { # now expand the dcat into matrix of zeros and ones
      catexp_KHS[i,j] <- equals(a_KHS[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

    # for fish that went past HLM, what branch did they take next?
    a_HLM[i] ~ dcat( pMat_HLM[(catexp_Pot[i,3]+1),1:(n.pops.Potlatch[3]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Potlatch[3]+1))  { # now expand the dcat into matrix of zeros and ones
      catexp_HLM[i,j] <- equals(a_HLM[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

  #------------------------------------
    # OBSERVATION part in Potlatch
    #------------------------------------
    #first do main stem (if it is seen anywhere in mainstem OR tribs in Potlatch -- thus the max statement)
    #first array (JUL)
    Potlatch[i,1] ~ dbern(JUL_p * max(catexp_Pot[i,1:(n.pops.Potlatch[1])]))

    # Kendrick High School array (KHS)
    Potlatch[i,3] ~ dbern(KHSB0_p * catexp_Pot[i, 2])
    Potlatch[i,4] ~ dbern(KHSA0_p * catexp_Pot[i, 2])

    # above KHS
    Potlatch[i,2] ~ dbern(BIGBEC_p * catexp_KHS[i, 2])
    Potlatch[i,5] ~ dbern(LBEARC_p * catexp_KHS[i, 3])

    # PCM
    Potlatch[i,6] ~ dbern(PCMB0_p * catexp_Pot[i, 3])
    Potlatch[i,7] ~ dbern(PCMA0_p * catexp_Pot[i, 3])

    # Helmer array (HLM)
    Potlatch[i,9] ~ dbern(HLMB0_p * catexp_Pot[i, 4])
    Potlatch[i,10] ~ dbern(HLMA0_p * catexp_Pot[i, 4])

    # above HLM
    Potlatch[i,8] ~ dbern(POTREF_p * catexp_HLM[i, 2])
    Potlatch[i,11] ~ dbern(POTRWF_p * catexp_HLM[i, 3])

  } #ends the ifish loop started at the top of this section

  ####################################################
  #   Now we deal with Joseph Creek
  ####################################################
  # getting past JOSEPC
  phi_josepc ~ dbeta(1,1)

  for (i in 1:n.fish) {
  #------------------------------------
  # TRUE STATE part in Joseph Creek
  #------------------------------------
  # did the fish make it to JOSEPC?
  z_josepc[i] ~ dbern(catexp[i,9] * phi_josepc)

  #------------------------------------
  # OBSERVATION part in Joseph Creek
  #------------------------------------
  # first array (JOC)
  JosephCreek[i,2] ~ dbern( JOCB0_p * catexp[i,9] )
  JosephCreek[i,3] ~ dbern( JOCA0_p * catexp[i,9] )

  # JOSEPC
  JosephCreek[i,1] ~ dbern( JOSEPC_p * z_josepc[i] )

  }

  ####################################################
  #   Now we deal with Cow Creek
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

  # first array (COC)
  CowCreek[i,1] ~ dbern( COCB0_p * catexp[i,10] )
  CowCreek[i,2] ~ dbern( COCA0_p * catexp[i,10] )

  }

  ####################################################
  #   Now we deal with Imnaha
  ####################################################
  # 7 bins: mainstem (1), HORS3C (2), CMP (3), LSHEEF (4), BSC (5), IR3 (6), and not seen (7)

  p_pop_Imnaha[1:n.pops.Imnaha[1]] ~ ddirch(imn_dirch_vec)

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_Imnaha[1,1:n.pops.Imnaha[1]] <- zero_vec[1:(n.pops.Imnaha[1])] # when not in trib, 0 prob of being in sub areas
  pMat_Imnaha[1,(n.pops.Imnaha[1]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_Imnaha[2,1:n.pops.Imnaha[1]] <- p_pop_Imnaha # when in trib, >0 probs of being in sub areas
  pMat_Imnaha[2,(n.pops.Imnaha[1]+1)] <- 0 #set the "not there" bin to prob = 0

  # upper Imnaha (above IR3)
  # 7 bins: mainstem (1), FREEZC (2), CZY (3), MAHOGC (4), IR4 (5), and not seen (6)
  p_pop_UppImn[1:n.pops.Imnaha[2]] ~ ddirch(imnUp_dirch_vec)

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_UppImn[1,1:n.pops.Imnaha[2]] <- zero_vec[1:(n.pops.Imnaha[2])] # when not in trib, 0 prob of being in sub areas
  pMat_UppImn[1,(n.pops.Imnaha[2]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_UppImn[2,1:n.pops.Imnaha[2]] <- p_pop_UppImn # when in trib, >0 probs of being in sub areas
  pMat_UppImn[2,(n.pops.Imnaha[2]+1)] <- 0 #set the "not there" bin to prob = 0

  # above IR5 (above IR5)
  # 4 bins: mainstem (1), GUMBTC (2), DRY2C (3), and not seen (4)
  p_pop_ImnWeir[1:n.pops.Imnaha[3]] ~ ddirch(imnWeir_dirch_vec)

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_ImnWeir[1,1:n.pops.Imnaha[3]] <- zero_vec[1:(n.pops.Imnaha[3])] # when not in trib, 0 prob of being in sub areas
  pMat_ImnWeir[1,(n.pops.Imnaha[3]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_ImnWeir[2,1:n.pops.Imnaha[3]] <- p_pop_ImnWeir # when in trib, >0 probs of being in sub areas
  pMat_ImnWeir[2,(n.pops.Imnaha[3]+1)] <- 0 #set the "not there" bin to prob = 0

  # probability of moving past certain points
  phi_iml ~ dbeta(1,1)
  phi_ir5 ~ dbeta(1,1)

  for (i in 1:n.fish) {
  #------------------------------------
  # TRUE STATE part in Imnaha
  #------------------------------------
  # the row number acts as switch between rows 1&2 using stochastic node
  a_Imn[i] ~ dcat( pMat_Imnaha[(catexp[i,11]+1),1:(n.pops.Imnaha[1]+1)] ) # the row number acts as on/off switch
  for (j in 1:(n.pops.Imnaha[1]+1))	{ # now expand the dcat into matrix of zeros and ones
    catexp_Imn[i,j] <- equals(a_Imn[i],j) #equals(x,y) is a test for equality, returns [1,0]
  }

  # above IR3
  a_ImnUp[i] ~ dcat( pMat_UppImn[(catexp_Imn[i,6]+1),1:(n.pops.Imnaha[2]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Imnaha[2]+1))  { # now expand the dcat into matrix of zeros and ones
      catexp_ImnUp[i,j] <- equals(a_ImnUp[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

  # past IML (IR4 -> IML)
  z_iml[i] ~ dbern(catexp_ImnUp[i,5] * phi_iml)

  # past IR5 (IML -> IR5)
  z_ir5[i] ~ dbern(z_iml[i] * phi_ir5)

  # above IR5
  a_ImnWeir[i] ~ dcat( pMat_ImnWeir[(z_ir5[i]+1),1:(n.pops.Imnaha[3]+1)] ) # the row number acts as on/off switch
  for (j in 1:(n.pops.Imnaha[3]+1))  { # now expand the dcat into matrix of zeros and ones
    catexp_ImnWeir[i,j] <- equals(a_ImnWeir[i],j) #equals(x,y) is a test for equality, returns [1,0]
  }

  #------------------------------------
  # OBSERVATION part in Imnaha
  #------------------------------------
  # first do main stem (if it is seen anywhere in mainstem OR tribs in Imnaha -- thus the max statement)
  # first array (IR1)
  ImnahaRiver[i,1] ~ dbern(IR1_p * max(catexp_Imn[i,1:(n.pops.Imnaha[1])]))
  # second array (IR2)
  ImnahaRiver[i,2] ~ dbern(IR2_p * max(catexp_Imn[i,1:(n.pops.Imnaha[1])]))

  # HORS3C
  ImnahaRiver[i,3] ~ dbern(HORS3C_p * catexp_Imn[i, 2])

  # Camp Creek (CMP)
  ImnahaRiver[i,4] ~ dbern(CMPB0_p * catexp_Imn[i, 3])
  ImnahaRiver[i,5] ~ dbern(CMPA0_p * catexp_Imn[i, 3])

  # LSHEEF
  ImnahaRiver[i,6] ~ dbern(LSHEEF_p * catexp_Imn[i, 4])

  # Big Sheep Creek (BSC)
  ImnahaRiver[i,7] ~ dbern(BSCB0_p * catexp_Imn[i, 5])
  ImnahaRiver[i,8] ~ dbern(BSCA0_p * catexp_Imn[i, 5])

  # Upper Imnaha (IR3)
  ImnahaRiver[i,9] ~ dbern(IR3B0_p * catexp_Imn[i, 6])
  ImnahaRiver[i,10] ~ dbern(IR3A0_p * catexp_Imn[i, 6])

  # Freezeout Creek weir
  ImnahaRiver[i,11] ~ dbern(FREEZC_p * catexp_ImnUp[i, 2])

  # Crazy Creek (CZY)
  ImnahaRiver[i,12] ~ dbern(CZYB0_p * catexp_ImnUp[i, 3])
  ImnahaRiver[i,13] ~ dbern(CZYA0_p * catexp_ImnUp[i, 3])

  # Mahogany Creek weir
  ImnahaRiver[i,14] ~ dbern(MAHOGC_p * catexp_ImnUp[i, 4])

  # IR4
  ImnahaRiver[i,15] ~ dbern(IR4B0_p * catexp_ImnUp[i, 5])
  ImnahaRiver[i,16] ~ dbern(IR4A0_p * catexp_ImnUp[i, 5])

  # Imnaha weir
  ImnahaRiver[i,17] ~ dbern(IMLB0_p * z_iml[i])
  ImnahaRiver[i,18] ~ dbern(IMLA0_p * z_iml[i])
  ImnahaRiver[i,19] ~ dbern(IMNAHW_p * z_iml[i])

  # IR5
  ImnahaRiver[i,20] ~ dbern(IR5B0_p * z_ir5[i])
  ImnahaRiver[i,21] ~ dbern(IR5A0_p * z_ir5[i])

  # Gumboot Creek weir
  ImnahaRiver[i,22] ~ dbern(GUMBTC_p * catexp_ImnWeir[i, 2])

  # Dry Creek
  ImnahaRiver[i,23] ~ dbern(DRY2C_p * catexp_ImnWeir[i, 3])


  } #ends the ifish loop started at the top of this section

  ####################################################
  #   Now we deal with Lolo Creek
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (LC1)
    Lolo[i,1] ~ dbern( LC1_p * catexp[i,12] )

    # second array (LC2)
    Lolo[i,2] ~ dbern( LC2_p * catexp[i,12] )

  }

  ####################################################
  #   Now we deal with SF Clearwater
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (SC1)
    SFClearwater[i,1] ~ dbern( SC1_p * catexp[i,13] )

    # second array (SC2)
    SFClearwater[i,2] ~ dbern( SC2B0_p * catexp[i,13] )

    # other arrays (above SC2)
    SFClearwater[i,3] ~ dbern( SC2A0_p * catexp[i,13] )

  }

  ####################################################
  #   Now we deal with Wenaha River
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first site (WEN)
    Wenaha[i,1] ~ dbern( WENB0_p * catexp[i,14] )
    Wenaha[i,2] ~ dbern( WENA0_p * catexp[i,14] )

  }

  ####################################################
  #   Now we deal with Clear Creek
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (CLC)
    ClearCreek[i,1] ~ dbern( CLC_p * catexp[i,15] )

    # other observation spot (KOOS)
    ClearCreek[i,2] ~ dbern( KOOS_p * catexp[i,15] )

  }

  ####################################################
  #   Now we deal with Lochsa
  ####################################################
  # probability of moving past FISTRP
  phi_fistrp ~ dbeta(1,1)

  for (i in 1:n.fish) {

    # first array (LRL)
    Lochsa[i,1] ~ dbern( LRL_p * catexp[i,16] )
    Lochsa[i,2] ~ dbern( LRU_p * catexp[i,16] )

    z_fistrp[i] ~ dbern(phi_fistrp * catexp[i,16] )

    # other observation spot (FISTRP)
    Lochsa[i,3] ~ dbern( FISTRP_p * z_fistrp[i])

  }


  ####################################################
  #   Now we deal with Selway
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (SW1)
    Selway[i,1] ~ dbern( SW1_p * catexp[i,17] )
    Selway[i,2] ~ dbern( SW2_p * catexp[i,17] )

  }


  ####################################################
  #   Now we deal with Lookingglass Creek
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (LOOKGC)
    LookingGlass[i,1] ~ dbern( LOOKGC_p * catexp[i,18] )

  }

  ####################################################
  #   Now we deal with Wallowa
  ####################################################
  # 5 bins: mainstem (1), BCANF (2), WR2 (3) and not seen (4)

  p_pop_Wallowa[1:n.pops.Wallowa[1]] ~ ddirch(wal_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_Wallow[1,1:n.pops.Wallowa[1]] <- zero_vec[1:(n.pops.Wallowa[1])] # when not in trib, 0 prob of being in sub areas
  pMat_Wallow[1,(n.pops.Wallowa[1]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_Wallow[2,1:n.pops.Wallowa[1]] <- p_pop_Wallowa # when in trib, >0 probs of being in sub areas
  pMat_Wallow[2,(n.pops.Wallowa[1]+1)] <- 0 #set the "not there" bin to prob = 0


  # Upper Wallowa
  p_pop_UppWall[1:n.pops.Wallowa[2]] ~ ddirch(walUp_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_WalUp[1,1:n.pops.Wallowa[2]] <- zero_vec[1:(n.pops.Wallowa[2])] # when not in trib, 0 prob of being in sub areas
  pMat_WalUp[1,(n.pops.Wallowa[2]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_WalUp[2,1:n.pops.Wallowa[2]] <- p_pop_UppWall # when in trib, >0 probs of being in sub areas
  pMat_WalUp[2,(n.pops.Wallowa[2]+1)] <- 0 #set the "not there" bin to prob = 0

  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in Wallowa
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_Wal[i] ~ dcat( pMat_Wallow[(catexp[i,19]+1),1:(n.pops.Wallowa[1]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Wallowa[1]+1))	{ # now expand the dcat into matrix of zeros and ones
      catexp_Wal[i,j] <- equals(a_Wal[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

     # above WR2
    a_WalUp[i] ~ dcat( pMat_WalUp[(catexp_Wal[i,3]+1),1:(n.pops.Wallowa[2]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Wallowa[2]+1))  { # now expand the dcat into matrix of zeros and ones
      catexp_WalUp[i,j] <- equals(a_WalUp[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

    #------------------------------------
    # OBSERVATION part in Wallowa
    #------------------------------------
    #first do main stem (if it is seen anywhere in mainstem OR tribs in Wallowa -- thus the max statement)
    #first array (WR1)
    Wallowa[i,1] ~ dbern(WR1_p * max(catexp_Wal[i,1:(n.pops.Wallowa[1])]))

    # BCANF
    Wallowa[i,2] ~ dbern(BCANF_p * catexp_Wal[i, 2])

    # WR2
    Wallowa[i,3] ~ dbern(WR2B0_p * catexp_Wal[i,3])
    Wallowa[i,4] ~ dbern(WR2A0_p * catexp_Wal[i,3])

    # LOSTIW
    Wallowa[i,5] ~ dbern(LOSTIW_p * catexp_WalUp[i, 2])

    # WALH
    Wallowa[i,6] ~ dbern(WALH_p * catexp_WalUp[i, 3])

}

  ####################################################
  #   Now we deal with Grande Ronde
  ####################################################
  # 4 bins: mainstem (1), Catherine Creek (2), GRANDW (3), and not seen (4)

  p_pop_UppGR[1:n.pops.UppGR] ~ ddirch(ugr_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_UppGR[1,1:n.pops.UppGR] <- zero_vec[1:(n.pops.UppGR)] # when not in trib, 0 prob of being in sub areas
  pMat_UppGR[1,(n.pops.UppGR+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_UppGR[2,1:n.pops.UppGR] <- p_pop_UppGR # when in trib, >0 probs of being in sub areas
  pMat_UppGR[2,(n.pops.UppGR+1)] <- 0 #set the "not there" bin to prob = 0

  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in Upper Grande Ronde
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_UGR[i] ~ dcat( pMat_UppGR[(catexp[i,20]+1),1:(n.pops.UppGR+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.UppGR+1))  { # now expand the dcat into matrix of zeros and ones
      catexp_UGR[i,j] <- equals(a_UGR[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

    #------------------------------------
    # OBSERVATION part in Upper Grande Ronde
    #------------------------------------
    #first do main stem (if it is seen anywhere in mainstem OR tribs in Wallowa -- thus the max statement)
    #first array (UGR)
    GrandeRonde[i,1] ~ dbern(UGR_p * max(catexp_UGR[i,1:(n.pops.UppGR)]))

    # Catherine Creek
    GrandeRonde[i,2] ~ dbern(CCWB0_p * catexp_UGR[i, 2])
    GrandeRonde[i,3] ~ dbern(CCWA0_p * catexp_UGR[i, 2])
    GrandeRonde[i,4] ~ dbern(CATHEW_p * catexp_UGR[i, 2])

    # GRANDW
    GrandeRonde[i,5] ~ dbern(GRANDW_p * catexp_UGR[i, 3])
    GrandeRonde[i,6] ~ dbern(UGSB0_p * catexp_URG[i,3])
    GrandeRonde[i,7] ~ dbern(UGSA0_p * catexp_URG[i,3])

  }

  ####################################################
  #   Now we deal with Rapid River
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (RAPH)
    RapidRiver[i,1] ~ dbern( RAPH_p * catexp[i,21] )

  }

  ####################################################
  #   Now we deal with South Fork Salmon
  ####################################################
  # 5 bins: mainstem (1), ZEN (2), ESS (3), KRS (4), and not seen (5)

  p_pop_SFS[1:n.pops.SFS] ~ ddirch(sfs_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_SFS[1,1:n.pops.SFS] <- zero_vec[1:(n.pops.SFS)] # when not in trib, 0 prob of being in sub areas
  pMat_SFS[1,(n.pops.SFS+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_SFS[2,1:n.pops.SFS] <- p_pop_SFS # when in trib, >0 probs of being in sub areas
  pMat_SFS[2,(n.pops.SFS+1)] <- 0 #set the "not there" bin to prob = 0

  # upstream migration parameters
  phi_lakec ~ dbeta(1,1)
  phi_johnsc ~ dbeta(1,1)
  phi_str ~ dbeta(1,1)

  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in South Fork Salmon
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_SFS[i] ~ dcat( pMat_SFS[(catexp[i,22]+1),1:(n.pops.SFS+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.SFS+1))	{ # now expand the dcat into matrix of zeros and ones
      catexp_SFS[i,j] <- equals(a_SFS[i],j) #equals(x,y) is a test for equality, returns [1,0]
    }

    # on/off for whether fish moved to end of each branch
    z_lakec[i] ~ dbern(catexp_SFS[i,2] * phi_lakec)
    z_johnsc[i] ~ dbern(catexp_SFS[i,3] * phi_johnsc)
    z_str[i] ~ dbern(catexp_SFS[i,4] * phi_str)

    #------------------------------------
    # OBSERVATION part in South Fork Salmon
    #------------------------------------
    #first do main stem (if it is seen anywhere in mainstem OR tribs in SFS -- thus the max statement)
    # first array (SFG)
    SFSalmon[i,1] ~ dbern(SFG_p * max(catexp_SFS[i,1:(n.pops.SFS)]))

    #  Zena Creek array (ZEN)
    SFSalmon[i,2] ~ dbern(ZENB0_p * catexp_SFS[i, 2])
    SFSalmon[i,3] ~ dbern(ZENA0_p * catexp_SFS[i, 2])
    SFSalmon[i,4] ~ dbern(LAKEC_p * z_lakec[i])

    # East Fork South Fork array (ESS)
    SFSalmon[i,5] ~ dbern(ESSB0_p * catexp_SFS[i, 3])
    SFSalmon[i,6] ~ dbern(ESSA0_p * catexp_SFS[i, 3])
    # upstream of ESS, at Johnson Creek weir
    SFSalmon[i,7] ~ dbern( JOHNSC_p * z_johnsc[i])

    # Krassel Creek array (KRS)
    SFSalmon[i,8] ~ dbern( KRS_p * catexp_SFS[i, 4] )
    # upstream of KRS, at McCall hatchery (STR)
    SFSalmon[i,9] ~ dbern( STR_p * z_str[i] )

  } #ends the ifish loop started at the top of this section

  ####################################################
  #   Now we deal with Panther Creek
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first site (PCA)
    Panther[i,1] ~ dbern( PCAB0_p * catexp[i,23] )
    Panther[i,2] ~ dbern( PCAA0_p * catexp[i,23] )

  }

  ####################################################
  #   Now we deal with Big Creek
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (TAY)
    BigCreek[i,1] ~ dbern( TAYB0_p * catexp[i,24] )
    BigCreek[i,2] ~ dbern( TAYA0_p * catexp[i,24] )

  }

  ####################################################
  #   Now we deal with North Fork Salmon
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (NFS)
    NFSalmon[i,1] ~ dbern( NFSB0_p * catexp[i,25] )
    NFSalmon[i,2] ~ dbern( NFSA0_p * catexp[i,25] )

  }

  ####################################################
  #   Now we deal with Carmen Creek
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first array (CRC)
    CarmenCreek[i,1] ~ dbern( CRCB0_p * catexp[i,26] )
    CarmenCreek[i,2] ~ dbern( CRCA0_p * catexp[i,26] )

  }


  ####################################################
  #   Now we deal with Lemhi
  ####################################################
  #------------------------------------
  # First deal with Lower Lemhi
  #------------------------------------
  # 8 bins: mainstem (1), Bohannon (2), Wimpy (3), Kenney (4), Agency (5), Hayden (6), Upper Lemhi (7), and not seen (8)

  p_pop_LowLemhi[1:n.pops.Lemhi[1]] ~ ddirch(lemlow_dirch_vec)

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_LowLemhi[1,1:n.pops.Lemhi[1]] <- zero_vec[1:(n.pops.Lemhi[1])] # when not in trib, 0 prob of being in sub areas
  pMat_LowLemhi[1,(n.pops.Lemhi[1]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_LowLemhi[2,1:n.pops.Lemhi[1]] <- p_pop_LowLemhi # when in trib, >0 probs of being in sub areas
  pMat_LowLemhi[2,(n.pops.Lemhi[1]+1)] <- 0 #set the "not there" bin to prob = 0

  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in Lower Lemhi
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_LowLem[i] ~ dcat( pMat_LowLemhi[(catexp[i,27]+1),1:(n.pops.Lemhi[1]+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.Lemhi[1]+1))	{ # now expand the dcat into matrix of zeros and ones
      catexp_LowLem[i,j] <- equals(a_LowLem[i],j)
    }

    #------------------------------------
    # OBSERVATION part in Lower Lemhi
    #------------------------------------
    #first do main stem
    #first array (LLR)
    Lemhi[i,1] ~ dbern(LLRB0_p * max(catexp_LowLem[i,1:(n.pops.Lemhi[1])]))
    Lemhi[i,2] ~ dbern(LLRA0_p * max(catexp_LowLem[i,1:(n.pops.Lemhi[1])]))

    # Bohannon array (BHC)...
    Lemhi[i,3] ~ dbern(BHCB0_p * catexp_LowLem[i, 2])
    Lemhi[i,4] ~ dbern(BHCA0_p * catexp_LowLem[i, 2])

    # Wimpy array (WPC)...
    Lemhi[i,5] ~ dbern(WPCB0_p * catexp_LowLem[i, 3])
    Lemhi[i,6] ~ dbern(WPCA0_p * catexp_LowLem[i, 3])

    # Kenney array (KEN)...
    Lemhi[i,7] ~ dbern(KENB0_p * catexp_LowLem[i, 4])
    Lemhi[i,8] ~ dbern(KENA0_p * catexp_LowLem[i, 4])

    # Agency array (AGC)...
    Lemhi[i,9] ~ dbern(AGCB0_p * catexp_LowLem[i, 5])
    Lemhi[i,10] ~ dbern(AGCA0_p * catexp_LowLem[i, 5])

    # Hayden array (HYC)...
    Lemhi[i,11] ~ dbern(HYCB0_p * catexp_LowLem[i, 6])
    Lemhi[i,12] ~ dbern(HYCA0_p * catexp_LowLem[i, 6])

    # Upper Lemhi (LRW)
    Lemhi[i,13] ~ dbern(LRWB0_p * catexp_LowLem[i, 7])
    Lemhi[i,14] ~ dbern(LRWA0_p * catexp_LowLem[i, 7])

  } # ends the ifish loop started at the top of this section


  #------------------------------------
  # Now deal with Upper Lemhi
  #------------------------------------

  # 9 bins: mainstem (1), Little Springs (2), Big Eightmile (3), Big Springs (4), Lee (5), Big Timber (6), Canyon (7), Eighteenmile (8), and not seen (9)

  p_pop_UpLemhi[1:n.pops.Lemhi[2]] ~ ddirch(lemupp_dirch_vec)

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_UpLemhi[1,1:n.pops.Lemhi[2]] <- zero_vec[1:(n.pops.Lemhi[2])] # when not in trib, 0 prob of being in sub areas
  pMat_UpLemhi[1,(n.pops.Lemhi[2]+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_UpLemhi[2,1:n.pops.Lemhi[2]] <- p_pop_UpLemhi # when in trib, >0 probs of being in sub areas
  pMat_UpLemhi[2,(n.pops.Lemhi[2]+1)] <- 0 #set the "not there" bin to prob = 0

  # movement up Big Timber Creek
  phi_btm ~ dbeta(1,1)
  phi_btu ~ dbeta(1,1)


  for (i in 1:n.fish) {
    #------------------------------------
    # TRUE STATE part in Upper Lemhi
    #------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_UpLem[i] ~ dcat( pMat_UpLemhi[(catexp_LowLem[i,7]+1),1:(n.pops.Lemhi[2]+1)] ) # the row number acts as on/off switch

    for (j in 1:(n.pops.Lemhi[2]+1))	{ # now expand the dcat into matrix of zeros and ones
      catexp_UpLem[i,j] <- equals(a_UpLem[i],j)
    }

    # movement up Big Timber Creek
    z_btm[i] ~ dbern(catexp_UpLem[i, 6] * phi_btm)  # past BTM
    z_btu[i] ~ dbern(z_btm[i] * phi_btu)  # past BTU

    #------------------------------------
    # OBSERVATION part in Upper Lemhi
    #------------------------------------
    #first do main stem (if it is seen anywhere in mainstem OR tribs in Upper Lemhi -- thus the max statement)

    # Little Springs array (LLS)...
    Lemhi[i,15] ~ dbern(LLSB0_p * catexp_UpLem[i, 2])
    Lemhi[i,16] ~ dbern(LLSA0_p * catexp_UpLem[i, 2])

    # Big Eightmile (LB8)
    Lemhi[i,17] ~ dbern(LB8B0_p * catexp_UpLem[i, 3])
    Lemhi[i,18] ~ dbern(LB8A0_p * catexp_UpLem[i, 3])

    # Big Springs (LBS)
    Lemhi[i,19] ~ dbern(LBSB0_p * catexp_UpLem[i, 4])
    Lemhi[i,20] ~ dbern(LBSA0_p * catexp_UpLem[i, 4])

    # Lee Creek (LCL)
    Lemhi[i,21] ~ dbern(LCLB0_p * catexp_UpLem[i, 5])
    Lemhi[i,22] ~ dbern(LCLA0_p * catexp_UpLem[i, 5])

    # Big Timber array (BTC)...
    Lemhi[i,23] ~ dbern(BTLB0_p * catexp_UpLem[i, 6])
    Lemhi[i,24] ~ dbern(BTLA0_p * catexp_UpLem[i, 6])

    Lemhi[i,25] ~ dbern(BTMB0_p * z_btm[i])
    Lemhi[i,26] ~ dbern(BTMA0_p * z_btm[i])

    Lemhi[i,27] ~ dbern(BTUB0_p * z_btu[i])
    Lemhi[i,28] ~ dbern(BTUA0_p * z_btu[i])

    # Canyon array (CAC)...
    Lemhi[i,29] ~ dbern(CACB0_p * catexp_UpLem[i, 7])
    Lemhi[i,30] ~ dbern(CACA0_p * catexp_UpLem[i, 7])

    # Hawley (HEC)
    Lemhi[i,31] ~ dbern(HECB0_p * catexp_UpLem[i, 8])
    Lemhi[i,32] ~ dbern(HECA0_p * catexp_UpLem[i, 8])

  } # ends the ifish loop started at the top of this section


  ####################################################
  #   Now we deal with Upper Salmon
  ####################################################
  # Initial survival model to USI
  # migration parameter
  phi_usi ~ dbeta(1,1) # prob of migrating past USI

  #--------------------------------
  # Now a branching model past USI
  #--------------------------------
  # 7 bins: mainstem (1), Pahsimeroi (2), East Fork Salmon (3), Yankee Fork (4), Valley Creek (5), Sawtooth (6), and not seen (7)

  p_pop_UpSalm[1:n.pops.UpSalm] ~ ddirch(upsalm_dirch_vec) # Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMat_UpSalm[1,1:n.pops.UpSalm] <- zero_vec[1:(n.pops.UpSalm)] # when not in trib, 0 prob of being in sub areas
  pMat_UpSalm[1,(n.pops.UpSalm+1)] <- 1 #set the "not there" bin to prob = 1
  pMat_UpSalm[2,1:n.pops.UpSalm] <- p_pop_UpSalm # when in trib, >0 probs of being in sub areas
  pMat_UpSalm[2,(n.pops.UpSalm+1)] <- 0 #set the "not there" bin to prob = 0

  for (i in 1:n.fish) {
    #-----------------------------------------------------
    # TRUE STATE part in first section of Upper Salmon
    #-----------------------------------------------------
    # migration up the Upper Salmon
    z_usi[i] ~ dbern(catexp[i,28] * phi_usi ) # did fish make it past USI?

    #-----------------------------------------------------
    # TRUE STATE part in second section of Upper Salmon
    #-----------------------------------------------------
    # the row number acts as switch between rows 1&2 using stochastic node
    a_UpSalm[i] ~ dcat( pMat_UpSalm[(z_usi[i]+1),1:(n.pops.UpSalm+1)] ) # the row number acts as on/off switch
    for (j in 1:(n.pops.UpSalm+1))  { # now expand the dcat into matrix of zeros and ones
      catexp_UpSalm[i,j] <- equals(a_UpSalm[i],j)
    }

    #-----------------------------------------------------
    # OBSERVATION part in Upper Salmon
    #-----------------------------------------------------
    # first array (USE)
    UpperSalmon[i,1] ~ dbern( USE_p * catexp[i,28])
    # second array (USI)
    UpperSalmon[i,2] ~ dbern( USI_p * z_usi[i])

    # Pahsimeroi array (PAHH)...
    UpperSalmon[i,3] ~ dbern(PAHH_p * catexp_UpSalm[i, 2])

    # East Fork Salmon River trap array (SALEFT)...
    UpperSalmon[i,4] ~ dbern(SALEFT_p * catexp_UpSalm[i, 3])

    # Yankee Fork array (YKF)...
    UpperSalmon[i,5] ~ dbern(YFKB0_p * catexp_UpSalm[i, 4])
    UpperSalmon[i,6] ~ dbern(YFKA0_p * catexp_UpSalm[i, 4])

    # Valley Creek arrays (VC2 & VC1)...
    UpperSalmon[i,7] ~ dbern(VC2_p * catexp_UpSalm[i, 5])
    UpperSalmon[i,8] ~ dbern(VC1_p * catexp_UpSalm[i, 5])

    # Sawtooth trap (STL or SAWT)...
    UpperSalmon[i,9] ~ dbern(STL_p * catexp_UpSalm[i, 6])

  } # ends the ifish loop started at the top of this section

  ####################################################
  #   Now we deal with Bear Valley
  ####################################################
  # only have to worry about observation piece
  for (i in 1:n.fish) {

    # first site (BRC)
    BearValley[i,1] ~ dbern( BRC_p * catexp[i,29] )

  }

} # ends model file
'

  # write model as text file
  cat(model_code,
      file = file_name)

  model_file = readLines(file_name)

  if(time_varying) {

    model_file[seq(grep('p_pop_main ~ ', model_file), length.out = 5)] = ''

    model_file[grep('\\# set probability to any main bin that saw NO fish to 0', model_file)] =
      "  # set probability to any main bin that saw NO fish to 0

  # prior on log odds ratio for initial week
  for(j in 1:(n.pops.main - 1)) {
      # somewhat informative, but fairly vague prior
    phi[1,j] ~ dnorm(-2, 1/16)
    exp_phi[1,j] <- exp(phi[1,j]) * main_dirch_vec[j]
  }

  # set black box as baseline
  for(t in 1:(n.weeks)) {
    phi[t,n.pops.main] <- 0
    exp_phi[t, n.pops.main] <- exp(phi[t, n.pops.main]) * main_dirch_vec[n.pops.main]
    # get sum of all phi's
    sum_exp_phi[t] <- sum(exp_phi[t,]);
  }

  # extract initial movement probabilities for week 1
  for(j in 1:n.pops.main) {
    p_pop_main[1,j] <- ifelse(main_dirch_vec[j] == 0, 0, exp_phi[1,j] / sum_exp_phi[1])
  }

  # variation in time-varying random walk movement probabilities
  sigma_rw ~ dunif(0,10)
  tau_rw <- pow(sigma_rw, -2)

  for(t in 2:(n.weeks)) {
    for(j in 1:(n.pops.main - 1)) {
      epsilon[t,j] ~ dnorm(0, tau_rw)
      # set phi to any main bin that saw NO fish to 0
      phi[t,j] <- ifelse(main_dirch_vec[j] == 0, 0, phi[t - 1, j] + epsilon[t,j])
      exp_phi[t,j] <- exp(phi[t,j]) * main_dirch_vec[j]
    }

    for (j in 1:(n.pops.main)) {
      p_pop_main[t,j] <- (exp_phi[t,j] / sum_exp_phi[t])
    }
  }

  # Which main branch does each fish follow?
  for(i in 1:n.fish) {
    a[i] ~ dcat( p_pop_main[dam_week[i], 1:n.pops.main] )
  }"
  }
  writeLines(model_file, file_name)

}
