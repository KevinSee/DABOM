#' @title Write Priest Rapids JAGS model
#'
#' @description This writes the overall JAGS model for Priest Rapids DABOM as a text file. It can then be modified depending on the observations for a particular valid tag list.
#'
#' @author Kevin See
#'
#' @param file_name name (with file path) to save the model as
#'
#' @export
#' @return NULL
#' @examples writeJAGSmodel_PRA()

writeDABOM_PRA = function(file_name = NULL) {

  if(is.null(file_name)) file_name = 'PRA_DABOM.txt'

  model_code = "
model
  {

  # Set up array detection efficiency priors
  #-------------------------
  # Wenatchee sites
  #-------------------------
  RIA_p ~ dbeta(1,1)
  CLK_p <- 1 # assume perfect detection (i.e. worst case)
  LWEB0_p ~ dbeta(1,1)
  LWEA0_p ~ dbeta(1,1)
  MCLB0_p ~ dbeta(1,1)
  MCLA0_p ~ dbeta(1,1)
  PESB0_p ~ dbeta(1,1)
  PESA0_p ~ dbeta(1,1)
  PEUB0_p ~ dbeta(1,1)
  PEUA0_p ~ dbeta(1,1)
  CHMB0_p ~ dbeta(1,1)
  CHMA0_p ~ dbeta(1,1)
  ICLB0_p ~ dbeta(1,1)
  ICLA0_p ~ dbeta(1,1)
  LNFB0_p ~ dbeta(1,1)
  LNFA0_p ~ dbeta(1,1)
  ICMB0_p ~ dbeta(1,1)
  ICMA0_p ~ dbeta(1,1)
  ICUB0_p ~ dbeta(1,1)
  ICUA0_p ~ dbeta(1,1)
  TUM_p ~ dbeta(1,1)
  CHWB0_p ~ dbeta(1,1)
  CHWA0_p ~ dbeta(1,1)
  CHLB0_p ~ dbeta(1,1)
  CHLA0_p ~ dbeta(1,1)
  CHUB0_p ~ dbeta(1,1)
  CHUA0_p ~ dbeta(1,1)
  UWE_p ~ dbeta(1,1)
  NALB0_p ~ dbeta(1,1)
  NALA0_p ~ dbeta(1,1)
  NAUB0_p ~ dbeta(1,1)
  NAUA0_p ~ dbeta(1,1)
  WTLB0_p ~ dbeta(1,1)
  WTLA0_p ~ dbeta(1,1)
  LWNB0_p ~ dbeta(1,1)
  LWNA0_p ~ dbeta(1,1)

  #-------------------------
  # Entiat sites
  #-------------------------
  RRF_p ~ dbeta(1,1)
  ENLB0_p ~ dbeta(1,1)
  ENLA0_p ~ dbeta(1,1)
  RCTB0_p ~ dbeta(1,1)
  RCTA0_p ~ dbeta(1,1)
  EHLB0_p ~ dbeta(1,1)
  EHLA0_p ~ dbeta(1,1)
  ENAB0_p ~ dbeta(1,1)
  ENAA0_p ~ dbeta(1,1)
  MADB0_p ~ dbeta(1,1)
  MADA0_p ~ dbeta(1,1)
  ENMB0_p ~ dbeta(1,1)
  ENMA0_p ~ dbeta(1,1)
  ENSB0_p ~ dbeta(1,1)
  ENSA0_p ~ dbeta(1,1)
  ENFB0_p ~ dbeta(1,1)
  ENFA0_p ~ dbeta(1,1)
  WVTB0_p ~ dbeta(1,1)
  WVTA0_p ~ dbeta(1,1)

  #-------------------------
  # Methow sites
  #-------------------------
  WEA_p ~ dbeta(1,1)
  LMRB0_p ~ dbeta(1,1)
  LMRA0_p ~ dbeta(1,1)
  GLCB0_p ~ dbeta(1,1)
  GLCA0_p ~ dbeta(1,1)
  LBCB0_p ~ dbeta(1,1)
  LBCA0_p ~ dbeta(1,1)
  MRCB0_p ~ dbeta(1,1)
  MRCA0_p ~ dbeta(1,1)
  BVCB0_p ~ dbeta(1,1)
  BVCA0_p ~ dbeta(1,1)
  TWRB0_p ~ dbeta(1,1)
  TWRA0_p ~ dbeta(1,1)
  TWISPW_p <- 1 # assume perfect detection (i.e. worst case)
  CRWB0_p ~ dbeta(1,1)
  CRWA0_p ~ dbeta(1,1)
  CRUB0_p ~ dbeta(1,1)
  CRUA0_p ~ dbeta(1,1)
  SCPB0_p ~ dbeta(1,1)
  SCPA0_p ~ dbeta(1,1)
  MSHB0_p ~ dbeta(1,1)
  MSHA0_p ~ dbeta(1,1)
  METHB0_p ~ dbeta(1,1)
  METHA0_p ~ dbeta(1,1)
  MRWB0_p ~ dbeta(1,1)
  MRWA0_p ~ dbeta(1,1)
  WFC_p <- 1 # assume perfect detection (i.e. worst case)

  #-------------------------
  # Okanogan sites
  #-------------------------
  FST_p <- 1 # assume perfect detection (i.e. worst case)
  OKLB0_p ~ dbeta(1,1)
  OKLA0_p ~ dbeta(1,1)
  LLCB0_p ~ dbeta(1,1)
  LLCA0_p ~ dbeta(1,1)
  SA1B0_p ~ dbeta(1,1)
  SA1A0_p ~ dbeta(1,1)
  SA0B0_p ~ dbeta(1,1)
  SA0A0_p ~ dbeta(1,1)
  OMKB0_p ~ dbeta(1,1)
  OMKA0_p ~ dbeta(1,1)
  OBF_p <- 1 # assume perfect detection (i.e. worst case)
  WAN_p <- 1 # assume perfect detection (i.e. worst case)
  JOH_p <- 1 # assume perfect detection (i.e. worst case)
  TNK_p <- 1 # assume perfect detection (i.e. worst case)
  AEN_p <- 1 # assume perfect detection (i.e. worst case)
  BPCB0_p ~ dbeta(1,1)
  BPCA0_p ~ dbeta(1,1)
  ANTB0_p ~ dbeta(1,1)
  ANTA0_p ~ dbeta(1,1)
  WHS_p <- 1 # assume perfect detection (i.e. worst case)
  TON_p <- 1 # assume perfect detection (i.e. worst case)
  ZSLB0_p ~ dbeta(1,1)
  ZSLA0_p ~ dbeta(1,1)
  NMCB0_p ~ dbeta(1,1)
  NMCA0_p ~ dbeta(1,1)
  OKIB0_p ~ dbeta(1,1)
  OKIA0_p ~ dbeta(1,1)
  OKCB0_p ~ dbeta(1,1)
  OKCA0_p ~ dbeta(1,1)

  #-------------------------
  # Downstream of Priest Rapids
  #-------------------------
  BelowJD1_p <- 1 # assume perfect detection (i.e. worst case)
  JD1B0_p ~ dbeta(1,1)
  JD1A0_p ~ dbeta(1,1)
  TMF_p <- 1 # assume perfect detection (i.e. worst case)
  PRV_p ~ dbeta(1,1)
  HSTB0_p ~ dbeta(1,1)
  HSTA0_p ~ dbeta(1,1)
  MDRB0_p ~ dbeta(1,1)
  MDRA0_p ~ dbeta(1,1)
  ICHB0_p ~ dbeta(1,1)
  ICHA0_p ~ dbeta(1,1)
  PROB0_p ~ dbeta(1,1)
  PROA0_p ~ dbeta(1,1)
  RSHB0_p ~ dbeta(1,1)
  RSHA0_p ~ dbeta(1,1)
  PRHB0_p ~ dbeta(1,1)
  PRHA0_p ~ dbeta(1,1)


  ################################################################################
  # Initial branching structure after leaving Priest Rapids Dam
  ################################################################################
  # There are 3 bins -  black box (1), Rock Island (2), downstream (3)

  # first row is wild fish, second row is hatchery fish
  p_pop_PRA[1,1:n_pops_PRA] ~ ddirch(PRA_dirch_vec[1,]); # Dirichlet for probs for going to n_pops_PRA bins
  p_pop_PRA[2,1:n_pops_PRA] ~ ddirch(PRA_dirch_vec[2,]); # Dirichlet for probs for going to n_pops_PRA bins

  for (i in 1:n_fish) {
  a_PRA[i] ~ dcat( p_pop_PRA[fishOrigin[i], 1:n_pops_PRA] )
  # expand the dcat variable into a matrix of zeros and ones

  for (j in 1:n_pops_PRA)	{
  catexp_PRA[i,j] <- equals(a_PRA[i],j) #equals(x,y) is a test for equality, returns [1,0]
  }
  }

  ################################################################################
  # Observations at Rock Island dam
  ################################################################################
  # only have to worry about observation piece
  for ( i in 1:n_fish ) {
  Wenatchee[i, 1] ~ dbern(RIA_p * catexp_PRA[i,2])
  }

  ################################################################################
  # Now we deal with above Rock Island dam
  ################################################################################
  # There are 3 bins -  mainstem (1), LWE (2), RRF(3)

  # first row is wild fish, second row is hatchery fish
  p_pop_RIA[1, 1:(n_pops_RIA)] ~ ddirch(RIA_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_RIA[2, 1:(n_pops_RIA)] ~ ddirch(RIA_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatRIA[1,1:n_pops_RIA] <- zero_vec[1:(n_pops_RIA)] # when not in trib, 0 prob of being in sub areas
  pMatRIA[1,(n_pops_RIA+1)] <- 1 #set the 'not there' bin to prob = 1

  # 2nd row is wild fish, 3rd row is hatchery
  pMatRIA[2,1:n_pops_RIA] <- p_pop_RIA[1,] # when in trib, >0 probs of being in sub areas
  pMatRIA[2,(n_pops_RIA+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatRIA[3,1:n_pops_RIA] <- p_pop_RIA[2,] # when in trib, >0 probs of being in sub areas
  pMatRIA[3,(n_pops_RIA+1)] <- 0 #set the 'not there' bin to prob = 0


  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_RIA[i] ~ dcat( pMatRIA[(catexp_PRA[i,2] * fishOrigin[i] + 1), 1:(n_pops_RIA+1)] )

  for (j in 1:(n_pops_RIA+1))	{
  catexp_RIA[i,j] <- equals(a_RIA[i],j)
  }
  }

  ################################################################################
  # Now we deal with Wenatchee
  ################################################################################

  #----------------
  # LOWER WENACHEE - between mouth of Wenachee and TUMFBY/TUF/TUM
  # There are 6 bins -  mainstem (1), MCL (2), PES (3), CHM (4), ICL (5), upstream/TUM (6)

  # first row is wild fish, second row is hatchery fish
  p_pop_LWE[1, 1:(n_pops_LWE)] ~ ddirch(LWE_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_LWE[2, 1:(n_pops_LWE)] ~ ddirch(LWE_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatLWE[1,1:n_pops_LWE] <- zero_vec[1:(n_pops_LWE)] # when not in trib, 0 prob of being in sub areas
  pMatLWE[1,(n_pops_LWE+1)] <- 1 #set the 'not there' bin to prob = 1

  # 2nd row is wild fish, 3rd row is hatchery
  pMatLWE[2,1:n_pops_LWE] <- p_pop_LWE[1,] # when in trib, >0 probs of being in sub areas
  pMatLWE[2,(n_pops_LWE+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatLWE[3,1:n_pops_LWE] <- p_pop_LWE[2,] # when in trib, >0 probs of being in sub areas
  pMatLWE[3,(n_pops_LWE+1)] <- 0 #set the 'not there' bin to prob = 0

  #----------------
  # ABOVE ICL
  # There are 3 bins -  mainstem (1), LNF (2), ICM (3)

  # first row is wild fish, second row is hatchery fish
  p_pop_ICL[1, 1:(n_pops_ICL)] ~ ddirch(ICL_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_ICL[2, 1:(n_pops_ICL)] ~ ddirch(ICL_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatICL[1,1:n_pops_ICL] <- zero_vec[1:(n_pops_ICL)] # when not in trib, 0 prob of being in sub areas
  pMatICL[1,(n_pops_ICL+1)] <- 1 #set the 'not there' bin to prob = 1

  # 2nd row is wild fish, 3rd row is hatchery
  pMatICL[2,1:n_pops_ICL] <- p_pop_ICL[1,] # when in trib, >0 probs of being in sub areas
  pMatICL[2,(n_pops_ICL+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatICL[3,1:n_pops_ICL] <- p_pop_ICL[2,] # when in trib, >0 probs of being in sub areas
  pMatICL[3,(n_pops_ICL+1)] <- 0 #set the 'not there' bin to prob = 0

  #------------------------
  # True state of fish
  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_LWE[i] ~ dcat( pMatLWE[(catexp_RIA[i,2] * fishOrigin[i] + 1), 1:(n_pops_LWE+1)] )

  for (j in 1:(n_pops_LWE+1))	{ #now expand the dcat into matrix of zeros and ones
  catexp_LWE[i,j] <- equals(a_LWE[i],j)
  }
  }

  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_ICL[i] ~ dcat( pMatICL[(catexp_LWE[i,5] * fishOrigin[i] + 1), 1:(n_pops_ICL+1)] )

  for (j in 1:(n_pops_ICL+1))	{ #now expand the dcat into matrix of zeros and ones
  catexp_ICL[i,j] <- equals(a_ICL[i],j)
  }
  }

  #-----------------------------------------------------------
  # migration model up Peshastin and Icicle (like survival):
  #-----------------------------------------------------------
  for(j in 1:2) {
  phi_icu[j] ~ dbeta(1,1) # prob of migrating up past ICU
  phi_peu[j] ~ dbeta(1,1) # prob of migrating up past PEU
  }

  # OBSERTVATION PART FOR LOWER WENACHEE

  for (i in 1:n_fish) {
  # lowest array
  Wenatchee[i,2] ~ dbern(LWEB0_p * max(catexp_LWE[i,1:(n_pops_LWE)]) )
  Wenatchee[i,3] ~ dbern(LWEA0_p * max(catexp_LWE[i,1:(n_pops_LWE)]) )

  # next do MCL
  Wenatchee[i,4] ~ dbern(MCLB0_p * catexp_LWE[i,2])
  Wenatchee[i,5] ~ dbern(MCLA0_p * catexp_LWE[i,2])

  # next do PES
  Wenatchee[i,6] ~ dbern(PESB0_p * catexp_LWE[i,3])
  Wenatchee[i,7] ~ dbern(PESA0_p * catexp_LWE[i,3])

  z_peu[i] ~ dbern(catexp_LWE[i,3] * phi_peu[fishOrigin[i]] ) # did fish go past PEU?
  Wenatchee[i,8] ~ dbern(PEUB0_p * z_peu[i])
  Wenatchee[i,9] ~ dbern(PEUA0_p  * z_peu[i])

  # next do CHM
  Wenatchee[i,10] ~ dbern(CHMB0_p * catexp_LWE[i,4])
  Wenatchee[i,11] ~ dbern(CHMA0_p * catexp_LWE[i,4])


  # next do ICL
  Wenatchee[i,12] ~ dbern(ICLB0_p * catexp_LWE[i,5])
  Wenatchee[i,13] ~ dbern(ICLA0_p * catexp_LWE[i,5])
  Wenatchee[i,14] ~ dbern(LNFB0_p * catexp_ICL[i,2])
  Wenatchee[i,15] ~ dbern(LNFA0_p * catexp_ICL[i,2])
  Wenatchee[i,16] ~ dbern(ICMB0_p * catexp_ICL[i,3])
  Wenatchee[i,17] ~ dbern(ICMA0_p * catexp_ICL[i,3])

  z_icu[i] ~ dbern(catexp_ICL[i,3] * phi_icu[fishOrigin[i]] ) # did fish go past ICU?
  Wenatchee[i,18] ~ dbern(ICUB0_p * z_icu[i])
  Wenatchee[i,19] ~ dbern(ICUA0_p * z_icu[i])

  # Tumwater (TUF)...
  Wenatchee[i,20] ~ dbern(TUM_p * catexp_LWE[i,6])

  } #ends the ifish loop started at the top of this section


  #----------------
  # UPPER WENACHEE - Above TUMFBY/TUF
  # There are 4 bins --- mainstem (1), CHW (2), Chiwawa/CHL (3), UWE (4)

  # first row is wild fish, second row is hatchery fish
  p_pop_TUM[1, 1:(n_pops_TUM)] ~ ddirch(TUM_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_TUM[2, 1:(n_pops_TUM)] ~ ddirch(TUM_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatTUM[1,1:n_pops_TUM] <- zero_vec[1:(n_pops_TUM)] # when not in trib, 0 prob of being in sub areas
  pMatTUM[1,(n_pops_TUM+1)] <- 1 #set the 'not there' bin to prob = 1

  # 2nd row is wild fish, 3rd row is hatchery
  pMatTUM[2,1:n_pops_TUM] <- p_pop_TUM[1,] # when in trib, >0 probs of being in sub areas
  pMatTUM[2,(n_pops_TUM+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatTUM[3,1:n_pops_TUM] <- p_pop_TUM[2,] # when in trib, >0 probs of being in sub areas
  pMatTUM[3,(n_pops_TUM+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_TUM[i] ~ dcat( pMatTUM[(catexp_LWE[i,6] * fishOrigin[i] + 1), 1:(n_pops_TUM+1)] ) # the row number acts as on/off switch (6 = Lower-->upper)

  for (j in 1:(n_pops_TUM+1))	{ #now expand the dcat into matrix of zeros and ones
  catexp_TUM[i,j] <- equals(a_TUM[i],j) #equals(x,y) is a test for equality, returns [1,0]
  }
  }

  # Past CHU
  for(j in 1:2) {
  phi_chu[j] ~ dbeta(1,1) # prob of migrating up past CHU
  }

  # OBSERTVATION PART FOR UPPER WENACHEE
  for (i in 1:n_fish) {

  # next do Chiwaukum Creek (CHW)
  Wenatchee[i,21] ~ dbern(CHWB0_p * catexp_TUM[i,2])
  Wenatchee[i,22] ~ dbern(CHWA0_p * catexp_TUM[i,2])

  # Chiwawa
  Wenatchee[i,23] ~ dbern(CHLB0_p * catexp_TUM[i,3]) # did they go past CHL?
  Wenatchee[i,24] ~ dbern(CHLA0_p * catexp_TUM[i,3])

  z_chu[i] ~ dbern(catexp_TUM[i,3] * phi_chu[fishOrigin[i]] ) # did they go past CHU?
  Wenatchee[i,25] ~ dbern(CHUB0_p * z_chu[i])
  Wenatchee[i,26] ~ dbern(CHUA0_p * z_chu[i])

  # UWE
  Wenatchee[i,27] ~ dbern(UWE_p * catexp_TUM[i,4]) # did they go past UWE?

  }

  #----------------
  # Very UPPER WENACHEE - Above UWE
  # There are 4 bins --- mainstem (1), NAL (2), WTL (3), LWN (4)

  # first row is wild fish, second row is hatchery fish
  p_pop_UWE[1, 1:(n_pops_UWE)] ~ ddirch(UWE_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_UWE[2, 1:(n_pops_UWE)] ~ ddirch(UWE_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatUWE[1,1:n_pops_UWE] <- zero_vec[1:(n_pops_UWE)] # when not in trib, 0 prob of being in sub areas
  pMatUWE[1,(n_pops_UWE+1)] <- 1 #set the 'not there' bin to prob = 1

  # 2nd row is wild fish, 3rd row is hatchery
  pMatUWE[2,1:n_pops_UWE] <- p_pop_UWE[1,] # when in trib, >0 probs of being in sub areas
  pMatUWE[2,(n_pops_UWE+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatUWE[3,1:n_pops_UWE] <- p_pop_UWE[2,] # when in trib, >0 probs of being in sub areas
  pMatUWE[3,(n_pops_UWE+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_UWE[i] ~ dcat( pMatUWE[(catexp_TUM[i,4] * fishOrigin[i] + 1), 1:(n_pops_UWE+1)] )

  for (j in 1:(n_pops_UWE+1))	{ #now expand the dcat into matrix of zeros and ones
  catexp_UWE[i,j] <- equals(a_UWE[i],j) #equals(x,y) is a test for equality, returns [1,0]
  }
  }

  #-----------------------------------------------------------
  # migration model up Nason Creek (like survival):
  #-----------------------------------------------------------
  for(j in 1:2) {
  phi_nau[j] ~ dbeta(1,1) #prob of migrating past NAU
  }

  # OBSERTVATION PART FOR UPPER WENACHEE
  for (i in 1:n_fish) {

  # Nason
  Wenatchee[i,28] ~ dbern(NALB0_p * catexp_UWE[i,2]) # did they go past NAL?
  Wenatchee[i,29] ~ dbern(NALA0_p * catexp_UWE[i,2])

  z_nau[i] ~ dbern(catexp_UWE[i,2] * phi_nau[fishOrigin[i]] )  # did fish go past NAU?
  Wenatchee[i,30] ~ dbern(NAUB0_p * z_nau[i])
  Wenatchee[i,31] ~ dbern(NAUA0_p * z_nau[i])

  # White River
  Wenatchee[i,32] ~ dbern(WTLB0_p * catexp_UWE[i,3])
  Wenatchee[i,33] ~ dbern(WTLA0_p * catexp_UWE[i,3])

  # Little Wenatchee
  Wenatchee[i,34] ~ dbern(LWNB0_p * catexp_UWE[i,4])
  Wenatchee[i,35] ~ dbern(LWNA0_p * catexp_UWE[i,4])


  } #ends the ifish loop started at the top of this section

  ################################################################################
  # Observations at Rocky Reach dam
  ################################################################################
  # only have to worry about observation piece
  for ( i in 1:n_fish ) {
  Entiat[i, 1] ~ dbern(RRF_p * catexp_RIA[i,3])
  }

  ################################################################################
  # Now we deal with above Rocky Reach dam
  ################################################################################
  # There are 5 bins -  mainstem (1), ENL (2), WEA (3), WVT (4)

  # first row is wild fish, second row is hatchery fish
  p_pop_RRF[1, 1:(n_pops_RRF)] ~ ddirch(RRF_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_RRF[2, 1:(n_pops_RRF)] ~ ddirch(RRF_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatRRF[1,1:n_pops_RRF] <- zero_vec[1:(n_pops_RRF)] # when not in trib, 0 prob of being in sub areas
  pMatRRF[1,(n_pops_RRF+1)] <- 1 #set the 'not there' bin to prob = 1

  # 2nd row is wild fish, 3rd row is hatchery
  pMatRRF[2,1:n_pops_RRF] <- p_pop_RRF[1,] # when in trib, >0 probs of being in sub areas
  pMatRRF[2,(n_pops_RRF+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatRRF[3,1:n_pops_RRF] <- p_pop_RRF[2,] # when in trib, >0 probs of being in sub areas
  pMatRRF[3,(n_pops_RRF+1)] <- 0 #set the 'not there' bin to prob = 0


  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_RRF[i] ~ dcat( pMatRRF[(catexp_RIA[i,3] * fishOrigin[i] + 1), 1:(n_pops_RRF+1)] )

  for (j in 1:(n_pops_RRF+1))	{
  catexp_RRF[i,j] <- equals(a_RRF[i],j)
  }
  }


  ################################################################################
  # Now we deal with Entiat
  ################################################################################

  # We use catexp_RRF[i,2] as an on/off switch for presence/absence of fish in entiat
  # We model the prob of fish moving to consecutive upstream bins in the Entiat as follows...
  # P(being in bin) = P(migrating from next lowest bin) * present in lower bin (1/0)

  # There are 5 bins: mainstem (1), RCT (2), EHL (3), ENA and above (4), MAD (5)

  # first row is wild fish, second row is hatchery fish
  p_pop_ENL[1, 1:(n_pops_ENL)] ~ ddirch(ENL_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_ENL[2, 1:(n_pops_ENL)] ~ ddirch(ENL_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatEnt[1,1:n_pops_ENL] <- zero_vec[1:(n_pops_ENL)] # when not in trib, 0 prob of being in sub areas
  pMatEnt[1,(n_pops_ENL+1)] <- 1 #set the 'not there' bin to prob = 1

  pMatEnt[2,1:n_pops_ENL] <- p_pop_ENL[1,] # when in trib, >0 probs of being in sub areas
  pMatEnt[2,(n_pops_ENL+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatEnt[3,1:n_pops_ENL] <- p_pop_ENL[2,] # when in trib, >0 probs of being in sub areas
  pMatEnt[3,(n_pops_ENL+1)] <- 0 #set the 'not there' bin to prob = 0

  # deal with upstream migration like survival
  for( j in 1:2) {
  phi_enm[j] ~ dbeta(1,1) #prob of migrating past ENM
  phi_ens[j] ~ dbeta(1,1) #prob of migrating past ENS
  phi_enf[j] ~ dbeta(1,1) #prob of migrating past ENF
  }


  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_ENL[i] ~ dcat( pMatEnt[(catexp_RRF[i,2] * fishOrigin[i] + 1), 1:(n_pops_ENL+1)] )

  for (j in 1:(n_pops_ENL+1))	{
  catexp_ENL[i,j] <- equals(a_ENL[i],j)
  }

  # ENL
  Entiat[i,2] ~ dbern(ENLB0_p * max(catexp_ENL[i,1:(n_pops_ENL)]) )
  Entiat[i,3] ~ dbern(ENLA0_p * max(catexp_ENL[i,1:(n_pops_ENL)]) )

  # observation part for RCT
  Entiat[i,4] ~ dbern(RCTB0_p * catexp_ENL[i,2])
  Entiat[i,5] ~ dbern(RCTA0_p * catexp_ENL[i,2])

  # observation part for EHL
  Entiat[i,6] ~ dbern(EHLB0_p * catexp_ENL[i,3])
  Entiat[i,7] ~ dbern(EHLA0_p * catexp_ENL[i,3])

  # observation part for MAD
  Entiat[i,10] ~ dbern(MADB0_p * catexp_ENL[i,5])
  Entiat[i,11] ~ dbern(MADA0_p * catexp_ENL[i,5])

  # observation part for ENA
  Entiat[i,8] ~ dbern(ENAB0_p * catexp_ENL[i,4]) # did they go past ENA?
  Entiat[i,9] ~ dbern(ENAA0_p * catexp_ENL[i,4])

  # did the fish pass ENM?
  z_enm[i] ~ dbern(catexp_ENL[i,4] * phi_enm[fishOrigin[i]])
  Entiat[i,12] ~ dbern(ENMB0_p * z_enm[i] )
  Entiat[i,13] ~ dbern(ENMA0_p * z_enm[i] )

  # did the fish pass ENS?
  z_ens[i] ~ dbern(z_enm[i] * phi_ens[fishOrigin[i]])
  Entiat[i,14] ~ dbern(ENSB0_p * z_ens[i] )
  Entiat[i,15] ~ dbern(ENSA0_p * z_ens[i] )

  # did the fish pass ENF?
  z_enf[i] ~ dbern(z_ens[i] * phi_enf[fishOrigin[i]])
  Entiat[i,16] ~ dbern(ENFB0_p * z_enf[i] )
  Entiat[i,17] ~ dbern(ENFA0_p * z_enf[i] )


  } #ends n_fish loop in this section

  ################################################################################
  # Observations at WVT (right below Wells Dam)
  ################################################################################
  # only have to worry about observation piece
  for ( i in 1:n_fish ) {
    Entiat[i, 18] ~ dbern(WVTB0_p * catexp_RRF[i,4])
    Entiat[i, 19] ~ dbern(WVTA0_p * catexp_RRF[i,4])
  }

  ################################################################################
  # Observations at Wells Dam
  ################################################################################
  # only have to worry about observation piece
  for ( i in 1:n_fish ) {
  Methow[i, 1] ~ dbern(WEA_p * catexp_RRF[i,3])
  }


  ################################################################################
  # Now we deal with above Wells dam
  ################################################################################

  # We use catexp_RRF[i,3] as an on/off switch for presence/absence of fish past Wells Dam
  # There are 4 bins, Mainstem (1), Methow (2), Okanogan (3) and FST (4)

  p_pop_WEA[1,1:(n_pops_WEA)] ~ ddirch(WEA_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_WEA[2,1:(n_pops_WEA)] ~ ddirch(WEA_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatWEA[1,1:n_pops_WEA]<-zero_vec[1:(n_pops_WEA)] # when not in trib, 0 prob of being in sub areas
  pMatWEA[1,(n_pops_WEA+1)]<-1 #set the 'not there' bin to prob = 1

  pMatWEA[2,1:n_pops_WEA] <- p_pop_WEA[1,] # when in trib, >0 probs of being in sub areas
  pMatWEA[2,(n_pops_WEA+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatWEA[3,1:n_pops_WEA] <- p_pop_WEA[2,] # when in trib, >0 probs of being in sub areas
  pMatWEA[3,(n_pops_WEA+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {
  a_WEA[i] ~ dcat( pMatWEA[(catexp_RRF[i,3] * fishOrigin[i] + 1), 1:(n_pops_WEA+1)] )

  for (j in 1:(n_pops_WEA+1))  {
  catexp_WEA[i,j] <- equals(a_WEA[i],j)
  }
  }

  ################################################################################
  # Now we deal with lower Methow
  ################################################################################

  # We use catexp_WEA[i,2] as an on/off switch for presence/absence of fish in Methow
  # There are 4 bins, Mainstem (1), GLC (2), LBC (3), upstream/MRC (4)

  # There are 4 sections (z bins) in Methow...
  p_pop_LMR[1, 1:(n_pops_LMR)] ~ ddirch(LMR_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_LMR[2, 1:(n_pops_LMR)] ~ ddirch(LMR_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatLMR[1,1:n_pops_LMR] <- zero_vec[1:(n_pops_LMR)] # when not in trib, 0 prob of being in sub areas
  pMatLMR[1,(n_pops_LMR+1)] <- 1 #set the 'not there' bin to prob = 1

  pMatLMR[2,1:n_pops_LMR] <- p_pop_LMR[1,] # when in trib, >0 probs of being in sub areas
  pMatLMR[2,(n_pops_LMR+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatLMR[3,1:n_pops_LMR] <- p_pop_LMR[2,] # when in trib, >0 probs of being in sub areas
  pMatLMR[3,(n_pops_LMR+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {
  a_LMR[i] ~ dcat( pMatLMR[(catexp_WEA[i,2] * fishOrigin[i] + 1), 1:(n_pops_LMR+1)] )

  for (j in 1:(n_pops_LMR+1))	{
  catexp_LMR[i,j] <- equals(a_LMR[i],j)
  }

  #observation for LMR
  Methow[i,2] ~ dbern(LMRB0_p * max(catexp_LMR[i,1:(n_pops_LMR)]) )
  Methow[i,3] ~ dbern(LMRA0_p * max(catexp_LMR[i,1:(n_pops_LMR)]) )

  # observation part for GLC
  Methow[i,4] ~ dbern(GLCB0_p * catexp_LMR[i,2])
  Methow[i,5] ~ dbern(GLCA0_p * catexp_LMR[i,2])

  # observation part for LBC
  Methow[i,6] ~ dbern(LBCB0_p * catexp_LMR[i,3])
  Methow[i,7] ~ dbern(LBCA0_p * catexp_LMR[i,3])

  # observation part for MRC
  Methow[i,8] ~ dbern(MRCB0_p * catexp_LMR[i,4])
  Methow[i,9] ~ dbern(MRCA0_p * catexp_LMR[i,4])

  } #ends n_fish loop in this section

  ################################################################################
  # Now we deal with upper Methow - past MRC
  ################################################################################
  for(j in 1:2) {
  phi_meth[j] ~ dbeta(1,1) # probability of moving past METH
  phi_twispw[j] ~ dbeta(1,1) # probability of moving past TWISPW
  phi_cru[j] ~ dbeta(1,1) # probability of moving past CRU
  phi_wfc[j] ~ dbeta(1,1) # probability of moving past WFC
  }

  # We use catexp_LMR[i,4] as an on/off switch for presence/absence of fish in Methow
  # There are 7 bins, Mainstem (1), BVC (2), TWR (3), CRW (4), SCP (5), MSH (6), MRW (7)

  # There are 5 sections (z bins) in Methow...
  p_pop_MRC[1, 1:(n_pops_MRC)] ~ ddirch(MRC_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_MRC[2, 1:(n_pops_MRC)] ~ ddirch(MRC_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatMRC[1,1:n_pops_MRC] <- zero_vec[1:(n_pops_MRC)] # when not in trib, 0 prob of being in sub areas
  pMatMRC[1,(n_pops_MRC+1)] <- 1 #set the 'not there' bin to prob = 1

  pMatMRC[2,1:n_pops_MRC] <- p_pop_MRC[1,] # when in trib, >0 probs of being in sub areas
  pMatMRC[2,(n_pops_MRC+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatMRC[3,1:n_pops_MRC] <- p_pop_MRC[2,] # when in trib, >0 probs of being in sub areas
  pMatMRC[3,(n_pops_MRC+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {
  a_MRC[i] ~ dcat( pMatMRC[(catexp_LMR[i,4] * fishOrigin[i] + 1), 1:(n_pops_MRC+1)] )

    for (j in 1:(n_pops_MRC+1))  {
      catexp_MRC[i,j] <- equals(a_MRC[i],j)
    }

  # observation part for BVC
  Methow[i,10] ~ dbern(BVCB0_p * catexp_MRC[i,2])
  Methow[i,11] ~ dbern(BVCA0_p * catexp_MRC[i,2])

  # observation part for TWR
  Methow[i,12] ~ dbern(TWRB0_p * catexp_MRC[i,3])
  Methow[i,13] ~ dbern(TWRA0_p * catexp_MRC[i,3])

  z_twispw[i] ~ dbern(catexp_MRC[i,3] * phi_twispw[fishOrigin[i]] ) # did fish move past TWISPW?
  Method[i,14] ~ dbern(TWISPW_p * z_twispw[i])

  # observation part for CRW
  Methow[i,15] ~ dbern(CRWB0_p * catexp_MRC[i,4])
  Methow[i,16] ~ dbern(CRWA0_p * catexp_MRC[i,4])

  # upper array (CRU, above CRW)
  z_cru[i] ~ dbern(catexp_MRC[i,4] * phi_cru[fishOrigin[i]] ) # did fish move past CRU?
  Methow[i,17] ~ dbern(CRUB0_p * z_cru[i])
  Methow[i,18] ~ dbern(CRUA0_p * z_cru[i])

  # observation part for SCP
  Methow[i,19] ~ dbern(SCPB0_p * catexp_MRC[i,5])
  Methow[i,20] ~ dbern(SCPA0_p * catexp_MRC[i,5])

  # observation part for MSH
  Methow[i,21] ~ dbern(MSHB0_p * catexp_MRC[i,6])
  Methow[i,22] ~ dbern(MSHA0_p * catexp_MRC[i,6])

  z_meth[i] ~ dbern(catexp_MRC[i,6] * phi_meth[fishOrigin[i]] ) # did fish move past METH?
  Methow[i,23] ~ dbern(METHB0_p * z_meth[i])
  Methow[i,24] ~ dbern(METHA0_p * z_meth[i])

  # observation part for MRW
  Methow[i,25] ~ dbern(MRWB0_p * catexp_MRC[i,7])
  Methow[i,26] ~ dbern(MRWA0_p * catexp_MRC[i,7])

  # upper array (WFC, above MRW)
  z_wfc[i] ~ dbern(catexp_MRC[i,7] * phi_wfc[fishOrigin[i]] ) # did fish move past WFC?
  Methow[i,27] ~ dbern(WFC_p * z_wfc[i])

  } #ends n_fish loop in this section

  ################################################################################
  # Now we deal with Okanogan
  ################################################################################
  for(j in 1:2) {
  phi_sa0[j] ~ dbeta(1,1) # prob of migrating past SA0
  phi_obf[j] ~ dbeta(1,1) # prob of migrating past OBF
  }

  # We use catexp_WEA[i,3] as an on/off switch for presence/absence of fish past OKL
  # There are 12 bins, Mainstem (1), LLC (2), SA1 (3), OMK (4), WAN (5), JOH (6), TNK (7), AEN (8), BPC (9), ANT (10), WHS (11), ZSL (12)

  p_pop_OKL[1, 1:(n_pops_OKL)] ~ ddirch(OKL_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_OKL[2, 1:(n_pops_OKL)] ~ ddirch(OKL_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatOKL[1,1:n_pops_OKL] <- zero_vec[1:(n_pops_OKL)] # when not in trib, 0 prob of being in sub areas
  pMatOKL[1,(n_pops_OKL+1)] <- 1 #set the 'not there' bin to prob = 1

  pMatOKL[2,1:n_pops_OKL] <- p_pop_OKL[1,] # when in trib, >0 probs of being in sub areas
  pMatOKL[2,(n_pops_OKL+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatOKL[3,1:n_pops_OKL] <- p_pop_OKL[2,] # when in trib, >0 probs of being in sub areas
  pMatOKL[3,(n_pops_OKL+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {

  a_OKL[i] ~ dcat( pMatOKL[(catexp_WEA[i,3] * fishOrigin[i] + 1), 1:(n_pops_OKL+1)] )

  for (j in 1:(n_pops_OKL+1))  {
  catexp_OKL[i,j] <- equals(a_OKL[i],j)
  }


  # Observation in Okanogan
  Okanogan[i,1] ~ dbern(OKLB0_p * max(catexp_OKL[i,1:(n_pops_OKL)]) )
  Okanogan[i,2] ~ dbern(OKLA0_p * max(catexp_OKL[i,1:(n_pops_OKL)]) )

  # observation part for LLC
  Okanogan[i,3] ~ dbern(LLCB0_p * catexp_OKL[i,2])
  Okanogan[i,4] ~ dbern(LLCA0_p * catexp_OKL[i,2])

  # observation part for SA1
  Okanogan[i,5] ~ dbern(SA1B0_p * catexp_OKL[i,3])
  Okanogan[i,6] ~ dbern(SA1A0_p * catexp_OKL[i,3])

  z_sa0[i] ~ dbern(catexp_OKL[i,3] * phi_sa0[fishOrigin[i]] ) # did fish move past SA0?

  # observation part for SA0
  Okanogan[i,7] ~ dbern(SA0B0_p * z_sa0[i])
  Okanogan[i,8] ~ dbern(SA0A0_p * z_sa0[i])

  # observation part for OMK
  Okanogan[i,9] ~ dbern(OMKB0_p * catexp_OKL[i,4])
  Okanogan[i,10] ~ dbern(OMKA0_p * catexp_OKL[i,4])

  z_obf[i] ~ dbern(catexp_OKL[i,4] * phi_obf[fishOrigin[i]] )
  Okanogan[i,11] ~ dbern(OBF_p * z_obf[i] )

  # observation part for WAN
  Okanogan[i,12] ~ dbern(WAN_p * catexp_OKL[i,5])

  # observation part for JOH
  Okanogan[i,13] ~ dbern(JOH_p * catexp_OKL[i,6])

  # observation part for TNK
  Okanogan[i,14] ~ dbern(TNK_p * catexp_OKL[i,7])

  # observation part for AEN
  Okanogan[i,15] ~ dbern(AEN_p * catexp_OKL[i,8])

  # observation part for BPC
  Okanogan[i,16] ~ dbern(BPCB0_p * catexp_OKL[i,9])
  Okanogan[i,17] ~ dbern(BPCA0_p * catexp_OKL[i,9])

  # observation part for ANT
  Okanogan[i,18] ~ dbern(ANTB0_p * catexp_OKL[i,10])
  Okanogan[i,19] ~ dbern(ANTA0_p * catexp_OKL[i,10])

  # observation part for WHS
  Okanogan[i,20] ~ dbern(WHS_p * catexp_OKL[i,11])

  # observation part at Zosel
  Okanogan[i,21] ~ dbern(ZSLB0_p * catexp_OKL[i,12])
  Okanogan[i,22] ~ dbern(ZSLA0_p * catexp_OKL[i,12])


  } #ends n_fish loop in this section

  ################################################################################
  # NOW WE DEAL WITH ZSL POOLS
  ################################################################################
  # We use catexp_OKL[i,12] as an on/off switch for presence/absence of fish in ZSL
  # There are 5 bins, Mainstem (1), TON (2), NMC (3), OKI (4), OKC (5)

  p_pop_ZSL[1, 1:(n_pops_ZSL)] ~ ddirch(ZSL_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_ZSL[2, 1:(n_pops_ZSL)] ~ ddirch(ZSL_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatZSL[1,1:n_pops_ZSL] <- zero_vec[1:(n_pops_ZSL)] # when not in trib, 0 prob of being in sub areas
  pMatZSL[1,(n_pops_ZSL+1)] <- 1 #set the 'not there' bin to prob = 1

  pMatZSL[2,1:n_pops_ZSL] <- p_pop_ZSL[1,] # when in trib, >0 probs of being in sub areas
  pMatZSL[2,(n_pops_ZSL+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatZSL[3,1:n_pops_ZSL] <- p_pop_ZSL[2,] # when in trib, >0 probs of being in sub areas
  pMatZSL[3,(n_pops_ZSL+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {

  a_ZSL[i] ~ dcat( pMatZSL[(catexp_OKL[i,12] * fishOrigin[i] + 1), 1:(n_pops_ZSL+1)] )

  for (j in 1:(n_pops_ZSL+1))  {
  catexp_ZSL[i,j] <- equals(a_ZSL[i],j)
  }

  # Observations
  # next do TON
  Okanogan[i,23] ~ dbern(TON_p * catexp_ZSL[i,2])

  # next do NMC
  Okanogan[i,24] ~ dbern(NMCB0_p * catexp_ZSL[i,3])
  Okanogan[i,25] ~ dbern(NMCA0_p * catexp_ZSL[i,3])

  # next do OKI
  Okanogan[i,26] ~ dbern(OKIB0_p * catexp_ZSL[i,4])
  Okanogan[i,27] ~ dbern(OKIA0_p * catexp_ZSL[i,4])

  # next do OKC
  Okanogan[i,28] ~ dbern(OKCB0_p * catexp_ZSL[i,5])
  Okanogan[i,29] ~ dbern(OKCA0_p * catexp_ZSL[i,5])

  } #ends n_fish loop in this section


  ################################################################################
  # Observations at FST
  ################################################################################
  for(i in 1:n_fish) {
  Okanogan[i,30] ~ dbern(FST_p * catexp_WEA[i,4])
  }


  ################################################################################
  # Now we deal with sites downstream of Priest Rapids dam
  ################################################################################

  # We use catexp_PRA[i,3] as an on/off switch for presence/absence of fish downstream of Priest Rapids dam
  # There are 8 bins (no black box):  below JD1 (1), JD1 (2), TMF (3), PRV (4), ICH (5), PRO (6), RSH (7), PRH (8)

  p_pop_dwn[1, 1:(n_pops_dwn)] ~ ddirch(dwn_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_dwn[2, 1:(n_pops_dwn)] ~ ddirch(dwn_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatDwn[1,1:n_pops_dwn] <- zero_vec[1:(n_pops_dwn)] # when not in trib, 0 prob of being in sub areas
  pMatDwn[1,(n_pops_dwn+1)] <- 1 #set the 'not there' bin to prob = 1

  pMatDwn[2,1:n_pops_dwn] <- p_pop_dwn[1,] # when in trib, >0 probs of being in sub areas
  pMatDwn[2,(n_pops_dwn+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatDwn[3,1:n_pops_dwn] <- p_pop_dwn[2,] # when in trib, >0 probs of being in sub areas
  pMatDwn[3,(n_pops_dwn+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_dwn[i] ~ dcat( pMatDwn[(catexp_PRA[i,3] * fishOrigin[i] + 1),1:(n_pops_dwn+1)] ) # the row number acts as on/off switch (2 = above OKL)
  for (j in 1:(n_pops_dwn+1))  { #now expand the dcat into matrix of zeros and ones
  catexp_dwn[i,j] <- equals(a_dwn[i],j) #equals(x,y) is a test for equality, returns [1,0]
  }

  # observation part for below JD1
  BelowPriest[i,1] ~ dbern(BelowJD1_p * catexp_dwn[i,1])

  # observation part for JD1
  BelowPriest[i,2] ~ dbern(JD1B0_p * catexp_dwn[i,2])
  BelowPriest[i,3] ~ dbern(JD1A0_p * catexp_dwn[i,2])

  # observation part for TMF
  BelowPriest[i,4] ~ dbern(TMF_p * catexp_dwn[i,3])

  # observation part for PRV
  BelowPriest[i,5] ~ dbern(PRV_p * catexp_dwn[i,4])

  # observation part for ICH
  BelowPriest[i,10] ~ dbern(ICHB0_p * catexp_dwn[i,5])
  BelowPriest[i,11] ~ dbern(ICHA0_p * catexp_dwn[i,5])

  # observation part for PRO
  BelowPriest[i,12] ~ dbern(PROB0_p * catexp_dwn[i,6])
  BelowPriest[i,13] ~ dbern(PROA0_p * catexp_dwn[i,6])

  # observation part for RSH
  BelowPriest[i,14] ~ dbern(RSHB0_p * catexp_dwn[i,7])
  BelowPriest[i,15] ~ dbern(RSHA0_p * catexp_dwn[i,7])

  # observation part for PRH
  BelowPriest[i,16] ~ dbern(PRHB0_p * catexp_dwn[i,8])
  BelowPriest[i,17] ~ dbern(PRHA0_p * catexp_dwn[i,8])

  } #ends n_fish loop in this section

  ################################################################################
  # Now we deal with Walla Walla
  ################################################################################

  # We use catexp_dwn[i,4] as an on/off switch for presence/absence of fish upstream of PRV
  # There are 3 bins: black box (1), HST (2), MDR ()3

  p_pop_PRV[1, 1:(n_pops_PRV)] ~ ddirch(PRV_dirch_vec[1,]); #Dirichlet for probs for going to bins
  p_pop_PRV[2, 1:(n_pops_PRV)] ~ ddirch(PRV_dirch_vec[2,]); #Dirichlet for probs for going to bins

  # set up a matrix that deals with yes/no in the tributary or not
  pMatPRV[1,1:n_pops_PRV] <- zero_vec[1:(n_pops_PRV)] # when not in trib, 0 prob of being in sub areas
  pMatPRV[1,(n_pops_PRV+1)] <- 1 #set the 'not there' bin to prob = 1

  pMatPRV[2,1:n_pops_PRV] <- p_pop_PRV[1,] # when in trib, >0 probs of being in sub areas
  pMatPRV[2,(n_pops_PRV+1)] <- 0 #set the 'not there' bin to prob = 0
  pMatPRV[3,1:n_pops_PRV] <- p_pop_PRV[2,] # when in trib, >0 probs of being in sub areas
  pMatPRV[3,(n_pops_PRV+1)] <- 0 #set the 'not there' bin to prob = 0

  for (i in 1:n_fish) {
  # the row number acts as switch between rows 1&2 using stochastic node
  a_PRV[i] ~ dcat( pMatPRV[(catexp_dwn[i,4] * fishOrigin[i] + 1), 1:(n_pops_PRV+1)] )
  for (j in 1:(n_pops_PRV+1))  {
  catexp_PRV[i,j] <- equals(a_PRV[i],j)
  }

  # observation part for HST
  BelowPriest[i,6] ~ dbern(HSTB0_p * catexp_PRV[i,2])
  BelowPriest[i,7] ~ dbern(HSTA0_p * catexp_PRV[i,2])

  # observation part for MDR
  BelowPriest[i,8] ~ dbern(MDRB0_p * catexp_PRV[i,3])
  BelowPriest[i,9] ~ dbern(MDRA0_p * catexp_PRV[i,3])

  } #ends n_fish loop in this section

  #end model
  }
"

  # write model as text file
  cat(model_code,
    file = file_name)

  model_file = readLines(file_name)

  writeLines(model_file, file_name)

}
