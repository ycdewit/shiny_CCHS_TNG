# Generic cleaning - setting NA, removing blank variables, collapsing countable non-responses
cchs_cleanup <-
  function (dataframe, phu, peer) {
    phu <- rlang::sym(phu)
    peer <- rlang::sym(peer)

    cchs_clean <- dataframe

    cchs_clean1 <- dplyr::na_if(cchs_clean, 997)
    cchs_clean2 <- dplyr::na_if(cchs_clean1, 998)
    cchs_clean3 <- dplyr::na_if(cchs_clean2, 999)

    cchs_clean4 <-
      dplyr::mutate_if(
        cchs_clean3,is.factor, ~forcats::fct_other(., drop = c('Don\'t know','Refusal','Not stated'),
        other_level = "Refused unknown or missing"))
      
    cchs_clean5 <-
      dplyr::na_if(cchs_clean4,"Refused unknown or missing")

    cchs_clean6 <-
      dplyr::mutate_if(cchs_clean5,is.factor,forcats::fct_drop)
    
    cchs_clean7 <-
      dplyr::select(
        cchs_clean6,-starts_with("ACC"), -starts_with("ADL"), -starts_with("BPC"), -starts_with("CCS"), -starts_with("CIH"), -starts_with("CPG"), 
        -starts_with("DEN"), -starts_with("DRV"), -starts_with("EYX"), -starts_with("FDC"), -starts_with("FGU"), -starts_with("FSC"), 
        -starts_with("HUI"), -starts_with("INJ"), -starts_with("LOP"), -starts_with("MAM"), -starts_with("MED"), -starts_with("MXA"), 
        -starts_with("MXS"), -starts_with("NDE"), -starts_with("OHT"), -starts_with("PAP"), -starts_with("PCU"), -starts_with("PSA"), 
        -starts_with("PSC"), -starts_with("SCA"), -starts_with("SCH"), -starts_with("SPI"), -starts_with("SPS"), -starts_with("STS"), 
        -starts_with("SWL"), -starts_with("UCN"), -starts_with("UPE"), -starts_with("WST"), -starts_with("WTM")
               
        )
    #-DOACC, -DOADL, -DOBPC, -DOCCS, -DOCIH, -DOCPG, -DODEN, -DODRV, -DOEYX, -DOFDC, -DOFGU, -DOFSC, -DOHUI, -DOINJ, -DOLOP, -DOMAM, -DOMED, -DOMXA, -DOMXS, -DONDE, -DOOHT, -DOPAP, -DOPCU, -DOPSA, -DOPSC, -DOSCA, -DOSCH, -DOSPI, -DOSPS, -DOSTS, -DOSWL, -DOUCN, -DOUPE, -DOWST, -DOWTM, 
    # -c("CCC_150", "CCC_155", "CCC_160", "CCC_165", "CCC_170", "CCC_175A", "CCC_175B", "CCC_175C", "CCC_175D", "CCC_175E", "CCC_175F", "CCC_175G", "CCC_175H", "CCC_180") 

    cchs_cleanlast <-
      dplyr::select_if(cchs_clean7, ~sum(!is.na(.)) > 0)

    cchs_cleanlast$phu_vs_prov <- as.factor(ifelse(cchs_cleanlast$GEODVHR4==phu,"phu","prov"))
    
    cchs_cleanlast$phu_vs_peer <- as.factor(ifelse(cchs_cleanlast$GEODVHR4==phu,"phu",ifelse(
      cchs_cleanlast$GEODVPG==peer,"peer",NA)))
    
    cchs_cleanlast$phu <- as.factor(ifelse(cchs_cleanlast$GEODVHR4==phu,"Yes","No"))

    cchs_cleanlast$peer <- as.factor(ifelse(cchs_cleanlast$GEODVHR4!=phu & cchs_cleanlast$GEODVPG==peer,"Yes","No"))

    cchs_cleanlast$prov <- "Yes"

    return(cchs_cleanlast)
  }
