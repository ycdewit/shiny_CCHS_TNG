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

    cchs_cleanlast <-
      dplyr::select_if(cchs_clean6, ~sum(!is.na(.)) > 0)

    cchs_cleanlast$phu_vs_prov <- as.factor(ifelse(cchs_cleanlast$GEODVHR4==phu,"phu","prov"))
    
    cchs_cleanlast$phu_vs_peer <- as.factor(ifelse(cchs_cleanlast$GEODVHR4==phu,"phu",ifelse(
      cchs_cleanlast$GEODVPG==peer,"peer",NA)))
    
    cchs_cleanlast$phu <- as.factor(ifelse(cchs_cleanlast$GEODVHR4==phu,"Yes","No"))

    cchs_cleanlast$peer <- as.factor(ifelse(cchs_cleanlast$GEODVHR4!=phu & cchs_cleanlast$GEODVPG==peer,"Yes","No"))

    cchs_cleanlast$prov <- "Yes"

    return(cchs_cleanlast)
  }
