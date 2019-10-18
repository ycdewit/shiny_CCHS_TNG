# Generic cleaning - setting NA, removing blank variables, collapsing countable non-responses
cchs_cleanup <-
  function (dataframe) {

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
        cchs_clean6,-starts_with("DO"))
    
    cchs_cleanlast <-
      dplyr::select_if(cchs_clean7, ~!all(.=="Valid skip", na.rm = TRUE))

    return(cchs_cleanlast)
  }
