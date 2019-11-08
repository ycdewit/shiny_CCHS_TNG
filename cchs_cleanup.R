# Generic cleaning - setting NA, removing blank variables, collapsing countable non-responses
cchs_cleanup <-
  function (dataframe) {

    cchs_clean <- dataframe
    
    num_cchs <- dplyr::select_if(cchs_clean, is.numeric)
    
    max_cchs <- dplyr::mutate_all(num_cchs, ~max(., na.rm = TRUE))
    
    max_cchs <- dplyr::distinct(max_cchs)
    max_cchs$id <- 1
    
    max_cchs <- reshape2::melt(max_cchs, id.vars="id")
    
    min_max <- c(96, 996, 9996, 9.996, 999.6, 999.96, 9999.6, 99996)
    max_max <- c(99, 999, 9999, 9.999, 999.9, 999.99, 9999.9, 99999)
    
    min_max[1]
    
    for(i in seq_along(min_max)){
      
      names_needfix <- as.vector(max_cchs[which(max_cchs$value>=min_max[i] & max_cchs$value<=max_max[i]), "variable"])
      
      cchs_clean <- dplyr::mutate_at(cchs_clean, names_needfix, ~ifelse(. >= min_max[i] & . <= max_max[i], NA, .))
    }

    cchs_clean1 <-
      dplyr::mutate_if(
        cchs_clean,is.factor, ~forcats::fct_other(., drop = c('Don\'t know','Refusal','Not stated', 'Valid skip'),
        other_level = "Refused unknown or missing"))
      
    cchs_clean2 <-
      dplyr::na_if(cchs_clean1,"Refused unknown or missing")

    cchs_clean3 <-
      dplyr::mutate_if(cchs_clean2,is.factor,forcats::fct_drop)
    
    cchs_clean4 <-
      dplyr::select(
        cchs_clean3,-dplyr::starts_with("DO"))
    
    cchs_cleanlast <-
      dplyr::select_if(cchs_clean4, ~!all(.=="Valid skip", na.rm = TRUE))

    return(cchs_cleanlast)
  }
