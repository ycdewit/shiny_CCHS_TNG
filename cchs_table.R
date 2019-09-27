cchs_est <- function(question, dataframe, geo_vars, standardize=FALSE, stand_var=NULL, stand_data=NULL, stand_pop=NULL, universe=NULL){
  
  output_list <- lapply(setNames(geo_vars,geo_vars), function(geo_var) {
    
    geo_var <- rlang::sym(geo_var)
  
    question <- rlang::sym(question)
    
    if(!is.null(universe)){
      universe <- as.symbol(universe)
      dataframe <- dplyr::filter(dataframe, !!universe == 1)
    }
    
    df_geofiltered <- 
      dplyr::filter(dataframe, !! geo_var == "Yes" & !(is.na(!! question)))
    
    df_geofiltered <- dplyr::select(df_geofiltered, !! question, !! geo_var, !! stand_var, FWGT, dplyr::starts_with("BSW"))
      
    svy_design <- setup_design(in_data=df_geofiltered)
    
    if(is.numeric(df_geofiltered[[rlang::quo_name(question)]])){
      raw_n <- dplyr::count(df_geofiltered)
    }
    
    else{
    raw_n <- dplyr::count(df_geofiltered, !! question)
    }
     
    wgt_est <- survey::svymean(as.formula(paste0("~", rlang::quo_name(question))), svy_design, na.rm = TRUE)
  
    combine <- 
      dplyr::bind_cols(
        raw_n,
        as.data.frame(wgt_est),
        as.data.frame(survey::cv(wgt_est), optional=TRUE),
        as.data.frame(stats::confint(wgt_est)))
    
    cleanup <- 
      dplyr::rename(combine,`Sample Size`  =  "n", Estimate = "mean", CV = "V1", `Lower 95% CI` = "2.5 %",
                    `Upper 95% CI` = "97.5 %")
    
    cleanup1 <- dplyr::select(cleanup,-SE)
    
    if(is.numeric(df_geofiltered[[rlang::quo_name(question)]])){
      cleanup2 <- dplyr::mutate(cleanup1, CV=CV*100)
    }
    else {
      cleanup2 <- dplyr::mutate_at(cleanup1, dplyr::vars(Estimate, `Lower 95% CI`, `Upper 95% CI`, CV), scale100)
    }
    
    cleanup2$`Quality Indicator` <-
      as.factor(
        ifelse(
          cleanup2$CV <= 15,
          "",
          ifelse(
            cleanup2$CV <= 25,
            "C",
            ifelse(
              cleanup2$CV <= 35,
              "D",
              "NR"
            )
          )
        )
      )
    
    cleanup2$indicator <- rlang::quo_name(question)
    
    cleanup2$geo_area <- rlang::quo_name(geo_var)
    
    more_cleaning <- 
      dplyr::mutate_at(cleanup2, dplyr::vars(Estimate, `Lower 95% CI`, `Upper 95% CI`, CV),
                       list(~ifelse(`Quality Indicator` == "NR" | `Sample Size` < 10, NA, .)))
    
    if(standardize == TRUE){
    
      if(is.null(stand_data)|is.null(stand_pop)|is.null(stand_var)){
        stop("If standardize=TRUE then stand_data, stand_pop, and stand_var arguments must be specified")}
      
      else{
        
        stand_pop <- as.symbol(stand_pop)   
        
        stand_var <- as.symbol(stand_var)
        
        base <- dplyr::count(df_geofiltered, !!stand_var)
        std_pop_data <- dplyr::left_join(base, stand_data)
  
        std_svy_design <- 
          survey::svystandardize(
            design = svy_design,
            by = as.formula(paste0("~ ",as.name(stand_var))),
            over = ~ 1,
            population = dplyr::pull(std_pop_data,var = !! stand_pop))
        
        std_est <-
          survey::svymean(as.formula(paste0("~", rlang::quo_name(question))), design = std_svy_design, na.rm = TRUE)
        
        std_combine <- 
          dplyr::bind_cols(
            as.data.frame(std_est),
            as.data.frame(survey::cv(std_est), optional=TRUE),
            as.data.frame(stats::confint(std_est)))
        
        std_cleanup <- 
          dplyr::rename(std_combine, `Std Estimate` = "mean", `Std CV` = "V1", `Std Lower 95% CI` = "2.5 %",
                        `Std Upper 95% CI` = "97.5 %")
        
        std_cleanup1 <- dplyr::select(std_cleanup, -SE)
        
        if(is.numeric(df_geofiltered[[rlang::quo_name(question)]])){
          std_cleanup2 <- dplyr::mutate(std_cleanup1, 
                                        CV=CV*100,
                                        ind_level="mean")
        }
        else {
          std_cleanup2 <- dplyr::mutate_at(std_cleanup1, dplyr::vars(`Std Estimate`, `Std Lower 95% CI`, `Std Upper 95% CI`, `Std CV`), scale100)
        }
        
        std_comb <- dplyr::bind_cols(more_cleaning, std_cleanup2)
        
        cleaning <- 
          dplyr::mutate_at(std_comb, dplyr::vars(`Std Estimate`, `Std Lower 95% CI`, `Std Upper 95% CI`, `Std CV`),
                           list(~ifelse(`Quality Indicator` == "NR" | `Sample Size` < 10, NA, .)))
        
        more_cleaning <- dplyr::mutate(
          cleaning, 
          `Std Quality Indicator`=`Quality Indicator`,
          `Std Sample Size`=`Sample Size`)
      }
    }
  
    if(is.numeric(df_geofiltered[[rlang::quo_name(question)]])){
      output <- reshape2::melt(more_cleaning, id.vars = c("indicator", "geo_area","ind_level"))
    }
    
    else{
    
      more_cleaning1 <- 
        dplyr::rename(more_cleaning, ind_level = rlang::quo_name(question))
      
      skinny <- reshape2::melt(more_cleaning1, id.vars = c("indicator", "ind_level", "geo_area"))
      
      output <- dplyr::filter(skinny, skinny$ind_level != "zno")
    
    }
    return(output)
  })
  output_combined <- as.data.frame(dplyr::bind_rows(output_list))
}

cchs_estby <- function(question, dataframe, geo_vars, by_vars, standardize=FALSE, stand_var=NULL, stand_data=NULL, stand_pop=NULL, universe=NULL) {
  
  for (j in seq_along(geo_vars)) {
  
    geo_var <- rlang::sym(geo_vars[j])
    question <- rlang::sym(question)
    
    for (i in seq_along(by_vars)) {
      by_var <- rlang::sym(by_vars[i])
      
      if(!is.null(universe)){
        universe <- as.symbol(universe)
        dataframe <- dplyr::filter(dataframe, !!universe == 1)
      }
      
      df_geofiltered <- 
        dplyr::filter(dataframe, (!! geo_var) == "Yes" & !(is.na(!! question)) & !(is.na(!! by_var)))
      
      df_geofiltered <- dplyr::select(df_geofiltered, !! question, !! geo_var, !! by_var, !! stand_var, FWGT, dplyr::starts_with("BSW"))
        
      svy_design <- setup_design(in_data=df_geofiltered)
      
      if(is.numeric(df_geofiltered[[rlang::quo_name(question)]])){
        raw_n <- dplyr::count(df_geofiltered, !! by_var)
      }
      
      else{
        skinny_n <- dplyr::count(df_geofiltered, !! question, !! by_var)
        wide_n <- tidyr::spread(skinny_n, !! question, n)
        raw_n <- dplyr::rename_at(wide_n, dplyr::vars(-!! by_var), paste0, "_n")
      }
      
      wgt_est <- 
        as.data.frame(
          survey::svyby(as.formula(paste0("~", question)),as.formula(paste0("~", by_var)), svy_design, survey::svymean, vartype = c("ci","cv")))
      
      if(is.numeric(df_geofiltered[[rlang::quo_name(question)]])){
        wgt_est_clean <- dplyr::mutate_at(wgt_est, dplyr::vars(dplyr::contains("cv")), scale100)
      }
      else {
        wgt_est_clean <- dplyr::mutate_at(wgt_est, dplyr::vars(-!! by_var), scale100)
      }
      
      joined <- dplyr::full_join(raw_n, wgt_est_clean, by = rlang::quo_name(by_var))
      
      joined1 <- dplyr::rename(joined, strat_level = rlang::quo_name(by_var))
      
      joined2 <- dplyr::mutate_at(joined1, dplyr::vars(dplyr::contains("cv")), .funs = list(qual = ~ifelse(
        .<=15,"", ifelse(
          .<=25,"C", ifelse(
            .<=35,"D","NR")))))
      
      joined3 <- dplyr::rename_at(joined2, dplyr::vars(dplyr::contains(rlang::quo_name(question))),stringr::str_replace,rlang::quo_name(question),"est_")
      
      joined4 <- dplyr::rename_at(joined3, dplyr::vars(dplyr::contains("_qual")), stringr::str_replace, "cv.", "")
      
      joined5 <- dplyr::select(joined4,-dplyr::contains("zno"))
   
      if(standardize==TRUE) {
        
        if(is.null(stand_data)|is.null(stand_pop)|is.null(stand_var)){
          stop("If standardize=TRUE then stand_data, stand_pop, and stand_var arguments must be specified")}
        
        else{
        
          stand_pop <- as.symbol(stand_pop) 
          
          stand_var <- as.symbol(stand_var)
          
          base <- dplyr::count(df_geofiltered, !!stand_var)
          std_pop_data <- dplyr::left_join(base,stand_data)
          
          std_svy_design <- 
            survey::svystandardize(
              design = svy_design,
              by = as.formula(paste0("~ ",stand_var)),
              over = ~ 1,
              population = dplyr::pull(std_pop_data,var = !! stand_pop))
        
        std_est <- as.data.frame(
          survey::svyby(as.formula(paste0("~", question)),as.formula(paste0("~", by_var)), std_svy_design, survey::svymean, vartype = c("ci","cv")))
        
        if(is.numeric(df_geofiltered[[rlang::quo_name(question)]])){
          std_est_clean <- dplyr::mutate_at(std_est, dplyr::vars(dplyr::contains("cv")), scale100)
        }
        
        else {
          std_est_clean <- dplyr::mutate_at(std_est, dplyr::vars(-!! by_var), scale100)
        }
        
        joined_std <- dplyr::rename(std_est_clean, strat_level = rlang::quo_name(by_var))
        
        joined_std2 <- dplyr::rename_at(joined_std, dplyr::vars(dplyr::contains(rlang::quo_name(question))),stringr::str_replace,rlang::quo_name(question),"est_")
        
        joined_std3 <- dplyr::rename_at(joined_std2, dplyr::vars(dplyr::contains("_qual")), stringr::str_replace, "cv.", "")
        
        joined_std4 <- dplyr::select(joined_std3,-dplyr::contains("zno"))
        
        joined_std5 <- dplyr::rename_all(joined_std4, paste0, "_std")
        
        joined_std6 <- dplyr::select(joined_std5, -strat_level_std)
        
        joined5 <- dplyr::bind_cols(joined5, joined_std6)
      }
    }  
    joined5$stratifier <- rlang::quo_name(by_var)
    joined5$geo_area <- rlang::quo_name(geo_var)
      
    if (i==1 & j==1) {
      output <- joined5
    }
    
    else{
      output <- dplyr::bind_rows(output, joined5)
    }
  
  clean <- reshape2::melt(output, id.vars = c("stratifier","strat_level","geo_area"))
  
  clean$indicator <- rlang::quo_name(question)
  
  clean$ind_level <- stringr::str_replace_all(clean$variable,c("ci_l." = "","ci_u." = "","cv." = "","_n" = "","est_" = "","_qual" = ""))
  
  if(standardize == TRUE){
    
  clean$ind_level <- stringr::str_replace_all(clean$ind_level, c("_std"=""))
  
  clean$std <- ifelse(stringr::str_detect(clean$variable, "_std"),"Std","Crude")
  }
  
  clean$measure <-
    as.factor(
      ifelse(stringr::str_detect(clean$variable,"ci_l"), "Lower 95% CI", ifelse(
        stringr::str_detect(clean$variable,"ci_u"), "Upper 95% CI", ifelse(
          stringr::str_detect(clean$variable,"qual"), "Quality Indicator", ifelse(
            stringr::str_detect(clean$variable,"cv"), "CV", ifelse(
              stringr::str_detect(clean$variable,"_n"),"Sample Size","Estimate")
          )
        )
      )
      )
    )
  
  clean1 <- dplyr::select(clean, -variable)

  if(standardize == TRUE){
    wide <- reshape2::dcast(clean1, indicator + stratifier + strat_level + ind_level + geo_area ~ measure + std)
    cleaning <- dplyr::mutate_at(wide, dplyr::vars(`Estimate_Crude`, `Lower 95% CI_Crude`, `Upper 95% CI_Crude`, `Estimate_Std`, `Lower 95% CI_Std`, `Upper 95% CI_Std`),
                                     list(~ifelse(`Quality Indicator_Crude` == "NR" | `Sample Size_Crude` < 10, NA, .)))
    clean_output <- 
      dplyr::mutate(
        cleaning, 
        `Quality Indicator_Std`=`Quality Indicator_Crude`,
        `Sample Size_Std`=`Sample Size_Crude`)
  }
  
  else{
    wide <- reshape2::dcast(clean1, indicator + stratifier + strat_level + ind_level + geo_area ~ measure)
    clean_output <- dplyr::mutate_at(wide, dplyr::vars(Estimate, `Lower 95% CI`, `Upper 95% CI`),
                            list(~ifelse(`Quality Indicator` == "NR" | `Sample Size` < 10, NA, .)))
  }
  skinny_output <- reshape2::melt(clean_output, id.vars = c("indicator", "ind_level", "stratifier", "strat_level", "geo_area"))
    }
  }
  return(skinny_output)
}

cchs_table <- function(questions, dataframe, geo_vars, by_vars="none", by_compare=FALSE, standardize=FALSE, stand_var=NULL, stand_data=NULL, stand_pop=NULL, universe=NULL){
  
  outputlist <- lapply(setNames(questions, questions), function(question) {
    
    output <- cchs_est(question = question, dataframe = dataframe, geo_vars = geo_vars, standardize = standardize, stand_data = stand_data, stand_pop = stand_pop, stand_var = stand_var, universe = universe)
    
    if (!by_vars == "none") {
      outputby <- cchs_estby(question = question, dataframe = dataframe, geo_vars = geo_vars, by_vars = by_vars, standardize = standardize, stand_data = stand_data, stand_pop = stand_pop, stand_var = stand_var, universe=universe)  
      output_comb <- dplyr::bind_rows(output, outputby)
    }
  })
}
  

