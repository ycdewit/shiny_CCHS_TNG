cchs_ratio <- function(question, response=NULL, dataframe, compareby, standardize = FALSE, 
                       stand_var=NULL, stand_data=NULL, stand_pop=NULL, universe=NULL) {
  
  question <- as.symbol(question)
  
  if(!is.null(response)){
    response <- as.symbol(response)
  }
  
  startdata <- 
    dplyr::mutate(
      dataframe,
      compref = as.factor(ifelse(
        is.numeric(!! question), "mean", ifelse(
          is.null(response), ifelse(is.factor(!! question), levels(!! question)[1], ifelse(
            is.character(!! question), dplyr::first(!! question), "error")), rlang::as_name(response)
        )
      )),
      compquestion = ifelse(compref=="mean", !! question, ifelse(!! question == as.character(compref), 1, 0))
    )
  
  quest_ref <- levels(startdata$compref)
  
  if(any(startdata$compref=="error")) stop("Question must be numeric, factor, or character column in specified dataframe")
  
  for (i_ in seq_along(compareby)){
    
    if(!is.null(names(compareby[i_]))){
      comp <- as.symbol(names(compareby[i_]))
      ref <- as.symbol(compareby[i_])
    }
    
    else{
      comp <- as.symbol(compareby[i_])
      ref <- as.symbol(levels(dplyr::pull(startdata,!! comp)))
    }
    
    dataframe1 <- dplyr::mutate(
      startdata, comparevar = !! comp)
    
    dataframe1$comparevar <- stats::relevel(dataframe1$comparevar, ref=rlang::as_string(ref))
    
    df_filtered <- dplyr::filter(dataframe1,!is.na(compquestion) & !is.na(comparevar))
    
    if(!is.null(universe)){
      as.symbol(universe)
      df_filtered <- dplyr::filter(df_filtered, !! universe==1)
    }
    
    svy_design <- setup_design(in_data=df_filtered)
    
    glm <- survey::svyglm(compquestion ~ comparevar, design=svy_design, family=quasipoisson())
    
    glmtable <- as.data.frame(summary(glm)[["coefficients"]])
    
    glmtable$RR <- exp(glmtable$Estimate)
    glmtable$L95CI <- exp(glmtable$Estimate - 1.96*glmtable$`Std. Err`)
    glmtable$U95CI <- exp(glmtable$Estimate + 1.96*glmtable$`Std. Err`)
    glmtable$name <- rownames(glmtable)
    glmtable$comparator <- stringr::str_replace(glmtable$name,"comparevar","")
    
    df <- dplyr::filter(glmtable,name!="(Intercept)")
    
    df1 <- dplyr::select(df,c("RR","L95CI","U95CI","comparator"))
    
    df1$reference <- rlang::as_name(ref)
    df1$indicator <- rlang::as_name(question)
    df1$ind_level <- quest_ref
    df1$sig <- ifelse(df1$U95CI < 1, "lower", ifelse(df1$L95CI > 1, "higher", "none"))
    
    if(standardize==TRUE) {
      
      if(is.null(stand_data)|is.null(stand_pop)|is.null(stand_var)){
        stop("If standardize=TRUE then stand_data, stand_pop, and stand_var arguments must be specified")}
      
      else{
        stand_pop <- as.symbol(stand_pop)   
        
        stand_var <- as.symbol(stand_var)
        
        base <- dplyr::count(df_filtered, !!stand_var)
        std_pop_data <- dplyr::left_join(base,stand_data)
        
        std_svy_design <- 
          survey::svystandardize(
            design = svy_design,
            by = as.formula(paste0("~ ", rlang::as_name(stand_var))),
            over = ~ 1,
            population = dplyr::pull(std_pop_data,var = !! stand_pop)
          )
        std_glm <- survey::svyglm(compquestion ~ comparevar, design=std_svy_design, family=quasipoisson())
        
        std_glmtable <- as.data.frame(summary(glm)$coefficients)
        
        std_glmtable$std_RR <- exp(glmtable$Estimate)
        std_glmtable$std_L95CI <- exp(glmtable$Estimate - 1.96*glmtable$`Std. Err`)
        std_glmtable$std_U95CI <- exp(glmtable$Estimate + 1.96*glmtable$`Std. Err`)
        std_glmtable$name <- rownames(std_glmtable)
        std_glmtable$comparator <- stringr::str_replace(std_glmtable$name,"comparevar","")
        
        std_df <- dplyr::filter(std_glmtable,name!="(Intercept)")
        
        df2 <- dplyr::select(std_df,c("std_RR","std_L95CI","std_U95CI","comparator"))
        
        df2$reference <- rlang::quo_name(ref)
        df2$indicator <- rlang::quo_name(question)
        df2$ind_level <- quest_ref
        df2$std_sig <- ifelse(df2$std_U95CI < 1, "lower", ifelse(df2$std_L95CI > 1, "higher", "none"))
        
        df1 <- dplyr::left_join(df1,df2)
      }
    }
    
    if (i_ == 1) {
      output <- df1
    }
    else {
      output <- dplyr::bind_rows(output, df1)
    }
  }

  return(output)
  
}

cchs_ratioby <- function(question, response, dataframe, compareby, groupby = NULL, standardize = FALSE, stand_var = NULL, 
                         stand_data = NULL, stand_pop = NULL, universe = NULL){
  
  for (j_ in seq_along(groupby)){
    
    stratifier <- groupby[j_]
    
    strat_levels <- levels(dataframe[[stratifier]])
    
    for (k_ in seq_along(strat_levels)){
      
      stratifier <- as.symbol(stratifier)
      strat_level <- strat_levels[k_]
      
      df <- dplyr::filter(dataframe, !! stratifier == strat_level)
      
      ratio <- cchs_ratio(question = question, response = response, dataframe = df, compareby = compareby, standardize = standardize, 
                          stand_data = stand_data, stand_pop = stand_pop, stand_var = stand_var, universe = universe)
      
      ratio$strat_level <- strat_level
      
      if (k_ == 1) {output <- ratio}
      else {output <- dplyr::bind_rows(output, ratio)}
      
      output$stratifier <- as.character(stratifier)
    }
    
    if (j_ == 1) {output1 <- output}
    else {output1 <- dplyr::bind_rows(output1, output)}
  }
  
  return(output1)
}

cchs_rr <- function(questions, dataframe, compareby, groupby = NULL, standardize = FALSE, stand_var = NULL, 
                    stand_data = NULL, stand_pop = NULL, universe = NULL) {
  
  if(!is.null(names(questions))){
    responses <- questions
    questions <- names(questions)
  }
  
  else{responses <- NULL}
  
  for (i in seq_along(questions)) {
    
    ratio <- cchs_ratio(question = questions[i], response = responses[i], dataframe = dataframe, compareby = compareby, 
               standardize = standardize, stand_data = stand_data, stand_pop = stand_pop, stand_var = stand_var, universe = universe)
    
    if (!is.null(groupby)) {
      
      ratio$strat_level <- "Overall"
      ratio$stratifier <- "None"
      
      ratioby <- cchs_ratioby(question = questions[i], response = responses[i], dataframe = dataframe, compareby = compareby, groupby = groupby,
                              standardize = standardize, stand_data = stand_data, stand_pop = stand_pop, stand_var = stand_var, universe = universe)
      
      ratio <- dplyr::bind_rows(ratio, ratioby)
      
    }
    
    if(i == 1) {
      output <- ratio
    }
    else{
      output <- dplyr::bind_rows(output, ratio)
    }
  }
  return(output)
}
      