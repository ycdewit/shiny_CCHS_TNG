cchs_table <- function(dataframe, svy_design_phu, svy_design_peer, svy_design_prov, questions, 
                       by_vars=NULL, crude=TRUE, standardize=FALSE, 
                       stand_var=NULL, stand_data=NULL, stand_pop=NULL, update_progress=NULL){
  
  qnum <- 0
  
  geo_vars <- c("phu", "peer", "prov")
  
  master_list <- lapply(setNames(geo_vars, geo_vars), function(geo_var) {
    
    df_filtered <- 
      dataframe[which(dataframe[[geo_var]] == "Yes"),]
    
    if(geo_var=="phu") {
      cchs_survey <- svy_design_phu
    }
    
    else if(geo_var=="peer") {
      cchs_survey <- svy_design_peer
    }
    
    else cchs_survey <- svy_design_prov
    
    cchs_survey[["variables"]] <- dplyr::mutate_if(cchs_survey[["variables"]], is.factor, ~forcats::fct_explicit_na(.))
    
    output_list <- lapply(setNames(questions,questions), function(question) {
      
      qnum <- qnum + 1
      
      if (is.function(update_progress)) {
        update_progress(detail = paste0("Running ", geo_var, " estimates for question ", qnum, ": ", question, "."))
      }
      
      if (!question %in% names(cchs_survey[["variables"]]))
      
      question <- rlang::sym(question)
      
      if(is.numeric(cchs_survey[["variables"]][[as.name(question)]])){
        cchs_survey <- subset(cchs_survey, !is.na(ALWDVWKY))
        n_sample <- length(which(!is.na(df_filtered[[as.name(question)]])))
        raw_n <- data.frame("Mean", n_sample)
        names(raw_n) <- c("ind_level", "Sample Size")
      }
      
      else{
        raw_n <- as.data.frame(table(df_filtered[[as.name(question)]]))
        names(raw_n) <- c("ind_level", "Sample Size")
        raw_n <- raw_n[which(raw_n$ind_level!="Valid skip"),]
      }
      
      if(!is.null(by_vars)){
        
        by_n <- lapply(setNames(by_vars, by_vars), function(by_col) {
          if(is.numeric(cchs_survey[["variables"]][[as.name(question)]])){
            freqdf <- as.data.frame(table(df_filtered[which(!is.na(df_filtered[[as.name(question)]])), by_col]))
            names(freqdf) <- c("strat_level", "Sample Size")
            freqdf <- cbind(stratifier=by_col, ind_level="Mean", freqdf)
          }
          else if(!is.factor(df_filtered[[as.name(by_col)]])){
            df_filtered[[as.name(by_col)]] <- as.factor(df_filtered[[as.name(by_col)]])
          }
          else{
            freqdf <- as.data.frame(table(df_filtered[[as.name(question)]],df_filtered[[as.name(by_col)]]))
            names(freqdf) <- c("ind_level", "strat_level", "Sample Size")
            freqdf <- cbind(stratifier=by_col, freqdf)
          }
          return(freqdf)
        })
        
        by_n <- dplyr::bind_rows(by_n)
        
        by_n$stratifier <- as.factor(by_n$stratifier)
        by_n$strat_level <- as.factor(by_n$strat_level)
        by_n <- by_n[which(by_n$ind_level!="Valid skip" & by_n$strat_level!="Valid skip"),]
      }
      
      if (crude == TRUE) {
        
        out_crude  <- cchs_est(svy_design=cchs_survey, question=question)
        
        out_crude$est_type <- "crude"
        
        output <-
          dplyr::left_join(raw_n, out_crude)
        
        if(!is.null(by_vars)){
          out_crude_by <- cchs_estby(svy_design=cchs_survey, by_vars, question)
          output_crude_by <- dplyr::left_join(by_n, out_crude_by)
          output_crude_by$est_type <- "crude"
          output <- dplyr::bind_rows(output, output_crude_by)
        }
      }
      
      if(standardize == TRUE) {
        
        if(is.null(stand_data)|is.null(stand_pop)|is.null(stand_var)){
          stop("If standardize=TRUE then stand_data, stand_pop, and stand_var arguments must be specified")}
        
        base <- data.frame(Var1=levels(dataframe[[stand_var]]))
        base[[as.name(stand_var)]] <- as.factor(base$Var1)
        
        std_pop_data <- dplyr::left_join(base, stand_data)
        
        std_svy_design <- 
          survey::svystandardize(
            design = cchs_survey,
            by = as.formula(paste0("~ ",stand_var)),
            over = ~ 1,
            population = stand_data[[stand_pop]])
        
        std_est <- cchs_est(std_svy_design, question)
        std_est$est_type <- "standardized"
        
        out_stand <- dplyr::left_join(raw_n, std_est)
        
        if(!exists("output")) {
          output <- out_stand
        }
        
        else output <- dplyr::bind_rows(output, out_stand)
        
        if(!is.null(by_vars)){
          out_std_by <- cchs_estby(std_svy_design, by_vars, question)
          output_std_by <- dplyr::left_join(by_n, out_std_by)
          output_std_by$est_type <- c("standardized")
          output <- dplyr::bind_rows(output, output_std_by)
        } 
      }
      output$indicator <- paste0(question)
      clean_output <- dplyr::mutate_at(output, dplyr::vars(Estimate, `Lower 95% CI`, `Upper 95% CI`),
                                       list(~ifelse(`Quality Indicator` == "NR" | `Sample Size` < 10, NA, .)))
      return(clean_output)
    })
    to_masterlist <- dplyr::bind_rows(output_list)
    to_masterlist$geo_area <- paste0(geo_var)
    return(to_masterlist)
  })
  output <- dplyr::bind_rows(master_list)
}