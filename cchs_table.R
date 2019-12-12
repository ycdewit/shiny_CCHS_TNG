## TO DO:
## 1. Add documentation
## 2. Create 2 versions - one for package, one for app

cchs_table <- function(dataframe, svy_design_phu, svy_design_peer, svy_design_prov, questions, 
                       by_vars=NULL, crude=TRUE, standardize=FALSE, 
                       stand_var=NULL, stand_data=NULL, stand_pop=NULL, update_progress=NULL){
  
  qnum <- 0
  
  geo_vars <- c("phu", "peer", "prov")
  
  master_list <- lapply(setNames(geo_vars, geo_vars), function(geo_var) {
    
    print(paste0(geo_var))

    
    if(geo_var=="phu") {
      survey <- svy_design_phu
      geo_data <- dplyr::filter(dataframe, phu=="Yes")
    }
    
    else if(geo_var=="peer") {
      survey <- svy_design_peer
      geo_data <- dplyr::filter(dataframe, peer=="Yes")
    }
    
    else {
      survey <- svy_design_prov
      geo_data <- dataframe
      }
    
    
    output_list <- lapply(setNames(questions,questions), function(question) {
      
      print(paste0(question))
      
      qnum <- qnum + 1
      
      if (is.function(update_progress)) {
        update_progress(detail = paste0("Running ", geo_var, " estimates for question ", qnum, ": ", question, "."))
      }
      
      if (!(question %in% names(survey[["variables"]]))) {
        stop(paste0(question, "must be a variable in the survey design."))
      }
      
      question <- rlang::sym(question)
      
      cchs_survey <- subset(survey, !is.na(eval(question)))
      
      if(is.numeric(cchs_survey[["variables"]][[as.name(question)]])){
        n_sample <- length(which(!is.na(geo_data[[as.name(question)]])))
        raw_n <- data.frame("Mean", n_sample)
        names(raw_n) <- c("ind_level", "Sample Size")
      }
      
      else{
        raw_n <- as.data.frame(table(geo_data[[as.name(question)]]))
        names(raw_n) <- c("ind_level", "Sample Size")
        raw_n <- raw_n[which(raw_n$ind_level!="Valid skip"),]
      }
      
      if(!is.null(by_vars)){
        
        by_n <- lapply(setNames(by_vars, by_vars), function(by_col) {
          if(is.numeric(cchs_survey[["variables"]][[as.name(question)]])){
            freqdf <- as.data.frame(table(geo_data[which(!is.na(geo_data[[as.name(question)]])), by_col]))
            names(freqdf) <- c("strat_level", "Sample Size")
            freqdf <- cbind(stratifier=by_col, ind_level="Mean", freqdf)
          }
          else if(!is.factor(geo_data[[as.name(by_col)]])){
            geo_data[[as.name(by_col)]] <- as.factor(geo_data[[as.name(by_col)]])
          }
          else{
            freqdf <- as.data.frame(table(geo_data[[as.name(question)]],geo_data[[as.name(by_col)]]))
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
        
        base <- data.frame(Var1=levels(geo_data[[stand_var]]))
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
  
  if(is.null(by_vars)){
    output_long <- reshape2::melt(output, id.vars=c("indicator","ind_level", "geo_area", "est_type"))
    output_final <- reshape2::dcast(output_long, indicator + ind_level + est_type ~ geo_area + variable, value.var="value")
  }
  else{
    output_long <- reshape2::melt(output, id.vars=c("indicator","ind_level", "stratifier", "strat_level", "geo_area", "est_type"))
    output_final <- reshape2::dcast(output_long, indicator + ind_level + stratifier + strat_level + est_type ~ geo_area + variable, value.var="value")
  }
  return(output_final)
}