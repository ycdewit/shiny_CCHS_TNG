## TO DO:
## 1. Add documentation
## 2. Create 2 versions - one for package, one for app

cchs_table <- function(svy_design, questions, by_vars=NULL, crude=TRUE, standardize=FALSE, 
                       stand_var=NULL, stand_data=NULL, stand_pop=NULL, update_progress=NULL){
  #delete for package
  qnum <- 0
    
    output_list <- lapply(setNames(questions,questions), function(question) {
      #delete for pacakage
      qnum <- qnum + 1
      
      #delete for package
      if (is.function(update_progress)) {
        update_progress(detail = paste0("Running estimates for question ", qnum, ": ", question, "."))
      }
      
      question <- rlang::sym(question)
      
      if (!question %in% names(svy_design[["variables"]])){
        stop(paste0(question, "must be a variable in the survey design"))
      }
      
      cchs_survey <- subset(svy_design, !is.na(question))
      
      if(is.numeric(cchs_survey[["variables"]][[as.name(question)]])){
        n_sample <- length(cchs_survey[["variables"]][[as.name(question)]])
        raw_n <- data.frame("Mean", n_sample)
        names(raw_n) <- c("ind_level", "Sample Size")
      }
      
      else if(!is.factor(cchs_survey[["variables"]][[as.name(question)]])){
        cchs_survey[["variables"]][[as.name(question)]] <- as.factor(cchs_survey[["variables"]][[as.name(question)]])
      }
      
      else{
        raw_n <- as.data.frame(table(cchs_survey[["variables"]][[as.name(question)]]))
        names(raw_n) <- c("ind_level", "Sample Size")
        raw_n <- raw_n[which(raw_n$ind_level!="Valid skip"),]
      }
      
      if(!is.null(by_vars)){
        
        by_n <- lapply(setNames(by_vars, by_vars), function(by_col) {
          
          if(!is.factor(cchs_survey[["variables"]][[as.name(by_col)]])){
            cchs_survey[["variables"]][[as.name(by_col)]] <- as.factor(cchs_survey[["variables"]][[as.name(by_col)]])
          }
          
          cchs_bysurvey <- subset(cchs_survey, !is.na(by_col))
          
          if(is.numeric(cchs_byurvey[["variables"]][[as.name(question)]])){
            freqdf <- as.data.frame(table(cchs_surveyby[["variables"]][[as.name(by_col)]]))
            names(freqdf) <- c("strat_level", "Sample Size")
            freqdf <- cbind(stratifier=by_col, ind_level="Mean", freqdf)
          }
          else{
            freqdf <- as.data.frame(table(cchs_survey[["variables"]][[as.name(question)]],cchs_survey[["variables"]][[as.name(by_col)]]))
            names(freqdf) <- c("ind_level", "strat_level", "Sample Size")
            freqdf <- cbind(stratifier=by_col, freqdf)
          }
          return(freqdf)
        })
        
        by_n <- dplyr::bind_rows(by_n)
        
        by_n$stratifier <- as.factor(by_n$stratifier)
        by_n$strat_level <- as.factor(by_n$strat_level)
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
                                      list(~ifelse(`Sample Size` < 10 | CV>35, NA, .)))
      return(clean_output)
    })
  output <- dplyr::bind_rows(clean_output)
  return(output)
}

cchs_table_multi <- function(svy_designs, questions, by_vars=NULL, crude=TRUE, standardize=FALSE, 
                             stand_var=NULL, stand_data=NULL, stand_pop=NULL, update_progress=NULL)  {
  
  master_list <- lapply(setNames(svy_designs, svy_designs), function(svy_design) {
    out <- cchs_table(svy_design = as.symbol(svy_design), questions=questions, by_vars = by_vars, crude=crude, standardize=standardize, 
                      stand_var=stand_var, stand_data=stand_data, stand_pop=stand_pop, update_progress=update_progress)
  })
  
}