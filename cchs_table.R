library(survey)

# TO DO
# Switch as.name(geo_var)!="prov" to be if df$geo_var == 100% of data

cchs_table <- function(dataframe, svy_design=NULL, questions, geo_vars, by_vars=NULL, standardize=FALSE, 
                       stand_var=NULL, stand_data=NULL, stand_pop=NULL, update_progress=NULL){
  
  if(is.null(svy_design)){
    svy_design <- setup_design(dataframe)
  }
  
  svy_design[["variables"]] <- dplyr::mutate_if(svy_design[["variables"]], is.factor, ~forcats::fct_explicit_na(.))
  
  qnum <- 0
  
  master_list <- lapply(setNames(questions, questions), function(question) {
    
    qnum <- qnum + 1
    
    if (is.function(update_progress)) {
      update_progress(detail = paste0("Running question ", qnum, ": ", question))
    }
    
    output_list <- lapply(setNames(geo_vars,geo_vars), function(geo_var) {
      
      if (is.function(update_progress)) {
        update_progress(detail = paste0("Running question ", qnum, "(", question, ") for ", geo_var))
      }
      
      geo_var <- rlang::sym(geo_var)
      
      question <- rlang::sym(question)
      
      if(as.name(geo_var)!="prov") {
        svy_design <- subset(svy_design, !! geo_var=="Yes")
      }
      
      df_filtered <- 
        dplyr::filter(dataframe, !! geo_var == "Yes" & !(is.na(!! question)))
      
      num_q <-is.numeric(df_filtered[[rlang::quo_name(question)]])
      
      if(num_q==TRUE){
        raw_n <- dplyr::count(df_filtered)
        names(raw_n) <- "Sample Size"
        ind_levels <- "Mean"
      }
      
      else{
        raw_n <- dplyr::count(df_filtered, !! question)
        names(raw_n) <- c("ind_levels", "Sample Size")
      }
      
      if(!is.null(by_vars)){
        
        by_n <- lapply(setNames(by_vars, by_vars), function(by_col) {
          if(!is.factor(df_filtered[[as.name(by_col)]])){
            df_filtered[[as.name(by_col)]] <- as.factor(df_filtered[[as.name(by_col)]])
          }
          freqdf <- as.data.frame(table(df_filtered[[as.name("gendvswl_rev")]],df_filtered[[as.name(by_col)]]))
          names(freqdf) <- c("ind_level", "strat_level", "Sample Size")
          freq <- cbind(freqdf$ind_level, stratifier=as.name(by_col), freqdf$strat_level, freqdf$`Sample Size`)
          freq$ind_level <- as.factor(ifelse(as.character(freq$ind_level)==as.character(freq$indicator), "Mean", as.character(freq$ind_level)))
          return(freq)
        })
          
        by_n <- dplyr::bind_rows(by_n)
        
        by_n$stratifier <- as.factor(by_n$stratifier)
        by_n$strat_level <- as.factor(by_n$strat_level)
        
      }
      
      if (crude == TRUE) {
        
        crude_est <- clean_est(svy_design, question)
        
        crude_est$est_type <- "crude"
        
        output <-
          dplyr::left_join(raw_n, out_crude)
        
        if(!is.null(by_vars)){
          out_crude_by <- cchs_estby(svy_design, by_vars, question)
          output_crude_by <- dplyr::left_join(by_n, out_crude_by)
          output_crude_by$est_type <- "crude"
          output <- dplyr::bind_rows(output, output_crude_by)
        }  
      }
      
      if(standardize == TRUE) {
        
        if(is.null(stand_data)|is.null(stand_pop)|is.null(stand_var)){
          stop("If standardize=TRUE then stand_data, stand_pop, and stand_var arguments must be specified")}
        
        stand_pop <- as.symbol(stand_pop)   
        
        stand_var <- as.symbol(stand_var)
        
        base <- dplyr::count(df_filtered, !!stand_var)
        std_pop_data <- dplyr::left_join(base, stand_data)
        
        std_svy_design <- 
          survey::svystandardize(
            design = svy_design,
            by = as.formula(paste0("~ ",as.name(stand_var))),
            over = ~ 1,
            population = dplyr::pull(std_pop_data,var = !! stand_pop))
        
        std_est <- cchs_est(std_svy_design, question)
        std_est$est_type <- "standardized"
        
        out_stand <- left_join(raw_n, std_est)
        
        if(!exists(output)) {
          output <- out_stand
        }
        
        else output <- dplyr::bind_rows(output, out_stand)
        
        if(!is.null(by_vars)){
          out_std_by <- cchs_estby(std_svy_design, by_vars, question)
          output_std_by <- dplyr::left_join(by_n, out_std_by)
          output_std_by$est_type <- "standardized"
          output <- dplyr::bind_rows(output, output_std_by)
        } 
      }
      output$geo_area <- as.name(geo_var)
      clean_output <- dplyr::mutate_at(output, dplyr::vars(Estimate, `Lower 95% CI`, `Upper 95% CI`),
                                       list(~ifelse(`Quality Indicator` == "NR" | `Sample Size` < 10, NA, .)))
      return(clean_output)
    })
    to_masterlist <- dplyr::bind_rows(output_list)
    to_masterlist$indicator <- as.name(question)
    return(to_masterlist)
  })
}