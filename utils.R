scale100 <- function(x){x*100}

mutate_when <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  for (i in seq(1, length(dots), by = 2)) {
    condition <- eval(dots[[i]], envir = data)
    mutations <- eval(dots[[i + 1]], envir = data[condition, , drop = FALSE])
    data[condition, names(mutations)] <- mutations
  }
  data
}

cchs_can2011 <- function(minage=NULL, maxage=NULL, agegrp_starts=NULL, agegrp_ends=NULL, agegrp_names=NULL) {
  
  stdpop <- c(376321, 	379990, 	383179, 	383741, 	375833, 	366757, 	361038, 	363440, 	358621, 	360577, 	
              365198, 	376458, 	379838, 	391245, 	405425, 	426802, 	440145, 	446524, 	455872, 	469609, 	
              479650, 	484077, 	470052, 	458775, 	461800, 	471522, 	475052, 	474287, 	475287, 	473693, 	
              479377, 	472955, 	462922, 	455951, 	456750, 	457914, 	458413, 	448563, 	450150, 	458047, 	
              480129, 	478010, 	475541, 	473014, 	479224, 	506180, 	541424, 	557834, 	562501, 	551970, 	
              558958, 	548847, 	536639, 	529913, 	516903, 	501158, 	493948, 	473623, 	451080, 	433281, 	
              424456, 	414501, 	405624, 	404282, 	401580, 	344130, 	317981, 	306903, 	292908, 	271018, 	
              257990, 	240638, 	230156, 	218638, 	206400, 	200626, 	190737, 	180672, 	176879, 	170424, 	
              163436, 	152274, 	138753, 	129386, 	117291, 	107226, 	96193, 	  85051, 	  73870, 	  64399, 	
              54015, 	  43307, 	  31444, 	  24184, 	  18966, 	  13842, 	  9962, 	  7109,  4974, 	3260, 	5268
  )
  
  age <- 0:100
  
  can2011 <- data.frame(age,stdpop)
  
  if(is.null(maxage)){
    if(!is.null(minage)){
      can2011 <- dplyr::filter(can2011, age >= minage)
    }
  }
  
  else {
    can2011 <- dplyr::filter(can2011, age <= maxage & age >= minage)
  }
  
  if(is.null(agegrp_starts)){
    stdpop <- can2011
  }
  
  else{
    
    for (k in seq_along(agegrp_starts)) {
        can2011$std_agegrp[can2011$age >= agegrp_starts[k] & can2011$age <= agegrp_ends[k]] <- agegrp_names[k]
    }
    can2011_grp <- dplyr::group_by(can2011, std_agegrp)
    stdpop <- dplyr::summarize(can2011_grp, stdpop=sum(stdpop))
  }
  return(stdpop)
}

make_std_agegrp <- function(dataframe, agegrp_starts, agegrp_ends, agegrp_names){
  
  if("std_agegrp" %in% names(dataframe)) {
    dataframe[["std_agegrp"]] <- NULL
  }
  
  for (k in seq_along(agegrp_starts)) {
  
  dataframe$std_agegrp[dataframe$DHH_AGE >= agegrp_starts[k] & dataframe$DHH_AGE <= agegrp_ends[k]] <- agegrp_names[k]
  }
  
  dataframe$std_agegrp <- as.factor(dataframe$std_agegrp)
  return(dataframe)
}

setup_design <-
  function(in_data, in_weights= "FWGT", in_repweights= "BSW[0-9]+", in_type="bootstrap", in_combined=TRUE) {
    survey::svrepdesign(
      data = in_data,
      weights = as.formula(paste0("~",in_weights)),
      repweights = in_repweights,
      type = in_type,
      combined.weights = in_combined)
  }

cchs_est <- function(svy_design, question) {
  
  if(is.symbol(question)==FALSE) {
    question <- as.symbol(question)
  }
  
  if(is.character(svy_design[["variables"]][[as.name(question)]])){
    svy_design[["variables"]][[as.name(question)]] <- as.factor(svy_design[["variables"]][[as.name(question)]])
  }
  
  wgt_est <- survey::svymean(as.formula(paste0("~", as.name(question))), svy_design)
  
  combine <- 
    dplyr::bind_cols(
      as.data.frame(wgt_est),
      as.data.frame(survey::cv(wgt_est), optional=TRUE),
      as.data.frame(stats::confint(wgt_est)))

  cleanup <- 
    dplyr::rename(combine, Estimate = "mean", CV = "V1", `Lower 95% CI` = "2.5 %",
                  `Upper 95% CI` = "97.5 %")
  
  cleanup1 <- dplyr::select(cleanup,-SE)
  
  if(is.numeric(svy_design[["variables"]][[as.name(question)]])) {
    clean_out <- dplyr::mutate(cleanup1, CV=CV*100)
    clean_out$ind_level <- "Mean"
  }
  else {
    clean_out <- dplyr::mutate_all(cleanup1, ~scale100(.))
    clean_out$ind_level <- levels(svy_design[["variables"]][[as.name(question)]])
    dplyr::filter(clean_out, clean_out$ind_level != "(Missing)")
  }
  
  clean_out$`Quality Indicator` <-
    as.factor(
      ifelse(
        clean_out$CV <= 15,
        "",
        ifelse(
          clean_out$CV <= 25,
          "C",
          ifelse(
            clean_out$CV <= 35,
            "D",
            "NR"
          )
        )
      )
    )
  return(clean_out)
}

cchs_estby <- function(svy_design, by_vars, question) {
  
  for (i in seq_along(by_vars)) {
    by_var <- rlang::sym(by_vars[i])
    
    bynum <- i
    if (exists("update_progress")) {
      if(is.function(update_progress)) {
      update_progress(detail = paste0("Running question ", qnum, "(", question, ") for ", geo_var, 
                                                      "by stratifier", bynum,":", by_var))
      }
    }
    
    if(!is.factor(svy_design[["variables"]][[as.name(by_var)]])){
      svy_design[["variables"]][[as.name(by_var)]] <- as.factor(svy_design[["variables"]][[as.name(by_var)]])
    }
    
    print(str(svy_design[["variables"]][[as.name(by_var)]]))
    
    if(is.character(svy_design[["variables"]][[as.name(question)]])){
      svy_design[["variables"]][[as.name(question)]] <- as.factor(svy_design[["variables"]][[as.name(question)]])
    }
  
    wgt_est <- 
      as.data.frame(
        survey::svyby(as.formula(paste0("~", question)),as.formula(paste0("~", by_var)), svy_design, 
                      survey::svymean, vartype = c("ci","cv")))

    if(is.numeric(svy_design[["variables"]][[as.name(question)]])){
      wgt_est_clean <- dplyr::mutate_at(wgt_est, dplyr::vars(dplyr::contains("cv")), scale100)
    }
  
    else {
      wgt_est_clean <- dplyr::mutate_at(wgt_est, dplyr::vars(-!! by_var), scale100)
    }
    
    names(wgt_est_clean)[1] <- "strat_level"
    
    wgt_est_clean1 <- dplyr::rename_at(wgt_est_clean, dplyr::vars(dplyr::contains(rlang::quo_name(question))),stringr::str_replace,rlang::quo_name(question),"est_")
    
    clean <- reshape2::melt(wgt_est_clean1, id.vars = c("strat_level"))
    
    if(is.numeric(svy_design[["variables"]][[as.name(question)]])){
      clean$ind_level="Mean"}
    else{
    clean$ind_level <- stringr::str_replace_all(clean$variable,c("ci_l." = "","ci_u." = "","cv." = "","est_" = ""))
    }

    clean$measure <-
      as.factor(
        ifelse(stringr::str_detect(clean$variable,"ci_l"), "Lower 95% CI", ifelse(
          stringr::str_detect(clean$variable,"ci_u"), "Upper 95% CI", ifelse(
              stringr::str_detect(clean$variable,"cv"), "CV", "Estimate")
            
          )
        )
      )
    
    clean <- dplyr::select(clean, -variable)

    ready <- reshape2::dcast(clean, ind_level+strat_level~measure)
    
    ready$stratifier <- rlang::quo_name(by_var)
    
    ready$`Quality Indicator` <-
      as.factor(
        ifelse(
          ready$CV <= 15,
          "",
          ifelse(
            ready$CV <= 25,
            "C",
            ifelse(
              ready$CV <= 35,
              "D",
              "NR"
            )
          )
        )
      )
    
    if (i==1) {
      output <- ready
    }
    else output <- {dplyr::bind_rows(output, ready)}
    print(output)
  }
  clean_out <- dplyr::filter(output, ind_level != "(Missing)", strat_level != "(Missing)")
  clean_out <- dplyr::mutate_if(clean_out, is.character, ~as.factor(.))
  return(clean_out)
}

  