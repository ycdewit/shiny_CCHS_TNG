scale100 <- function(x){x*100}

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
      can2011 <- filter(can2011, can2011$age >= minage)
    }
  }
  
  else {
    can2011 <- filter(can2011, can2011$age <= maxage & can2011$age >= minage)
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
  
  dataframe$std_agegrp[dataframe$DHH_AGE >= agegrp_starts[k] & dataframe$DHH_AGE <= agegrp_ends[k]] <- agegrp_names[k]
  
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