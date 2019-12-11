#' Import and prepare CCHS data
#'
#' Optionally imports CCHS data and bootstrap weights from Stata, runs errata from Statistics Canada, and cleans data.
#'
#' @param cchsfile Optional. File path to Stata data file containing 2015/2016 CCHS data. One of stata_cchs or r_cchs must be specified.
#' @param includes_bootwts Indicates if cchs data file, either Stata or R, already contains the bootstrap weights which are distributed in a separate file by Statistics Canada. FALSE, the default, is indicates that the CCHS response data and bootweights are in separate files. TRUE indicates that the CCHS data file contains the bootweights.
#' @param bootwts Optional. File path to Stata data file containing bootstrap weights for CCHS data.
#' @param run_errata Indicates if the published errata for CCHS should be corrected. 
#'
#' @return Dataframe in tibble format of clean and ready to use 2015/2016 CCHS data.
#'
#' @author Yvonne C DeWit, \email{ydewit@@hpeph.ca} or \email{ycdewit@@gmail.com}.
#'
#' @seealso \code{\link{cchs_errata}}
#' @seealso \code{\link{cchs_cleanup}}
#'
#' @examples
#' cchs <-
#'    cchs_prep(
#'       cchsfile = "/data/cchs_on_distr.dta",
#'       bootwts = "/data/cchs_on_bootwt.dta")
#'
#' cchs_combined <-
#'    cchs_prep(
#'       cchsfile = cccchs,
#'       inclues_bootwts = TRUE,
#'       run_errata = FALSE)
#' @export
#' 

cchs_prep <-
  function (cchsfile, includes_bootwts=FALSE, bootwts=NULL, run_errata=TRUE, update_progress=NULL) {

    if (grepl(".dta", cchsfile, ignore.case=TRUE) == TRUE) {
      cchs <- foreign::read.dta(cchsfile)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    else if (grepl(".xlsx", cchsfile, ignore.case=TRUE) == TRUE) {
      cchs <- read_xlsx(cchsfile)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    else if (grepl(".sav", cchsfile, ignore.case=TRUE) == TRUE) {
      cchs <- foreign::read.spss(cchsfile, to.data.frame = TRUE)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    else if (grepl(".rds", cchsfile, ignore.case=TRUE) == TRUE) {
      cchs <- readRDS(cchsfile)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    
    else if (grepl(".csv", cchsfile, ignore.case=TRUE) == TRUE) {
      cchs <- utils::read.csv(cchsfile)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    
    else {
      stop("cchsfile must be a filepath to a STATA, SPSS, R, Excel, or Comma-separated data file (i.e. with a .dta, .sav, .rds, .xlsx, or .csv file extension)")
    }
    
    cchs <- dplyr::rename_all(cchs, ~stringr::str_to_upper(.))
    cchs$cycle <- substr(cchs$ONT_ID, 1, 4)
    if(is.factor(cchs$ADM_YOI)) {
      cchs$ADM_YOI <- as.numeric(cchs$cycle)
    } 
    
    if(run_errata==TRUE) {
      cchs_indata <- cchs_errata(dataframe = cchs) 
      if (is.function(update_progress)) {
        update_progress(detail = "Errata")
      }
    }
    else {
      cchs_indata <- cchs
    }
    
    cchs_clean <- cchs_cleanup(dataframe = cchs_indata)
    
    if (is.function(update_progress)) {
      update_progress(detail = "Basic cleaning")
    }

    if (includes_bootwts == FALSE){
      if (grepl(".dta", bootwts, ignore.case=TRUE) == TRUE) {
        cchs_bootwt <- foreign::read.dta(bootwts)
        
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      else if (grepl(".xlsx", bootwts, ignore.case=TRUE) == TRUE) {
        cchs_bootwt <- read_xlsx(bootwts)
        
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      else if (grepl(".sav", bootwts, ignore.case=TRUE) == TRUE) {
        cchs_bootwt <- foreign::read.spss(bootwts, to.data.frame = TRUE)
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      else if (grepl(".rds", bootwts, ignore.case=TRUE) == TRUE) {
        cchs_bootwt <- readRDS(bootwts)
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      
      else if (grepl(".csv", bootwts, ignore.case=TRUE) == TRUE) {
        cchs_bootwt <- utils::read.csv(bootwts)
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      
      else {
        stop("If includes_bootwts is TRUE, bootwts must be a filepath to a STATA, SPSS, R, Excel, or Comma-separated data file (i.e. with a .dta, .sav, .rds, .xlsx, or .csv file extension)")
      }
    
    cchs_bootwt <- dplyr::rename_all(cchs_bootwt, ~stringr::str_to_upper(.))    
    cchs_output <- dplyr::left_join(cchs_clean, cchs_bootwt, by = c("ONT_ID"))
    
    }
    
    else {
      if (any(stringr::str_detect(names(cchs_clean), "BSW") == FALSE)){
        cchs_output <- cchs_clean
      }
      else {
        stop("includes_bootwts is TRUE, but file does not contain variables starting with BSW (the bootstrap weight variabls)")
      }
    }
    return(cchs_output)
  }
  

