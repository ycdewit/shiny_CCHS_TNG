#' Import and prepare CCHS data
#'
#' Optionally imports CCHS data and bootstrap weights from Stata, runs errata from Statistics Canada, and cleans data.
#'
#' @param stata_cchsfile Optional. File path to Stata data file containing 2015/2016 CCHS data. One of stata_cchs or r_cchs must be specified.
#' @param r_cchs Optional. Name of R dataframe of CCHS data.One of stata_cchs or r_cchs must be specified.
#' @param includes_bootwts Optional. Indicates if cchs data file, either Stata or R, already contains the bootstrap weights which are distributed in a separate file by Statistics Canada. FALSE, the default, is indicates that the CCHS response data and bootweights are in separate files. TRUE indicates that the CCHS data file contains the bootweights.
#' @param stata_bootwts Optional. File path to Stata data file containing bootstrap weights for CCHS data.
#' @param r_bootwts Optional. Name of R dataframe of bootstrap weights.
#' @param phu_name Name of public health unit or health region as found in CCHS variable GEODVHR4.
#' @param peer_group Name of the Health Region Peer Group for your public health unit as found in CCHS variable GEODVPG. Not sure what peer group your PHU/Health Region is in? Go to: \url{https://www150.statcan.gc.ca/n1/pub/82-402-x/2015001/regions/tbl/tbl8-eng.htm}.
#' @param prov_name Name of province. CCHS data should be already restricted to this province only.
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
#'       stata_cchsfile = "/data/hs1516_on_distr.dta",
#'       stata_bootwts = "/data/hs1516_on_bootwt.dta",
#'       phu_name = "Hastings and Prince Edward Counties HU",
#'       peer_group = "Health Region Peer Group C",
#'       prov_name = "Ontario")
#'
#' cchs_combined <-
#'    cchs_prep(
#'       r_cchsdf = cchs1516,
#'       inclues_bootwts = TRUE,
#'       phu_name = "Calgary Zone",
#'       peer_group = "Health Region Peer Group B",
#'       prov_name = "Alberta")
#' @export

# For ycdewit - To do:
# 1. Add parameter/code for creating prov_flag. Past efforts had *issues*.
# E.g. documentation: @param prov_code Optional. Code for creating a "Yes"/"No" column for your province. If your data file only contains data from your province, ignore.
# 2. Run second a version of second

cchs_prep <-
  function (cchsfile, bootwts=NULL, includes_bootwts=FALSE, run_errata=TRUE, 
            phu_name, peer_group, prov_name, update_progress=NULL) {

    if (grepl(".dta", cchsfile, ignore.case=TRUE) == TRUE) {
      hs1516 <- foreign::read.dta(cchsfile)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    else if (grepl(".xlsx", cchsfile, ignore.case=TRUE) == TRUE) {
      hs1516 <- (stata_cchsfile)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    else if (grepl(".sav", cchsfile, ignore.case=TRUE) == TRUE) {
      hs1516 <- foreign::read.spss(stata_cchsfile, to.data.frame = TRUE)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    else if (grepl(".rds", cchsfile, ignore.case=TRUE) == TRUE) {
      hs1516 <- readRDS(stata_cchsfile)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    
    else if (grepl(".csv", cchsfile, ignore.case=TRUE) == TRUE) {
      hs1516 <- utils::read.csv(stata_cchsfile)
      if (is.function(update_progress)) {
        update_progress(detail = "Data import")
      }
    }
    
    else {
      stop("cchsfile must be a filepath to a STATA, SPSS, R, Excel, or Comma-separated data file (i.e. with a .dta, .sav, .rds, .xlsx, or .csv file extension)")
    }

    if (!phu_name %in% levels(hs1516$GEODVHR4)) {
      print (levels(hs1516$GEODVHR4))
      stop("PHU name is not in list of PHUs. See list of PHUs above - must match spelling and case.", call. = FALSE)
    }
    
    if (!peer_group %in% levels(hs1516$GEODVPG)) {
      print (levels(hs1516$GEODVPG))
      stop("Peer group is not in list of peer groups. See list of peer groups above - must match spelling and case.")
    }
    
    if(run_errata==TRUE) {
      cchs_indata <- cchs_errata(dataframe = hs1516) 
      if (is.function(update_progress)) {
        update_progress(detail = "Errata")
      }
    }
    else {
      cchs_indata <- hs1516
    }
    
    cchs_indata <- dplyr::rename_all(cchs_indata, ~stringr::str_to_upper(.))
    cchs_clean <-
      cchs_cleanup(
        dataframe = cchs_indata,
        phu = phu_name,
        peer = peer_group)
    if (is.function(update_progress)) {
      update_progress(detail = "Basic cleaning")
    }

    if (includes_bootwts == TRUE){
      if (grepl(".dta", bootwts, ignore.case=TRUE) == TRUE) {
        hs1516_bootwt <- foreign::read.dta(stata_cchsfile)
        
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      else if (grepl(".xlsx", bootwts, ignore.case=TRUE) == TRUE) {
        hs1516_bootwt <- (stata_cchsfile)
        
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      else if (grepl(".sav", bootwts, ignore.case=TRUE) == TRUE) {
        hs1516_bootwt <- foreign::read.spss(stata_cchsfile, to.data.frame = TRUE)
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      else if (grepl(".rds", bootwts, ignore.case=TRUE) == TRUE) {
        hs1516_bootwt <- readRDS(stata_cchsfile)
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      
      else if (grepl(".csv", bootwts, ignore.case=TRUE) == TRUE) {
        hs1516_bootwt <- utils::read.csv(stata_cchsfile)
        if (is.function(update_progress)) {
          update_progress(detail = "Bootstrap weights import")
        }
      }
      
      else {
        stop("If includes_bootwts is TRUE, bootwts must be a filepath to a STATA, SPSS, R, Excel, or Comma-separated data file (i.e. with a .dta, .sav, .rds, .xlsx, or .csv file extension)")
      }
    
    hs1516_bootwt <- dplyr::rename_all(hs1516_bootwt, ~stringr::str_to_upper(.))    
    cchs_output <- dplyr::left_join(cchs_clean, hs1516_bootwt, by = c("ONT_ID"))
    
    }
    
    else {
      if (any(stringr::str_detect(names(cchs_clean), "BSW") == TRUE)){
        cchs_output <- cchs_clean
      }
      else {
        stop("includes_bootwts is TRUE, but file does not contain variables starting with BSW (the bootstrap weight variabls)")
      }
    }
    return(cchs_output)
  }
  

