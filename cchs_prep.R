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
  function (stata_cchsfile=NULL, stata_bootwts=NULL, r_cchs=NULL, r_bootwts=NULL, includes_bootwts=FALSE, phu_name, peer_group, prov_name) {

  if (!is.null(stata_cchsfile)) {
    hs1516 <- foreign::read.dta(stata_cchsfile)
  }
  else {
    if (is.null(r_cchs)){
      stop("Error: must specify one of stata_cchsfile or r_cchsdf")
      }

    else {
      hs1516 <- r_cchs
      }
    }

  if (!phu_name %in% levels(hs1516$GEODVHR4)) {
    print (levels(hs1516$GEODVHR4))
    stop("PHU name is not in list of PHUs. See list of PHUs above - must match spelling and case.", call. = FALSE)
  }
    
  else {
    if (!peer_group %in% levels(hs1516$GEODVPG)) {
      print (levels(hs1516$GEODVPG))
      stop("Peer group is not in list of peer groups. See list of peer groups above - must match spelling and case.")
    }
    else {
      cchs_errata <- cchs_errata(dataframe = hs1516)

      cchs_clean <-
        cchs_cleanup(
          dataframe = cchs_errata,
          phu = phu_name,
          peer = peer_group)

      
      if (includes_bootwts==FALSE){
        if (!is.null(stata_bootwts)) {
          hs1516_bootwt <- foreign::read.dta(stata_bootwts)
        }
        else {
          if (is.null(r_bootwts)){
            stop("Must specify one of stata_bootwts or r_bootwts")
            }
          else {
            hs1516_bootwt <- r_bootwts
            }
          }
        cchs_combined <- dplyr::left_join(cchs_clean, hs1516_bootwt, by = c("ONT_ID"))
      }
      else{
        return(cchs_clean)
      }
    }
  }
}

