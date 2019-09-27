### CCHS 2015/2016 Errata

### UP TO DATE BASED ON Canadian Community Health Survey – Annual Component Errata !!! JUNE 2018 !!! ###
### IMPORTANT NOTE: If you are using the PMH module from CCHS 2015, you will need to code Errata Item-006.
###     It is not included here as it is not in the 2015/2016 dataset and would yield errors.

### SET YOUR INPUT FILE (replace "your_file" with name of your file)
# You can either use this code to set your input cchs file or find and replace "cchs_input" with your file name
cchs_errata <- function(dataframe) {

cchs_input <- dataframe

# Create new columns for Item-009

cchs_input$PAATTRAV <- 
  rowSums(cchs_input[, c("PAA_010A", "PAA_010B", "PAA_010C", "PAA_010D", "PAA_010E", "PAA_010F", "PAA_010G")] == "Yes", na.rm = TRUE)
cchs_input$PAATREC <- 
  rowSums(cchs_input[, c("PAA_040A", "PAA_040B", "PAA_040C", "PAA_040D", "PAA_040E", "PAA_040F", "PAA_040G")] == "Yes", na.rm = TRUE)
cchs_input$PAATOTH <- 
  rowSums(cchs_input[, c("PAA_070A", "PAA_070B", "PAA_070C", "PAA_070D", "PAA_070E", "PAA_070F", "PAA_070G")] == "Yes", na.rm = TRUE)

# Item-001
cchs_input$PAY_100 <- ifelse(cchs_input$PAY_090 == "No", 0, cchs_input$PAY_100)
cchs_input$PAY_105 <- ifelse(cchs_input$PAY_090 == "No" | cchs_input$PAY_100 == 168, 0, cchs_input$PAY_105)

# Item-002
cchs_input$HMC_10_1 <- as.factor(ifelse(
  cchs_input$DHHDVHSZ > 1 & cchs_input$HMC_005A == "No", "Valid skip", as.character(cchs_input$HMC_10_1)))
cchs_input$HMC_10_2 <- as.factor(ifelse(
  cchs_input$DHHDVHSZ > 1 & cchs_input$HMC_005B == "No", "Valid skip", as.character(cchs_input$HMC_10_2)))
cchs_input$HMC_10_3 <- as.factor(ifelse(
  cchs_input$DHHDVHSZ > 1 & cchs_input$HMC_005C == "No", "Valid skip", as.character(cchs_input$HMC_10_3)))
cchs_input$HMC_10_4 <- as.factor(
  ifelse(cchs_input$DHHDVHSZ > 1 & cchs_input$HMC_005D == "No", "Valid skip", as.character(cchs_input$HMC_10_4)))
cchs_input$HMC_10_5 <- as.factor(ifelse(
  cchs_input$DHHDVHSZ > 1 & cchs_input$HMC_005E == "No", "Valid skip", as.character(cchs_input$HMC_10_5)))

# Item-003
cchs_input$DIA_055 <- as.factor(ifelse(
  cchs_input$DHH_AGE > 34 & cchs_input$DIA_005 %in% c("Yes", "No", "Don't know"), as.character(cchs_input$CCC_070), "Valid skip"))
cchs_input$DIA_060 <- as.factor(ifelse(
  cchs_input$DHH_AGE > 34 & cchs_input$DIA_005 %in% c("Yes", "No", "Don't know"), as.character(cchs_input$CCC_080), "Valid skip"))

# Item-004
cchs_input$ALWDVLTR <- 
  as.factor(
    ifelse(
      cchs_input$DOALW == "No" | cchs_input$ALC_005 == "No", "Valid Skip", 
      ifelse(
        cchs_input$ALC_005 %in% c("Don't know", "Refusal", "Not stated") | 
          cchs_input$ALC_010 %in% c("Don't know", "Refusal", "Not stated") | 
          cchs_input$ALW_005 %in% c("Don't know", "Refusal", "Not stated") | 
          dplyr::between(cchs_input$ALW_010, 997, 999) | dplyr::between(cchs_input$ALW_015, 997, 999) | 
          dplyr::between(cchs_input$ALW_020, 997, 999) | dplyr::between(cchs_input$ALW_025, 997, 999) | 
          dplyr::between(cchs_input$ALW_030, 997, 999) | dplyr::between(cchs_input$ALW_035, 997, 999) | 
          dplyr::between(cchs_input$ALW_040, 997, 999), 
        "Not stated", 
        ifelse(
          cchs_input$DHH_SEX == "Male" & (
            dplyr::between(cchs_input$ALW_010, 4, 995) | dplyr::between(cchs_input$ALW_015, 4, 995) | 
              dplyr::between(cchs_input$ALW_020, 4, 995) | dplyr::between(cchs_input$ALW_025, 4, 995) | 
              dplyr::between(cchs_input$ALW_030, 4, 995) | dplyr::between(cchs_input$ALW_035, 4, 995) | 
              dplyr::between(cchs_input$ALW_040, 4, 995) | dplyr::between(cchs_input$ALWDVWKY, 16, 995)
            ),
          "Increased long term health risk due to drinking",
          ifelse(
            cchs_input$DHH_SEX == "Female" & (
              dplyr::between(cchs_input$ALW_010, 3, 995) | dplyr::between(cchs_input$ALW_015, 3, 995) | 
                dplyr::between(cchs_input$ALW_020, 3, 995) | dplyr::between(cchs_input$ALW_025, 3, 995) | 
                dplyr::between(cchs_input$ALW_030, 3, 995) | dplyr::between(cchs_input$ALW_035, 3, 995) | 
                dplyr::between(cchs_input$ALW_040, 3, 995) | dplyr::between(cchs_input$ALWDVWKY, 11, 995)
              ), 
            "Increased long term health risk due to drinking", 
            ifelse(
              cchs_input$ALW_005 == "No" | cchs_input$ALC_010 == "No", "No increased long term heath risk due to drinking", 
              ifelse(
                cchs_input$DHH_SEX == "Male" & (
                  cchs_input$ALW_010 <= 3 & cchs_input$ALW_015 <= 3 & cchs_input$ALW_020 <= 3 & cchs_input$ALW_025 <= 3 &
                    cchs_input$ALW_030 <= 3 & cchs_input$ALW_035 <= 3 & cchs_input$ALW_040 <= 3 & cchs_input$ALWDVWKY <= 15
                  ),
                "No increased long term heath risk due to drinking", 
                ifelse(
                  cchs_input$DHH_SEX == "Female" & (
                    cchs_input$ALW_010 <= 2 & cchs_input$ALW_015 <= 2 & cchs_input$ALW_020 <= 2 & cchs_input$ALW_025 <= 2 & 
                      cchs_input$ALW_030 <= 2 & cchs_input$ALW_035 <= 2 & cchs_input$ALW_040 <= 2 & cchs_input$ALWDVWKY <= 10
                    ), 
                  "No increased long term heath risk due to drinking", 
                  "Error"
                  )
                )
              )
            )
          )
        )
      )
    )
cchs_input$ALWDVSTR <- 
  as.factor(
    ifelse(
      cchs_input$DOALW == "No" | cchs_input$ALC_005 == "No", 
      "Valid Skip", 
      ifelse(
        cchs_input$ALC_005 %in% c("Don't know", "Refusal", "Not stated") | 
          cchs_input$ALW_005 %in% c("Don't know", "Refusal", "Not stated") | 
          dplyr::between(cchs_input$ALW_010, 997, 999) | dplyr::between(cchs_input$ALW_015, 997, 999) | 
          dplyr::between(cchs_input$ALW_020, 997, 999) | dplyr::between(cchs_input$ALW_025, 997, 999) | 
          dplyr::between(cchs_input$ALW_030, 997, 999) | dplyr::between(cchs_input$ALW_035, 997, 999) | 
          dplyr::between(cchs_input$ALW_040, 997, 999), 
        "Not stated", 
        ifelse(
          cchs_input$DHH_SEX == "Male" & (
            dplyr::between(cchs_input$ALW_010, 5, 995) | dplyr::between(cchs_input$ALW_015, 5, 995) | 
              dplyr::between(cchs_input$ALW_020, 5, 995) | dplyr::between(cchs_input$ALW_025, 5, 995) | 
              dplyr::between(cchs_input$ALW_030, 5, 995) | dplyr::between(cchs_input$ALW_035, 5, 995) | 
              dplyr::between(cchs_input$ALW_040, 5, 995) | dplyr::between(cchs_input$ALWDVWKY, 16, 995)
            ), 
          "Increased short term health risks due to drinking", 
          ifelse(
            cchs_input$DHH_SEX == "Female" & (
              dplyr::between(cchs_input$ALW_010, 4, 995) | dplyr::between(cchs_input$ALW_015, 4, 995) | 
                dplyr::between(cchs_input$ALW_020, 4, 995) | dplyr::between(cchs_input$ALW_025, 4, 995) | 
                dplyr::between(cchs_input$ALW_030, 4, 995) | dplyr::between(cchs_input$ALW_035, 4, 995) | 
                dplyr::between(cchs_input$ALW_040, 4, 995) | dplyr::between(cchs_input$ALWDVWKY, 11, 995)
              ), 
            "Increased short term health risks due to drinking", 
            ifelse(
              cchs_input$ALW_005 == "No" | cchs_input$ALC_010 == "No", 
              "No increased short term risk from due to drinking", 
              ifelse(
                cchs_input$DHH_SEX == "Male" & (
                  cchs_input$ALW_010 <= 4 & cchs_input$ALW_015 <= 4 & cchs_input$ALW_020 <= 4 & cchs_input$ALW_025 <= 4 & 
                    cchs_input$ALW_030 <= 4 & cchs_input$ALW_035 <= 4 & cchs_input$ALW_040 <= 4 & cchs_input$ALWDVWKY <= 15
                  ), 
                "No increased short term risk from due to drinking", 
                ifelse(
                  cchs_input$DHH_SEX == "Female" & (
                    cchs_input$ALW_010 <= 3 & cchs_input$ALW_015 <= 3 & cchs_input$ALW_020 <= 3 & cchs_input$ALW_025 <= 3 &
                      cchs_input$ALW_030 <= 3 & cchs_input$ALW_035 <= 3 & cchs_input$ALW_040 <= 3 & cchs_input$ALWDVWKY <= 10
                    ), 
                  "No increased short term risk from due to drinking", 
                  "Error"
                  )
                )
              )
            )
          )
        )
      )
    )

# Item-005 - NA - just for SAS file

# Item-006 - NA - for PMH which is a 2015 only module

# Item-007 - NA - Optional UPE module - not use in Ontario in 15/16

# Item-008
cchs_input$EHG2DVR3 <- 
  as.factor(
    ifelse(
      (cchs_input$EHG2_01 %in% c("Grade 8 or lower (Québec: Secondary II or lower)", 
                                 "Grade 9-10 (Qc: Sec. III or IV, N.L.: 1st yr sec.)") | 
         cchs_input$EHG2_02 == "No") & (cchs_input$EHG2_04 == "Less than high school diploma or its equivalent" | cchs_input$EHG2_03 == "No"), 
      "Less than secondary school graduation", 
      ifelse(
        cchs_input$EHG2_04 == "High school diploma or a high school equivalency certificate" | 
          cchs_input$EHG2_02 == "Yes" & cchs_input$EHG2_03 == "No", 
        "Secondary school graduation, no post-secondary education", 
        ifelse(
          cchs_input$EHG2_04 %in% c("Trade certificate or diploma", 
                                    "College/CEGEP/other non-university certificate or diploma", 
                                    "University certificate or diploma below the bachelor's level", 
                                    "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)", 
                                    "University certificate, diploma, degree above the BA level"), 
          "Post-secondary certificate diploma or univ degree", 
          ifelse(
            cchs_input$EHG2_01 %in% c("Don't know", "Refusal", "Not Stated") & cchs_input$EHG2_02 == "No" | 
              cchs_input$EHG2_02 %in% c("Don't know", "Refusal", "Not Stated") | 
              cchs_input$EHG2_03 %in% c("Don't know", "Refusal", "Not Stated") | 
              cchs_input$EHG2_04 %in% c("Don't know", "Refusal", "Not Stated"), 
            "Not stated", 
            as.character(cchs_input$EHG2DVR3)
            )
          )
        )
      )
    )

cchs_input$EHG2DVR9 <- 
  as.factor(
    ifelse(
      cchs_input$EHG2_01 == "Grade 8 or lower (Québec: Secondary II or lower)" & (
        cchs_input$EHG2_03 == "No" | cchs_input$EHG2_04 == "Less than high school diploma or its equivalent"
        ), 
      "Grade 8 or lower (QC: Sec 2 or lower)", 
      ifelse(
        cchs_input$EHG2_01 == "Grade 9-10 (Qc: Sec. III or IV, N.L.: 1st yr sec.)" & (
          cchs_input$EHG2_03 == "No" | cchs_input$EHG2_04 == "Less than high school diploma or its equivalent"
          ), 
        "Grade 9-10 (Qué: Sec 3/4; Newfoundland & Labrador: Sec 1)", 
        ifelse(
          cchs_input$EHG2_01 == "Grade 11-13 (Qc: Sec. V, N.L.: 2-3 yr of sec.)" & cchs_input$EHG2_02 == "No" & (cchs_input$EHG2_03 == "No" | cchs_input$EHG2_04 == "Less than high school diploma or its equivalent"), 
          "Grade 11-13 (Qué: Sec 5; Newfoundland & Labrador: Sec 2/3)", 
          ifelse(
            (cchs_input$EHG2_02 == "Yes" & cchs_input$EHG2_03 == "No") | 
              cchs_input$EHG2_04 == "High school diploma or a high school equivalency certificate", 
            "Secondary school graduation, no post-secondary", 
            ifelse(
              cchs_input$EHG2_04 == "Trade certificate or diploma", 
              "Trade certificate or diploma", 
              ifelse(
                cchs_input$EHG2_04 == "College/CEGEP/other non-university certificate or diploma", 
                "Certificate/diploma - college, CEGEP, etc (non-trades)", 
                ifelse(
                  cchs_input$EHG2_04 == "University certificate or diploma below the bachelor's level", 
                  "University certificate or diploma below bachelor's level", 
                  ifelse(
                    cchs_input$EHG2_04 == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)", 
                    "Bachelor's degree", 
                    ifelse(
                      cchs_input$EHG2_04 == "University certificate, diploma, degree above the BA level", 
                      "Certificate/diploma/univ degree above bachelor's level", 
                      ifelse(
                        cchs_input$EHG2_01 %in% c("Don't know", "Refusal", "Not Stated") & cchs_input$EHG2_02 == "No" | 
                          cchs_input$EHG2_02 %in% c("Don't know", "Refusal", "Not Stated") | 
                          cchs_input$EHG2_03 %in% c("Don't know", "Refusal", "Not Stated") | 
                          cchs_input$EHG2_04 %in% c("Don't know", "Refusal", "Not Stated"), 
                        "Not stated", 
                        as.character(cchs_input$EHG2DVR9)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

# Item-009
cchs_input$PAATTRAV <- ifelse(cchs_input$PAA_010A %in% c("Don't know", "Refusal", "Not stated"), 9, cchs_input$PAATTRAV)
cchs_input$PAATREC <- ifelse(cchs_input$PAA_040A %in% c("Don't know", "Refusal", "Not stated"), 9, cchs_input$PAATREC)
cchs_input$PAATOTH <- ifelse(cchs_input$PAA_070A %in% c("Don't know", "Refusal", "Not stated"), 9, cchs_input$PAATOTH)
cchs_input$PAADVATR <- 
  ifelse(
    cchs_input$DOPAA == "No" | cchs_input$DHH_AGE < 18, 
    9999.6, 
    ifelse(
      cchs_input$PAADVTRV == 99999 | cchs_input$PAATTRAV == 9, 
      9999.9, 
      ifelse(
        cchs_input$PAADVTRV == 0, 
        0, 
        ifelse(
          dplyr::between(cchs_input$PAADVTRV, 1, 10080) & dplyr::between(cchs_input$PAATTRAV, 1, 7), 
          round(cchs_input$PAADVTRV/cchs_input$PAATTRAV, digits = 2), 
          8888.8
          )
        )
      )
    )
cchs_input$PAADVARC <- 
  ifelse(
    cchs_input$DOPAA == "No" | cchs_input$DHH_AGE < 18, 
    9999.6, 
    ifelse(
      cchs_input$PAADVREC == 99999 | cchs_input$PAATREC == 9, 
      9999.9, 
      ifelse(
        cchs_input$PAADVREC == 0, 
        0, 
        ifelse(
          cchs_input$PAADVREC >= 1 & cchs_input$PAADVREC <= 10080 & cchs_input$PAATREC >= 1 & cchs_input$PAADVREC <= 7, 
          round(cchs_input$PAADVREC/cchs_input$PAATREC, digits = 2), 
          cchs_input$PAADVARC
          )
        )
      )
    )
cchs_input$PAADVATH <- 
  ifelse(
    cchs_input$DOPAA == "No" | cchs_input$DHH_AGE < 18, 
    9999.6, 
    ifelse(
      cchs_input$PAADVOTH == 99999 | cchs_input$PAATOTH == 9, 
      9999.9, 
      ifelse(
        cchs_input$PAADVOTH == 0, 
        0, 
        ifelse(
          cchs_input$PAADVOTH >= 1 & cchs_input$PAADVOTH <= 10080 & cchs_input$PAATOTH >= 1 & cchs_input$PAATOTH <= 7, 
          round(cchs_input$PAADVOTH/cchs_input$PAATOTH, digits = 1), 
          8888.8
          )
        )
      )
    )

# Item 010
cchs_input$SXBDVPRT <- ifelse(cchs_input$DHH_AGE < 15 | cchs_input$DHH_AGE > 64, 9996, cchs_input$SXBDVPRT)
cchs_input$SXBDVSTI <- as.factor(ifelse(cchs_input$DHH_AGE < 15 | cchs_input$DHH_AGE > 64, "Valid skip", as.character(cchs_input$SXBDVSTI)))
cchs_input$SXBDVTST <- as.factor(ifelse(cchs_input$DHH_AGE < 15 | cchs_input$DHH_AGE > 64, "Valid skip", as.character(cchs_input$SXBDVTST)))

# Item 011 - not possible to fix, affected variables: LOP_015, affects 38 cases

# Item 012
cchs_input$MEXDVBM6 <- as.factor(ifelse(cchs_input$MEX_100 == "No", "Has not breastfed her last baby at all", as.character(cchs_input$MEXDVBM6)))

# Item 013 - not possible to fix, affected variables: SDC_020A-K/SDCDVCGT, affects 145 responses

# Item 014
cchs_input$DHHDVAOS <- ifelse(cchs_input$DHHDVAOS < 16, 999, cchs_input$DHHDVAOS)

# Item 015
cchs_input$FLU_025A <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025A)))
cchs_input$FLU_025B <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025B)))
cchs_input$FLU_025C <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025C)))
cchs_input$FLU_025D <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025D)))
cchs_input$FLU_025E <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025E)))
cchs_input$FLU_025F <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025F)))
cchs_input$FLU_025G <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025G)))
cchs_input$FLU_025H <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025H)))
cchs_input$FLU_025I <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025I)))
cchs_input$FLU_025J <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025J)))
cchs_input$FLU_025K <- as.factor(ifelse(
  cchs_input$FLU_015 %in% c("Don't know", "Refusal", "Not stated"), "Valid skip", as.character(cchs_input$FLU_025K)))

# Item 016
cchs_input$GEN_025 <- 
  as.factor(
    ifelse(
      cchs_input$ADM_PRX == "No" & !(cchs_input$MAC_005 %in% c("Working at a paid job or business", "Vacation (from paid work)")) &
        !(cchs_input$MAC_010 %in% c("Yes", "Don't know", "Refusal")), 
      "Valid skip", 
      as.character(cchs_input$GEN_025)
      )
    )

return(cchs_input)

}

