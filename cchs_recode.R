cchs_recode <-
  function (dataframe) {
  # recoding variables for analysis (mostly to dichotomous variables)
  recode_cchs <-
  # Reference/basis for most SDOH: http://www.ottawapublichealth.ca/en/reports-research-and-statistics/resources/Documents/mental_health_report_2018_en.pdf
    dplyr::mutate(dataframe,
      rural=as.factor(GEODVUR2),
      education=as.factor(EHG2DVR3),
      sex=as.factor(DHH_SEX),
      agegrp_3=
        as.factor(
          ifelse(
            DHH_AGE<=29,
            "12-29",
              ifelse(
                DHH_AGE<=64,
                "30-64",
                "65+"
              ))),
      agegrp_intermahp=
        as.factor(
          ifelse(
            DHH_AGE<15, NA, ifelse(
            DHH_AGE<=34, "15-34", ifelse(
              DHH_AGE<=64, 
              "35-64",
              "65+"
            )))),
      agegrp_4b=
        as.factor(
          ifelse(
            DHH_AGE<=19,
            "12-19",
            ifelse(
              DHH_AGE<=44,
              "20-44",
              ifelse(
                DHH_AGE<=64,
                "45-64",
                "65+"
              )))),
      agegrp_4a=
        as.factor(
          ifelse(
            DHH_AGE<=29,
            "12-29",
            ifelse(
              DHH_AGE<=49,
              "30-49",
              ifelse(
                DHH_AGE<=64,
                "50-64",
                "65+"
              )))),
      agegrp_6= as.factor(
        dplyr::case_when(
          DHH_AGE<=19 ~ "12-19",
          DHH_AGE<=34 ~ "20-34",
          DHH_AGE<=49 ~ "35-49",
          DHH_AGE<=64 ~ "50-64",
          DHH_AGE<=79 ~ "65-79",
          DHH_AGE>=80 ~ "80+")
      ),
      agegrp_5= as.factor(
        dplyr::case_when(
          DHH_AGE<=19 ~ "12-19",
          DHH_AGE<=34 ~ "20-34",
          DHH_AGE<=49 ~ "35-49",
          DHH_AGE<=64 ~ "50-64",
          DHH_AGE>=65 ~ "65+")
      ),
      agegrp_8= as.factor(
        dplyr::case_when(
          DHH_AGE<=19 ~ "12-19",
          DHH_AGE<=29 ~ "20-29",
          DHH_AGE<=39 ~ "30-39",
          DHH_AGE<=49 ~ "40-49",
          DHH_AGE<=59 ~ "50-59",
          DHH_AGE<=69 ~ "60-69",
          DHH_AGE<=79 ~ "70-79",
          DHH_AGE>=80 ~ "80+")
      ),
      agegrp_alc=
        as.factor(
          ifelse(
            DHH_AGE<=18,
            "12-18",
            ifelse(
              DHH_AGE<=44,
              "19-44",
              ifelse(
                DHH_AGE<=64,
                "45-64",
                "65+"
              )))),
      ownhome=dplyr::recode(DHH_OWN,`Owned by member of hhld, even if it is still being paid for`="Owned",`Rented, even if no cash rent is paid`="Rented"),
      mothertongue=forcats::fct_collapse(SDCDVFL1,English=c("English","English and Other"),French=c("French","French and Other"),`English and French`=c("English and French","English, French and Other")),
      livingsit=as.factor(ifelse(
        DHHDVLVG %in% c("Single parent living with children","Child living with a single parent","Child living with a single parent and siblings"),
        "Single parent and children",
        ifelse(
          DHHDVLVG %in% c("Parent living with spouse / partner and children","Child living with two parents","Child living with two parents and siblings"),
          "Parents and children",
          ifelse(
            DHHDVLVG %in% c("Unattached individual living with others","Individual living with spouse / partner"),
            "Living with others",
            ifelse(
              DHHDVLVG=="Unattached individual living alone",
              "Living alone",
              ifelse(
                DHHDVLVG=="Other","Other",NA)
              ))))),
      gen_030_rev=forcats::fct_collapse(GEN_030,Strong=c("Very strong","Somewhat strong"), Weak=c("Somewhat weak","Very weak")),
      gendvswl_rev=forcats::fct_collapse(GENDVSWL,Satisfied=c("Satisfied","Very Satisfied"), `Not Satified`=c("Neither satisfied nor dissatisfied","Dissatisfied","Very Dissatisfied")),
      depdvsev_rev=forcats::fct_collapse(DEPDVSEV,`Moderate to severe depression`=c("Moderate depression","Moderately severe depression","Severe depression")),
      dep=forcats::fct_collapse(depdvsev_rev,`Mild to severe depression`=c("Mild depression","Moderate to severe depression")),
      smk_agefirst=as.factor(ifelse(
        SMK_035<=15, "15 or less", ifelse(
          SMK_035<=17, "16-17", ifelse(
            SMK_035<=20, "18-20", "21+"
          )
        )
      )),
      smk_agedaily=as.factor(ifelse(
        SMK_040<=15, "15 or less", ifelse(
          SMK_040<=17, "16-17", ifelse(
            SMK_040<=20, "18-20", "21+"
          )
        )
      )),
      ets_nonsmkr=as.factor(ifelse(
        SMK_005=="Not at all", as.character(ETS_005), NA
      )),
      smk_status3=
        as.factor(
          ifelse(
            stringr::str_detect(SMKDVSTY, "Current"),"Current", ifelse(
              stringr::str_detect(SMKDVSTY, "Former"), "Former", "Non-smoker")
            )
          ),
      alc_day1= ifelse(ALW_010>0,1,0),
      alc_day2= ifelse(ALW_015>0,1,0),
      alc_day3= ifelse(ALW_020>0,1,0),
      alc_day4= ifelse(ALW_025>0,1,0),
      alc_day5= ifelse(ALW_030>0,1,0),
      alc_day6= ifelse(ALW_035>0,1,0),
      alc_day7= ifelse(ALW_040>0,1,0),
      alc_days = alc_day1+alc_day2+alc_day3+alc_day4+alc_day5+alc_day6+alc_day7,
      flag_preg = ifelse(is.na(MAC_025),"No",ifelse(MAC_025=="Yes","Yes","No")),
      flag_bf = ifelse(is.na(MEX_110),"No",ifelse(MEX_110=="Yes","Yes","No")),
      flag_preg_bf = ifelse(flag_preg=="Yes"|flag_bf=="Yes","Yes","No"),
      alc_exceedlowrisk1 = ifelse(DHH_AGE<19, NA, ifelse(flag_preg_bf == "Yes", NA, ifelse(
        ALC_005 == "Yes" & ALC_010 == "Yes" & ALW_005 == "Yes", ifelse(
          alc_days>5, "Yes", ifelse(
            sex=="Female", ifelse(
              ALW_010>2|ALW_015>2|ALW_020>2|ALW_025>2|ALW_030>2|ALW_035>2|ALW_040>2|ALWDVWKY>10,"Yes","No"),
            ifelse(ALW_010>3|ALW_015>3|ALW_020>3|ALW_025>3|ALW_030>3|ALW_035>3|ALW_040>3|ALWDVWKY>15,"Yes","No")
          )),"No"))),
      alc_exceedlowrisk2 = ifelse(
        DHH_AGE<19, NA, ifelse(
          flag_preg_bf == "Yes", NA, ifelse(
            ALC_005 == "Yes" & ALC_010 == "Yes", ifelse(
              ALC_020 == "Never", "No","Yes"),"No"))),
      alc_exceedlowrisk = ifelse(
        DHH_AGE<19, NA, ifelse(
          flag_preg_bf == "Yes", NA, ifelse(
            alc_exceedlowrisk1=="Yes"|alc_exceedlowrisk2=="Yes","Yes",ifelse(is.na(alc_exceedlowrisk1) | is.na(alc_exceedlowrisk2), NA, "No")))),
      alc_heavy = ifelse(ALC_010 == "Yes" & ALC_005 == "Yes", ifelse(
        stringr::str_detect(ALC_020,"Never|Less than once"),"No", "Yes"),"No"),
      alc_underage = ifelse(DHH_AGE<19, ifelse(ALC_005=="Yes", ifelse(ALC_010=="Yes","Yes","No"), "No"), NA),
      alc_bingecat = as.factor(ifelse(
            !is.na(ALC_020), ifelse(
              ALC_020 == "Never", "No binge drinking in past year", ifelse(
                ALC_020 == "Less than once a month", "Less than once a month", "Once a month or more")), 
            ifelse(
              ALC_005 == "No"|ALC_010 == "No", "Did not drink in the past year", NA))),
      alc_status = 
        as.factor(
          ifelse(
              ALC_005=="No", "Never", ifelse(
                  ALC_005=="Yes", ifelse(ALC_010=="No", "Former", ifelse(ALC_010=="Yes", "Current", NA)), NA))),
      alc_freq =
        as.factor(ifelse(
          flag_preg_bf == "Yes", NA, ifelse(
            !is.na(ALC_015), ifelse(
              ALC_015=="Less than once a month", "Less than once a month", ifelse(
                ALC_015 %in% c("Once a month", "2 to 3 times a month"), "1 to 3 times a month", ifelse(
                  ALC_015 %in% c("Once a week", "2 to 3 times a week"), "1 to 3 times a week", ifelse(
                    ALC_015 %in% c("4 to 6 times a week", "Every day"), "More than 3 times a week", NA)))), 
            ifelse(
              ALC_005=="No", "No alcoholic drinks (lifetime)", ifelse(
                ALC_005=="Yes" & ALC_010=="No", "No alcoholic drinks in past year", NA
              )
            )
          )
        )
        ),
      drguse_lt=as.factor(ifelse(
        DRGDVLCO == "Has used cocaine or crack" | DRGDVLAM == "Has used amphetamines" | DRGDVLEX == "Has used MDMA (ecstasy)" | 
          DRGDVLHA == "Has used hallucinogens, PCP, or LSD"| DRGDVLGL =="Has sniffed glue, gasoline or other solvents" | DRGDVINJ == "Has injected drugs", "Has used illicit drugs",ifelse(
            DRGDVLCO == "Has never used cocaine or crack" | DRGDVLAM == "Has never used amphetamines" | DRGDVLEX == "Has never used MDMA (ecstasy)" | 
              DRGDVLHA == "Has never used hallucinogens, PCP, or LSD"| DRGDVLGL =="Has never sniffed glue, gasoline or other solvents" | DRGDVINJ == "Has never injected drugs", "Has never used illicit drugs", NA)
      )),
      drg_025_rev=as.factor(ifelse(DRG_020=="No","No",as.character(DRG_025))),
      drg_035_rev=as.factor(ifelse(DRG_030=="No","No",as.character(DRG_035))),
      drg_045_rev=as.factor(ifelse(DRG_040=="No","No",as.character(DRG_045))),
      drg_055_rev=as.factor(ifelse(DRG_050=="No","No",as.character(DRG_055))),
      drg_065_rev=as.factor(ifelse(DRG_060=="No","No",as.character(DRG_065))),
      drg_075_rev=as.factor(ifelse(DRG_070=="No","No",as.character(DRG_075))),
      drguse_yr=as.factor(
        ifelse(drg_025_rev=="Yes"|drg_035_rev=="Yes"|drg_045_rev=="Yes"|drg_055_rev=="Yes"|drg_065_rev=="Yes"|drg_075_rev=="Yes","Yes",ifelse(
          drg_025_rev=="No",ifelse(
            drg_035_rev=="No",ifelse(
              drg_045_rev=="No",ifelse(
                drg_055_rev=="No",ifelse(
                  drg_065_rev=="No",ifelse(
                    drg_075_rev=="No", "No", NA), NA), NA), NA), NA), NA))),
      sui_con_1yr=as.factor(ifelse(SUI_005=="No","No",SUI_010)),
      sui_plan=as.factor(ifelse(SUI_005=="No","No",SUI_020)),
      sui_attempt=as.factor(ifelse(SUI_005=="No","No",SUI_035)),
      sui_medatt=as.factor(ifelse(SUI_005=="No","No",SUI_055)),
      sui_con_age=ifelse(SUI_010=="Yes",DHH_AGE,SUI_015),
      sui_con_agegrp=as.factor(dplyr::case_when(
        sui_con_age<29 ~ "15-29",
        sui_con_age<49 ~ "30-49",
        sui_con_age<64 ~ "50-64",
        sui_con_age<91 ~ "65+")),
      sui_plan_age=ifelse(SUI_025=="Yes",DHH_AGE,SUI_015),
      sui_plan_agegrp=as.factor(dplyr::case_when(
        sui_plan_age<29 ~ "15-29",
        sui_plan_age<49 ~ "30-49",
        sui_plan_age<64 ~ "50-64",
        sui_plan_age<91 ~ "65+")),
      sui_attempt_age=ifelse(SUI_045=="Yes",DHH_AGE,SUI_015),
      sui_attempt_agegrp=as.factor(dplyr::case_when(
        sui_attempt_age<29 ~ "15-29",
        sui_attempt_age<49 ~ "30-49",
        sui_attempt_age<64 ~ "50-64",
        sui_attempt_age<91 ~ "65+"))
      )
  
  recode_cchs1 <-
    dplyr::mutate_at(recode_cchs, c("GEN_005","GEN_015","GENDVHDI","GENDVMHI"),list(rev = ~forcats::fct_collapse(., `Very good or excellent`=c("Very good","Excellent"), `Not very good`=c("Good","Fair","Poor"))))
  
  recode_cchs2 <-    
    dplyr::mutate_at(recode_cchs1, c("GEN_020","GEN_025"), list(rev = ~ forcats::fct_collapse(., `Stressful`=c("Quite a bit stressful","Extremely stressful"), `Not stressful`=c("A bit stressful","Not very stressful","Not at all stressful"))))
  
  cchs_recode <-
    dplyr::rename_at(recode_cchs2, dplyr::vars(dplyr::contains("_rev")), ~stringr::str_to_lower(.))
  
  return(cchs_recode) 
}