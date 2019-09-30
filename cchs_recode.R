cchs_recode <-
  function (dataframe) {
# recoding variables for analysis (mostly to dichotomous variables)
recode_cchs <-
  dataframe %>%
# SDOH and/or stratifying/subset variables
# Reference/basis for most SDOH: http://www.ottawapublichealth.ca/en/reports-research-and-statistics/resources/Documents/mental_health_report_2018_en.pdf
  mutate(
    agegrpmh=
      as_factor(
        if_else(
          DHH_AGE<=19,
          "12-19",
          if_else(
            DHH_AGE<=44,
            "20-44",
            if_else(
              DHH_AGE<=64,
              "45-64",
              "65+"
            )))),
    agegrp4=
      as_factor(
        if_else(
          DHH_AGE<=29,
          "12-29",
          if_else(
            DHH_AGE<=49,
            "30-49",
            if_else(
              DHH_AGE<=64,
              "50-64",
              "65+"
            )))),
    agegrpmh2= as_factor(
      case_when(
        DHH_AGE<=19 ~ "12-19",
        DHH_AGE<=34 ~ "20-34",
        DHH_AGE<=49 ~ "35-49",
        DHH_AGE<=64 ~ "50-64",
        DHH_AGE<=79 ~ "65-79",
        DHH_AGE>=80 ~ "80+"),
    ),
    ownhome=recode(DHH_OWN,`Owned by member of hhld, even if it is still being paid for`="Owned",`Rented, even if no cash rent is paid`="Rented"),
    mothertongue=fct_collapse(SDCDVLHM,English=c("English","English and Other"),French=c("French","French and Other"),`English and French`=c("English and French","English, French and Other")),
    aboriginal=fct_collapse(SDCDVABT,zno=c("Non-Aboriginal identity")),
    livingsit=as.factor(if_else(
      DHHDVLVG %in% c("Single parent living with children","Child living with a single parent","Child living with a single parent and siblings"),
      "Single parent and children",
      if_else(
        DHHDVLVG %in% c("Parent living with spouse / partner and children","Child living with two parents","Child living with two parents and siblings"),
        "Parents and children",
        if_else(
          DHHDVLVG %in% c("Unattached individual living with others","Individual living with spouse / partner"),
          "Living with others",
          if_else(
            DHHDVLVG=="Unattached individual living alone",
            "Living alone",
            if_else(
              DHHDVLVG=="Other","Other",NULL)
            )))))
    )%>%
    rename(rural="GEODVUR2",education="EHG2DVR3",sex="DHH_SEX")%>%
# GEN Recodes
    mutate_at(c("GEN_005","GEN_015","GENDVHDI","GENDVMHI"),fct_collapse,`Very good or excellent`=c("Very good","Excellent"), zno=c("Good","Fair","Poor")) %>%
    mutate_at(c("GEN_020","GEN_025"),fct_collapse,`Stressful`=c("Quite a bit stressful","Extremely stressful"), zno=c("A bit stressful","Not very stressful","Not at all stressful"))%>%
    mutate(
      GEN_030=fct_collapse(GEN_030,Strong=c("Very strong","Somewhat strong"),zno=c("Somewhat weak","Very weak")),
      GENDVSWL=fct_collapse(GENDVSWL,Satisfied=c("Satisfied","Very Satisfied"),zno=c("Neither satisfied nor dissatisfied","Dissatisfied","Very Dissatisfied"))
    ) %>%
# Recode for fruit veg
    mutate(FVCDVGDT=fct_collapse(FVCDVGDT,`Eats fruits and vegetables more than 5 times per day`=c("Eats fruits and vegetables more than 10 times per day","Eats fruits and vegetables between 5 and 10 times per day")))%>%
    mutate(
      DEPDVSEV=fct_collapse(DEPDVSEV,`Moderate to severe depression`=c("Moderate depression","Moderately severe depression","Severe depression")),
      depsev=fct_collapse(DEPDVSEV,`Mild to severe depression`=c("Mild depression","Moderate to severe depression")),
    ) %>%
# Recode for smoke age
    mutate(
      smk_agefirst=ifelse(
        SMK_035<=15, "15 or less", ifelse(
          SMK_035<=17, "16-17", ifelse(
            SMK_035<=20, "18-20", "21+"
            )
          )
        ),
      smk_agedaily=ifelse(
        SMK_035<=15, "15 or less", ifelse(
          SMK_035<=17, "16-17", ifelse(
            SMK_035<=20, "18-20", "21+"
          )
        )
      ),
      smk_ets005=ifelse(
        SMK_005=="Not at all", ETS_005, NA
        )
    ) %>%
# Recode for drugs
    mutate(
      drg_use_lt=if_else(
        DRGDVLCO == 1 | DRGDVLAM == 1 | DRGDVLEX == 1 | DRGDVLHA == 1 | DRGDVLGL == 1 | DRGDVINJ == 1, "Yes",if_else(
          DRGDVLCO == 2 & DRGDVLAM == 2 & DRGDVLEX == 2 & DRGDVLHA == 2 & DRGDVLGL == 2 & DRGDVINJ == 2, "No", NULL)
      ),
      drg_use_yr= if_else(
        DRG_025 == 1 | DRG_035 == 1 | DRG_045 == 1 | DRG_055== 1 | DRG_065 == 1 | DRG_075 == 1, "Yes",if_else(
          DRG_025 == 2 & DRG_035 == 2 & DRG_045 == 2 & DRG_055 == 2 & DRG_065 == 2 & DRG_075 == 2, "No", NULL)
      ),
      drguselt=as_factor(if_else(
        DRGDVLCO == "Has used cocaine or crack" | DRGDVLAM == "Has used amphetamines" | DRGDVLEX == "Has used MDMA (ecstasy)" | 
          DRGDVLHA == "Has used hallucinogens, PCP, or LSD"| DRGDVLGL =="Has sniffed glue, gasoline or other solvents" | DRGDVINJ == "Has injected drugs", "Has used illicit drugs",if_else(
            DRGDVLCO == "Has never used cocaine or crack" | DRGDVLAM == "Has never used amphetamines" | DRGDVLEX == "Has never used MDMA (ecstasy)" | 
              DRGDVLHA == "Has never used hallucinogens, PCP, or LSD"| DRGDVLGL =="Has never sniffed glue, gasoline or other solvents" | DRGDVINJ == "Has never injected drugs", "Has never used illicit drugs", NULL)
      )),
      DRG_0251=as.factor(if_else(DRG_020=="No","No",as.character(DRG_025))),
      DRG_0351=as.factor(if_else(DRG_030=="No","No",as.character(DRG_035))),
      DRG_0451=as.factor(if_else(DRG_040=="No","No",as.character(DRG_045))),
      DRG_0551=as.factor(if_else(DRG_050=="No","No",as.character(DRG_055))),
      DRG_0651=as.factor(if_else(DRG_060=="No","No",as.character(DRG_065))),
      DRG_0751=as.factor(if_else(DRG_070=="No","No",as.character(DRG_075))),
      drguse_yr=as_factor(
        if_else(DRG_0251=="Yes"|DRG_0351=="Yes"|DRG_0451=="Yes"|DRG_0551=="Yes"|DRG_0651=="Yes"|DRG_0751=="Yes","Yes",if_else(
          DRG_0251=="No",if_else(
            DRG_0351=="No",if_else(
              DRG_0451=="No",if_else(
                DRG_0551=="No",if_else(
                  DRG_0651=="No",if_else(
                    DRG_0751=="No","No",NULL),NULL),NULL),NULL),NULL),NULL),NULL)),
#Recode for Suicide
      sui_con_1yr=as.factor(ifelse(SUI_005=="No","No",SUI_010)),
      sui_plan=as.factor(ifelse(SUI_005=="No","No",SUI_020)),
      sui_attempt=as.factor(ifelse(SUI_005=="No","No",SUI_035)),
      sui_medatt=as.factor(ifelse(SUI_005=="No","No",SUI_055)),
      sui_con_age=ifelse(SUI_010=="Yes",DHH_AGE,SUI_015),
      sui_con_agegrp=as.factor(case_when(
        sui_con_age<29 ~ "15-29",
        sui_con_age<49 ~ "30-49",
        sui_con_age<64 ~ "50-64",
        sui_con_age<91 ~ "65+")),
      sui_plan_age=ifelse(SUI_025=="Yes",DHH_AGE,SUI_015),
      sui_plan_agegrp=as.factor(case_when(
        sui_plan_age<29 ~ "15-29",
        sui_plan_age<49 ~ "30-49",
        sui_plan_age<64 ~ "50-64",
        sui_plan_age<91 ~ "65+")),
      sui_attempt_age=ifelse(SUI_045=="Yes",DHH_AGE,SUI_015),
      sui_attempt_agegrp=as.factor(case_when(
        sui_attempt_age<29 ~ "15-29",
        sui_attempt_age<49 ~ "30-49",
        sui_attempt_age<64 ~ "50-64",
        sui_attempt_age<91 ~ "65+")),
#Recode for alcohol
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
            sex=="Female", if_else(
              ALW_010>2|ALW_015>2|ALW_020>2|ALW_025>2|ALW_030>2|ALW_035>2|ALW_040>2|ALWDVWKY>10,"Yes","No"),
            if_else(ALW_010>3|ALW_015>3|ALW_020>3|ALW_025>3|ALW_030>3|ALW_035>3|ALW_040>3|ALWDVWKY>15,"Yes","No")
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
        str_detect(ALC_020,"Never|Less than once"),"No", "Yes"),"No"),
      alc_underage = ifelse(DHH_AGE<19, ifelse(ALC_005=="Yes", ifelse(ALC_010=="Yes","Yes","No"), "No"), NA),
      agegrp_alc=
        as_factor(
          if_else(
            DHH_AGE<=18,
            "12-18",
            if_else(
              DHH_AGE<=44,
              "19-44",
              if_else(
                DHH_AGE<=64,
                "45-64",
                "65+"
              ))))
      )
  }

