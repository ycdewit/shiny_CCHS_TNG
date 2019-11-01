cchs_testdf <- readRDS("GitHub/shiny_CCHS_TNG/cchs_data.rds")

cchs_testdf <- cchs_testdf %>% mutate(
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
    )
)

cchs_testdf$phu <- as.factor(ifelse(cchs_testdf$GEODVHR4=="Hastings and Prince Edward Counties HU","Yes","No"))

cchs_testdf$peer <- 
  as.factor(ifelse(cchs_testdf$GEODVHR4!="Hastings and Prince Edward Counties HU" & 
                     cchs_testdf$GEODVPG=="Health Region Peer Group C","Yes","No"))

cchs_testdf$prov <- "Yes"

cchs_testdf$phu_vs_prov <- as.factor(ifelse(cchs_testdf$GEODVHR4=="Hastings and Prince Edward Counties HU","phu","prov"))

cchs_testdf$phu_vs_peer <- as.factor(ifelse(cchs_testdf$GEODVHR4=="Hastings and Prince Edward Counties HU","phu",ifelse(
  cchs_testdf$GEODVPG=="Health Region Peer Group C","peer",NA)))

cchs_testdf <- make_std_agegrp(cchs_testdf, agegrp_starts=c(12,19,35,50,65), agegrp_ends=c(18,34,49,64,105), 
                               agegrp_names=c("12-19","20-34","35-49","50-64","65+"))

std_pop <- cchs_can2011(minage=12, maxage=105, agegrp_starts=c(12,19,35,50,65), agegrp_ends=c(18,34,49,64,105), 
                        agegrp_names=c("12-19","20-34","35-49","50-64","65+"))

cchs_testdf <- make_std_agegrp(cchs_testdf, agegrp_starts=c(15, 30, 45, 60, 75), agegrp_ends=c(29, 44, 59, 74, 105), 
                               agegrp_names=c("15-29","30-44","45-59","60-74", "75+"))

std_pop15 <- cchs_can2011(minage=12, maxage=105, agegrp_starts=c(15, 30, 45, 60, 75), agegrp_ends=c(29, 44, 59, 74, 105), 
                          agegrp_names=c("15-29","30-44","45-59","60-74", "75+"))

cchs_testdf <- make_std_agegrp(cchs_testdf, agegrp_starts=c(15,35,65), agegrp_ends=c(34,64,105), 
                               agegrp_names=c("15-34","35-64","65+"))

cchs_testdf$agegrp_intermahp <- as.factor(
  ifelse(
    cchs_testdf$DHH_AGE>=15 & cchs_testdf$DHH_AGE<=34, "15-34", ifelse(
      cchs_testdf$DHH_AGE>=35 & cchs_testdf$DHH_AGE<=64, "35-64", ifelse(
        cchs_testdf$DHH_AGE>=65, "65+", NA
        )
      )
    ))

std_pop19 <- cchs_can2011(minage=19, maxage=105, agegrp_starts=c(19,35,50,65), agegrp_ends=c(34,49,64,105), 
                        agegrp_names=c("12-19","19-34","35-49","50-64","65+"))

cchs_testdf$ALWDVWKY<-ifelse(cchs_testdf$ALWDVWKY>694, NA, cchs_testdf$ALWDVWKY)

test_survey <- setup_design(cchs_testdf)
test_survey_phu <- subset(test_survey, phu=="Yes")
test_survey_peer <- subset(test_survey, peer=="Yes")

library(dplyr)

cchs_testdf_F <- dplyr::filter(cchs_testdf, DHH_SEX=="Female") %>% mutate(ALWDVWKY=ifelse(ALWDVWKY>694, NA, ALWDVWKY))

test_survey_F <- setup_design(cchs_testdf_F)
test_survey_phu_F <- subset(test_survey_F, phu=="Yes")
test_survey_peer_F <- subset(test_survey_F, peer=="Yes")

cchs_testdf_M <- dplyr::filter(cchs_testdf, DHH_SEX=="Male") %>% mutate(ALWDVWKY=ifelse(ALWDVWKY>694, NA, ALWDVWKY))
test_survey_M <- setup_design(cchs_testdf_M)
test_survey_phu_M <- subset(test_survey_M, phu=="Yes")
test_survey_peer_M <- subset(test_survey_M, peer=="Yes")


test_est <- cchs_est(test_survey, "gendvswl_rev")
test_estby <- cchs_estby(test_survey, question="gendvswl_rev", by_vars = c("DHH_SEX", "std_agegrp"))

table(cchs_testdf$DRGDVLCA)
table(cchs_testdf$DRGDVLCM)

test_table <- 
  shiny_cchs_table(
    dataframe = cchs_testdf,
    svy_design_phu = test_survey_phu,
    svy_design_peer = test_survey_peer,
    svy_design_prov = test_survey,
    questions = c("DRGDVLCA", "DRGDVLCM"),
    by_vars = c("DHH_SEX", "std_agegrp"),
    crude = TRUE,
    standardize = TRUE,
    stand_data = std_pop,
    stand_pop = "stdpop",
    stand_var = "std_agegrp")

test_table_df <- dplyr::bind_rows(test_table)

cannabis_ests <- test_table
cannabis_RR <- test_table

test_table <- 
  cchs_rr(
    dataframe = cchs_testdf,
    questions = c("DRGDVLCA", "DRGDVLCM"),
    compareby = c(phu_vs_peer="peer", phu_vs_prov="prov"),
    groupby = c("DHH_SEX", "std_agegrp"),
    standardize = TRUE,
    stand_data = std_pop,
    stand_pop = "stdpop",
    stand_var = "std_agegrp")

survey_phu_under <- subset(test_survey, phu=="Yes",cchs_testdf$DHH_AGE<19)
survey_peer_under <- subset(test_survey, peer=="Yes",cchs_testdf$DHH_AGE<19)
survey_prov_under <- subset(test_survey, cchs_testdf$DHH_AGE<19)


cchs_19plus <- cchs_testdf[which(cchs_testdf$DHH_AGE>=19),]
cchs_19plus <- dplyr::mutate_if(cchs_19plus, is.factor, ~forcats::fct_drop(.))

survey_prov_19 <- setup_design(cchs_19plus)
survey_peer_19 <- subset(survey_prov_19, peer=="Yes",cchs_testdf$DHH_AGE>=19)
survey_phu_19 <- subset(survey_prov_19, phu=="Yes")

smk_std19plus <- 
  shiny_cchs_table(
    questions=c("SMKDVSTY", "smk_status3", "ets_nonsmkr", "smk_agedaily", "smk_agefirst"), 
    dataframe=cchs_19plus, svy_design_phu = survey_phu_19, svy_design_peer = survey_peer_19, svy_design_prov =survey_prov_19,
    crude=TRUE,
    standardize=TRUE, stand_data=std_pop19, stand_pop="stdpop", stand_var="std_agegrp")

smk_std19plus_by <- 
  shiny_cchs_table(
    questions=c("smk_status3"), 
    by_vars = c("DHH_SEX", "std_agegrp"),
    dataframe=cchs_19plus, svy_design_phu = survey_phu_19, svy_design_peer = survey_peer_19, svy_design_prov =survey_prov_19,
    crude=TRUE,
    standardize=TRUE, stand_data=std_pop19, stand_pop="stdpop", stand_var="std_agegrp")

table(cchs_testdf$SMKDVSTY)

smk_rr_by <- 
  cchs_rr(
    dataframe = cchs_19plus,
    questions = c(smk_status3="Current", SMKDVSTY="Current daily smoker"),
    compareby = c(phu_vs_peer="peer", phu_vs_prov="prov"),
    groupby = c("DHH_SEX", "std_agegrp"),
    standardize = TRUE,
    stand_data = std_pop,
    stand_pop = "stdpop",
    stand_var = "std_agegrp")

smk_rr <- 
  cchs_rr(
    dataframe = cchs_19plus,
    questions = c(ets_nonsmkr="Yes"),
    compareby = c(phu_vs_peer="peer", phu_vs_prov="prov"),
    groupby = c("DHH_SEX", "std_agegrp"),
    standardize = TRUE,
    stand_data = std_pop,
    stand_pop = "stdpop",
    stand_var = "std_agegrp")
system('CMD /C "ECHO The R process has finished running && PAUSE"',
       invisible=FALSE, wait=FALSE)

table(cchs_testdf$gendvmhi_rev)
table(cchs_testdf$gendvswl_rev)
table(cchs_testdf$gendvhdi_rev)
table(cchs_testdf$gen_020_rev)
table(cchs_testdf$CMH_005)
table(cchs_testdf$SUI_005)


mh_rr <- 
  cchs_rr(
    dataframe = cchs_testdf,
    questions = c(gendvmhi_rev="Very good or excellent", gendvhdi_rev="Very good or excellent",
                  gendvswl_rev="Satisfied", gen_030_rev="Strong", gen_030_rev="Stressful",
                  CCC_195="Yes", CCC_200="Yes", SUI_005="Yes"),
    compareby = c(phu_vs_peer="peer", phu_vs_prov="prov"),
    standardize = TRUE,
    stand_data = std_pop,
    stand_pop = "stdpop",
    stand_var = "std_agegrp")
system('CMD /C "ECHO The R process has finished running && PAUSE"',
       invisible=FALSE, wait=FALSE)

diabetes_rr <- 
  cchs_rr(
    dataframe = cchs_testdf,
    questions = c("CCC_095"),
    compareby = c(phu_vs_peer="peer", phu_vs_prov="prov"),
    standardize = TRUE,
    stand_data = std_pop,
    stand_pop = "stdpop",
    stand_var = "std_agegrp")
system('CMD /C "ECHO The R process has finished running && PAUSE"',
       invisible=FALSE, wait=FALSE)

fruitveg_rr <- 
  cchs_rr(
    dataframe = cchs_testdf,
    questions = c(FVCDVGDT="Eats fruits and vegetables less than 5 times per day"),
    compareby = c(phu_vs_peer="peer", phu_vs_prov="prov"),
    standardize = TRUE,
    stand_data = std_pop,
    stand_pop = "stdpop",
    stand_var = "std_agegrp")
system('CMD /C "ECHO The R process has finished running && PAUSE"',
       invisible=FALSE, wait=FALSE)

table(cchs_testdf$alc_bingecat)

binge_rr <- 
  cchs_rr(
    dataframe = cchs_testdf,
    questions = c(FVCDVGDT="Once a month or more"),
    compareby = c(phu_vs_peer="peer", phu_vs_prov="prov"),
    standardize = TRUE,
    stand_data = std_pop,
    stand_pop = "stdpop",
    stand_var = "std_agegrp")
system('CMD /C "ECHO The R process has finished running && PAUSE"',
       invisible=FALSE, wait=FALSE)

as.data.frame(table(cchs_19plus$ets_nonsmkr, cchs_19plus$std_agegrp))

smk_std19plus3 <- cchs_table(questions=c("smk_agedaily", "smk_agefirst"), by_vars = c("sex","INCDVSPR","INCDVSRS", "std_agegrp"),
                             dataframe=cchs_19plus, geo_vars=c("phu","peer","prov"),
                             standardize=TRUE, stand_data=standpop, stand_pop="stdpop", stand_var="std_agegrp")


smk_under19 <- cchs_table(questions=c("SMKDVSTY", "smk_status3", "smk_agefirst", "smk_agedaily", "ets_nonsmkr"), by_vars = c("sex","INCDVSPR","INCDVSRS", "std_agegrp"),
                          dataframe=cchs_under19, geo_vars=c("phu","peer","prov"),
                          standardize=FALSE)