cchs_testdf <- readRDS("App-1/App-1/cchs_data.rds")

cchs_testdf <- make_std_agegrp(cchs_testdf, agegrp_starts=c(12,20,35,50,65), agegrp_ends=c(19,34,49,64,105), 
                               agegrp_names=c("12-19","20-34","35-49","50-64","65+"))

table(cchs_testdf$GEODVPG)

cchs_testdf$phu <- as.factor(ifelse(cchs_testdf$GEODVHR4=="Hastings and Prince Edward Counties HU","Yes","No"))

cchs_testdf$peer <- as.factor(ifelse(cchs_testdf$GEODVHR4!="Hastings and Prince Edward Counties HU" & cchs_testdf$GEODVPG=="Health Region Peer Group C","Yes","No"))

cchs_testdf$prov <- "Yes"



by_var_test <- c("DHH_SEX", "GEODVUR2")
question_test <- c("gendvswl_rev")


table(cchs_testdf$GEODVUR2, useNA = "ifany")
table(cchs_testdf$gendvswl_rev)

test_est <- cchs_est(test_survey, question_test)
test_estby <- cchs_estby(test_survey2, question=question_test, by_vars = by_var_test)

stand_pop <- cchs_can2011(minage=12, maxage=105, agegrp_starts=c(12,20,35,50,65), agegrp_ends=c(19,34,49,64,105), 
                         agegrp_names=c("12-19","20-34","35-49","50-64","65+"))

test_survey <- setup_design(cchs_testdf)
test_table <- cchs_table(dataframe=cchs_testdf, svy_design=test_survey2, questions=c("gendvswl_rev","gendvmhi_rev"), 
                         geo_vars=c("phu","peer"), by_vars=by_var_test, standardize=TRUE, 
                         stand_var="std_agegrp", stand_data="stand_pop", stand_pop="stdpop")