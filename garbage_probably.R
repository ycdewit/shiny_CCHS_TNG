dd_ids <-
  data.frame(variable=c("ONT_ID", "GEODVD11", "GEODVFED", "GEODVCSD", "GEODVCD"),  type=c("id"), response_options=c("Corresponding ID value"), stringsAsFactors = FALSE)

cchs_fixchar <- 
  cchs_test %>% 
  select(-c("ONT_ID", "GEODVD11", "GEODVFED", "GEODVCSD", "GEODVCD")) %>% 
  mutate_if(is.character, ~as.factor(.))

factors_var <- 
  cchs_fixchar %>%
  select_if(is.factor) %>%
  summarise_all(list(~list(levels(.))))

numeric_min <-
  cchs_fixchar %>%
  select_if(is.numeric) %>%
  summarise_all(~min(., na.rm = TRUE))

numeric_max <-
  cchs_fixchar %>%
  select_if(is.numeric) %>%
  summarise_all(~max(., na.rm = TRUE))

dd_numeric <- as.data.frame(t(rbind(numeric_min,numeric_max))) %>% rownames_to_column(var="variable") %>%
  mutate(type="numeric", response_options=paste0("number: min=",V1,", max=", V2)) %>%
  select(-V1, -V2)

dd_factors <- as.data.frame(t(factors_var)) %>% 
  rownames_to_column(var="variable") %>% 
  mutate(
    type="factor",
    response_options=str_replace_all(paste(V1),"c(|)|\"|\\\\","")) %>% select(-V1)

CCHS_vars <- read_xlsx("CCHS_var_desc.xlsx")

hs_filetype <- guess_type(hs_filepath, mime_extra = c(dta="Stata", rds="R data file", sav="SPSS"))
if(hs_filetype=="Stata"){
  output$cchs_data <- renderDataTable(read.dta(hs_filepath))
}
else if (hs_filetype=="SPSS"){
  output$cchs_data <- renderDataTable(read.spss(hs_filepath, to.data.frame = TRUE))
}
else if (hs_filetype=="R data file"){
  output$cchs_data <- renderDataTable(readRDS(hs_filepath))
}
else if(hs_filetype=="text/csv"){
  output$cchs_data <- renderDataTable(read.csv(hs_filepath))
}
else {
  output$input_error <- renderText("Error: Please select a Stata, SPSS, R, or csv file (*.dta,*.sav,*.rds, *.csv)")
  show(input_error)
}




in_data <- reactive({
  datapath <- paste0(parseFilePaths(volumes, input$hs_file)$datapath)
  df <- read.dta(datapath)
  return(df)
})

observeEvent(input$hs_file, {
  if(str_detect(paste0(parseFilePaths(volumes, input$hs_file)$datapath),".")){
    phu_list <- in_data()$GEODVHR4
    updateCheckboxGroupInput(session,"phu", choices = phu_list)
  }
  else {return(NULL)}
})