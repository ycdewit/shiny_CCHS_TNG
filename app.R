package_list <- c("shiny", "shinyFiles", "shinyjs", "readxl", "foreign", "dplyr", "tibble", "stringr", "DT", "forcats", "reshape2", "survey")

get_packages <- function (packages) {
    for(package in packages) {
        if(! require(package, character.only = TRUE)){
            install.packages(package)
            require(package, character.only = TRUE)
        }
    }
}

get_packages(package_list)

# Define UI 
ui <- fluidPage(
    useShinyjs(),
    titlePanel(title=div(img(src = "HPE_LogoH_COL.png", width = 220), "CCHS 2015-16: The Next GenRation")),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel("Data Options",
                         div(id="prev", 
                             h4("Previous Data"),
                             checkboxInput("load_prev", label = "Load data from previous session", value = TRUE),
                             checkboxInput("delete_prev", label = "Delete data from previous session", value = FALSE)),
                         h4("Data files"),
                         strong("Please select CCHS data files"), 
                         br(),
                         em("Currently SPSS, STATA, R (.rds), Excel, and Comma-Separated data files are accepted. 
                            Will work with additional variables added but if any of the original variables are modified this tool may not work."),
                         br(),
                         br(),
                         shinyFilesButton('hs_file', label='Select CCHS 2015-16 Data File', title='Please select a file', multiple=FALSE),
                         textOutput("txt_hs"),
                         br(),
                         radioButtons("boot_yn", label = "Does this file contain the bootstrap weights (1000 variables starting with BSW)?",
                                      choices = list("Yes" = TRUE, "No" = FALSE), 
                                      selected = FALSE),
                         shinyFilesButton('bs_file', label='Bootstrap file for CCHS 2015-16', title='Please select a file', multiple=FALSE),
                         textOutput("txt_bs"),
                         textOutput("input_error"),
                         br(),
                         checkboxGroupInput("clean_options", label = h4("Additional Options for Data Processing"), 
                                            choices = list("Fix errata" = 1, "Include addition variable creation" = 2),
                                            selected = c(1,2)),
                         a("Click here",href="CCHS_Errata 2015-2017_Feb 2019.pdf"), "to view the version of the CCHS 2015-2016 errata used in this tool.",
                         a("Click here",href="additional variables.xlsx"), "to download data dictionary for additional computed variables.",
                         br(),
                         br(),
                         actionButton("submit", "Prepare CCHS Data", width = "100%")
                ),
                tabPanel(
                    "Setup Analysis",
                    br(),
                    actionButton("advanced", "Advanced Options"),
                    hidden(
                        div(id="adv_opt",
                            textInput("df_code", "R syntax for manipulating CCHS data (e.g. add new variables)")
                        )),
                    h4("Geographic Variables"),
                    selectInput("phu", label="Please select PHU", choices = NULL),
                    textInput("phu_label", "PHU Label for Tables and Graphs", placeholder = "Enter a short name for PHU"),
                    selectInput("peer", label="Please select Peer Group", choices = NULL),
                    "Not sure what Statistics Canada Peer Group your public health unit is in?", 
                    a("Click here", href="https://www150.statcan.gc.ca/n1/pub/82-402-x/2015001/regions/tbl/tbl8-eng.htm"), 
                    "to check.",
                    br(),
                    br(),
                    h4("CCHS Questions and Stratifiers"),
                    strong("Please select the question(s) to analyze."),
                    br(),
                    em("Start typing in the box below search by either by description or column (variable) name."),
                    selectizeInput("questions", NULL, NULL, multiple = TRUE),
                    strong("Please select any variables you want to group (stratify) the results by."),
                    selectizeInput("stratifiers", NULL, NULL, multiple = TRUE),
                    h4("Standardization Options"),
                    checkboxInput("crude", strong("Produce Crude Estimates"), TRUE),
                    checkboxInput("stand", strong("Produce Age-Standardized Estimates"), FALSE),
                    hidden(
                        div(id="std_ag",
                            hidden(
                                div(id="std_ag1",
                                    strong("Set Age Group"), br(),
                                    em("Specify the age group for age-standardization"),
                                    splitLayout(
                                        textInput("ag_name1", "Name", "", width="90%"),
                                        numericInput("ag_start1","Start Age", value=12, min=12, max=105, width="90%"), 
                                        numericInput("ag_end1","End Age", value=19, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag2",
                                    splitLayout(
                                        textInput("ag_name2", NULL, "", width="90%"),
                                        numericInput("ag_start2", NULL, value=20, min=12, max=105, width="90%"), 
                                        numericInput("ag_end2", NULL, value=34, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag3",
                                    splitLayout(
                                        textInput("ag_name3", NULL, "", width="90%"),
                                        numericInput("ag_start3", NULL, value=35, min=12, max=105, width="90%"), 
                                        numericInput("ag_end3", NULL, value=49, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag4",
                                    splitLayout(
                                        textInput("ag_name4", NULL, "", width="90%"),
                                        numericInput("ag_start4", NULL, value=50, min=12, max=105, width="90%"), 
                                        numericInput("ag_end4", NULL, value=64, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag5",
                                    splitLayout(
                                        textInput("ag_name5", NULL, "", width="90%"),
                                        numericInput("ag_start5", NULL, value=65, min=12, max=105, width="90%"), 
                                        numericInput("ag_end5", NULL, value=105, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag6",
                                    splitLayout(
                                        textInput("ag_name6", NULL, "", width="90%"),
                                        numericInput("ag_start6", NULL, value=NA, min=12, max=105, width="90%"), 
                                        numericInput("ag_end6", NULL, value=NA, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag7",
                                    splitLayout(
                                        textInput("ag_name7", NULL, "", width="90%"),
                                        numericInput("ag_start7", NULL, value=NA, min=12, max=105, width="90%"), 
                                        numericInput("ag_end7", NULL, value=NA, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag8",
                                    splitLayout(
                                        textInput("ag_name8", NULL, "", width="90%"),
                                        numericInput("ag_start8", NULL, value=NA, min=12, max=105, width="90%"), 
                                        numericInput("ag_end8", NULL, value=NA, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag9",
                                    splitLayout(
                                        textInput("ag_name9", NULL, "", width="90%"),
                                        numericInput("ag_start9", NULL, value=NA, min=12, max=105, width="90%"), 
                                        numericInput("ag_end9", NULL, value=NA, min=12, max=105, width="90%")))),
                            hidden(
                                div(id="std_ag10",
                                    splitLayout(
                                        textInput("ag_name10", NULL, "", width="90%"),
                                        numericInput("ag_start10", NULL, value=NA, min=12, max=105, width="90%"), 
                                        numericInput("ag_end10", NULL, value=NA, min=12, max=105, width="90%")))),
                            actionButton("more_ag","More Age Group Rows"), actionButton("less_ag","Less Age Group Rows")
                        )),
                    h4("Age Range and Sex"),
                    "Please select age range and sex(es) for Analyses.",
                    em("This is for restricting analyses to specific groups (e.g. adults 18+)"),
                    sliderInput("age_range", label = "Age Range", min = 10, 
                                max = 105, value = c(12, 105), step = 1),
                    checkboxGroupInput("choose_sex", label = h4("Sex"), 
                                       choices = list("Male" = "Male", "Female" = "Female"),
                                       selected = c("Male","Female")),
                    actionButton("submit_analysis", "Run", width = "100%")
                )
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Table",
                    DT::dataTableOutput("results_table"),
                    shinySaveButton(id="save_table", label="Export CCHS table", title=NULL, 
                                    filetype = list(excel=c('xlsx','xls'), R=c('rds'), csv=c('csv')))
                ),
                tabPanel("Graph"),
                tabPanel("Review loaded data",
                         h4("CCHS Data"),
                         textOutput("no_data"),     
                         DT::dataTableOutput("cchs_df"))
            )
        )
    )
)



# Define server logic 
server <- 
    
    shinyServer(function(input, output, session) {
        
        #file management
        volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
        shinyFileChoose(input, "hs_file", roots = c(volumes, "wd"="."), defaultRoot = "wd", session = session)
        shinyFileChoose(input, "bs_file", roots = c(volumes, "wd"="."), session = session)
        shinyFileSave(input, "save_table", roots = c(volumes), session = session)
            
        #Previous data    
        observe({    
            if (file.exists("cchs_data.rds")){
                cchs_df <<- readRDS("cchs_data.rds")
            }
            else {
                updateCheckboxInput(session, "load_prev", value=FALSE)
                shinyjs::hide("prev")
                cchs_df <<- NULL}
        }, priority = 30)
        
        observeEvent(input$delete_prev, {
            if(input$delete_prev==TRUE){
                file.remove("cchs_data.rds")
                cchs_df <<- NULL
                updateCheckboxInput(session, "load_prev", value=FALSE)
                shinyjs::hide("prev")
            }
        }, priority = 27)
        
        #Responsive Data Options Stuff
        observe({
            output$txt_hs <- renderText(paste0("Selected file: ",parseFilePaths(volumes, input$hs_file)$datapath))
        }, priority = -10)
        
        observe({
            output$txt_bs <- renderText(paste0("Selected file: ",parseFilePaths(volumes, input$bs_file)$datapath))
        }, priority = -10)
        
        observe({
            if(1 %in% input$clean_options){
                errata <<- TRUE
            }
            
            if (2 %in% input$clean_options) {
                include_recode <<- TRUE
            }
        }, priority = -10)
        
        observeEvent(input$boot_yn, {
            if(input$boot_yn==TRUE){
                shinyjs::hide("bs_file")  # hide is a shinyjs function
                shinyjs::hide("txt_bs")
            }
            else{
                shinyjs::show("bs_file")  # show is a shinyjs function
                shinyjs::show("txt_bs")
            }
        })
        
        #Source the code for data processing
        source("cchs_cleanup.R")
        source("cchs_errata.R")
        source("cchs_prep.R")
        
        #Process the data    
        observeEvent(input$submit, {
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Preparing data", detail= "This may take awhile, maybe go stretch your legs or something.", value = 0.1)
            # Close the progress when this reactive exits (even if there's an error)
            on.exit(progress$close())
            
            update_progress <- function(value = NULL, detail = NULL) {
                if (is.null(value)) {
                    value <- progress$getValue()
                    value <- value + (progress$getMax() - value) / 3
                }
                progress$set(value = value, detail = detail)
            }
            
            cchs_df <<-    
                cchs_prep(
                    cchsfile = parseFilePaths(volumes, input$hs_file)$datapath,
                    includes_bootwts = input$boot_yn,
                    bootwts = parseFilePaths(volumes, input$bs_file)$datapath,
                    run_errata = errata,
                    update_progress = update_progress)
            
            if (2 %in% input$clean_options){
                update_progress(detail = "Calculating additional variables")
                source("cchs_recode.R")
                cchs_df <- cchs_recode(cchs_df)
                update_progress(detail = "Finishing up")
            }
            
            saveRDS(cchs_df, "cchs_data.rds")
        }, priority=25)
        
        
        #Render data table (Main Panel)
        
        observe({
            input$submit
            input$load_prev
            
            if(is.null(cchs_df)){
                output$no_data <-  renderText("No data detected. Please use sidebar to select data files and process data.")
            }
            
            output$cchs_df <- 
                DT::renderDataTable(cchs_df)
            
            if(!is.null(cchs_df)){
                svy_design <<- setup_design(cchs_df)}
            
        }, priority = 20)
        
        #Source code I need for analyses
        source("utils.R")
        source("cchs_table.R")
        source("cchs_rr.R")
        
        #Data dictionary stuff        
        additional_variables <- read_xlsx("www/additional variables.xlsx")
        
        cchs_var_desc <- read_xlsx("data/CCHS_var_desc.xlsx") %>%
            bind_rows(additional_variables) %>%
            select(variable_name, variable_desc, variable_group)
        
        observeEvent(input$advanced, {
            shinyjs::toggle("adv_opt")
        })
        
        #Responsive age standardization stuff 
        age_rows <<- 5
        
        observeEvent(input$more_ag, {
            age_rows <<- min(age_rows+1,10)
        })
        
        observeEvent(input$less_ag, {
            age_rows <<- max(age_rows-1,1)
        })
        
        observe({
            input$less_ag
            input$more_ag
            
            if(input$stand==TRUE){
            
                shinyjs::show("std_ag")
                    
                    for (i in 1:10) {
                        if(i<=age_rows){
                            shinyjs::show(eval(paste0("std_ag",i)))
                            
                            start_val <- paste0("ag_start",i)
                            end_val <- paste0("ag_end",i)
                            group_name <- paste0("ag_name",i)
                            
                            if(!is.na(input[[start_val]]) & !is.na(input[[end_val]])){
                                updateTextInput(session, group_name, value = paste0(input[[start_val]],"-",input[[end_val]]))
                            }
                            
                            if(i==1){
                                agegrp_starts <<- c(input[[start_val]])
                                agegrp_ends <<- c(input[[end_val]])
                                agegrp_names <<- c(input[[group_name]])
                            }
                            
                            else {
                                agegrp_starts <<- c(agegrp_starts, input[[start_val]])
                                agegrp_ends <<- c(agegrp_ends, input[[end_val]])
                                agegrp_names <<- c(agegrp_starts, input[[group_name]])
                            }
                        }
                        else {
                            shinyjs::hide(eval(paste0("std_ag",i)))
                        }
                        
                    }
                    minage <- min(agegrp_starts, na.rm = TRUE)
                    maxage <- max(agegrp_ends, na.rm = TRUE)
                    updateSliderInput(session, "age_range", value=c(minage, maxage))
                    cchs_df <<- make_std_agegrp(cchs_df, agegrp_starts = agegrp_starts, agegrp_ends = agegrp_ends, agegrp_names = agegrp_names)
                    print(table(cchs_df$std_agegrp))
                    svy_design <<- setup_design(cchs_df)
                    std_pop <<- cchs_can2011(minage = minage, maxage = maxage, agegrp_starts = agegrp_starts, agegrp_ends = agegrp_ends, agegrp_names = agegrp_names)
                    print(table(std_pop$std_agegrp))
                }
            
            else{shinyjs::hide("std_ag")}
                
            })
        
        #Responsive select/selectize inputs for "Setup Analyses" tab
        observe({
            input$submit
            if(!is.null(cchs_df)){
                updateSelectInput(session, "phu", choices = levels(cchs_df$GEODVHR4))
                updateSelectInput(session, "peer", choices = levels(cchs_df$GEODVPG))
                
                if(file.exists("selections.rds")){
                    selections <- readRDS("selections.rds")
                    updateSelectInput(session, "phu", selected = selections$phu)
                    updateSelectInput(session, "peer", selected = selections$peer)
                    
                    if("phu_label" %in% colnames(selections)){
                        updateTextInput(session, "phu_label", value = selections$phu_label)
                    }
                }
                
                cchs_dd <<- 
                    data.frame(variable_name = names(cchs_df), stringsAsFactors = FALSE) %>%
                    left_join(cchs_var_desc)
                
                updateSelectizeInput(session, "questions", choices = cchs_dd, options=list(
                    optgroups = lapply(unique(cchs_dd$variable_desc), function(x){
                        list(value = as.character(x), label = as.character(x))
                    }),
                    optgroupField = "variable_desc",
                    searchField = c("variable_name","variable_desc", "variable_group"),
                    labelField = c("variable_name"),
                    render = I("{
                        option: function(item, escape) {
                        return '<div>' + escape(item.variable_name) +'</div>';
                        }
                        }")
                ))
                
                updateSelectizeInput(session, "questions", choices = cbind(cchs_dd, value = seq_len(nrow(cchs_dd))), server = TRUE)
                
                updateSelectizeInput(session, "stratifiers", choices = cchs_dd, options=list(
                    optgroups = lapply(unique(cchs_dd$variable_desc), function(x){
                        list(value = as.character(x), label = as.character(x))
                    }),
                    optgroupField = "variable_desc",
                    searchField = c("variable_name","variable_desc", "variable_group"),
                    labelField = "variable_name",
                    render = I("{
                        option: function(item, escape) {
                        return '<div>' + escape(item.variable_name) +'</div>';
                        }
                        }")
                ))
                
                updateSelectizeInput(session, "stratifiers", choices = cbind(cchs_dd, value = seq_len(nrow(cchs_dd))), server = TRUE)
            }
            
        }, priority = 10)
        
        observeEvent(input$phu, {
            cchs_df$phu <<- as.factor(ifelse(cchs_df$GEODVHR4==input$phu,"Yes","No"))
            
            cchs_df$prov <<- "Yes"
            
            cchs_df$phu_vs_prov <<- as.factor(ifelse(cchs_df$GEODVHR4==input$phu,"phu","prov"))
            
            if(exists("svy_design")){
            svy_design <<- update(svy_design, phu = cchs_df$phu)
            svy_design <<- update(svy_design, prov = cchs_df$prov)
            svy_design <<- update(svy_design, phu_vs_prov = cchs_df$phu_vs_prov)
            }
        })
        
        observeEvent(input$peer, {
            cchs_df$peer <<- as.factor(ifelse(cchs_df$GEODVHR4!=input$phu & cchs_df$GEODVPG==input$peer,"Yes","No"))
            
            cchs_df$phu_vs_peer <<- as.factor(ifelse(cchs_df$GEODVHR4==input$phu,"phu",ifelse(
                cchs_df$GEODVPG==input$peer,"peer",NA)))
            
            if(exists("svy_design")){
            svy_design <<- update(svy_design, peer = cchs_df$peer)
            svy_design <<- update(svy_design, phu_vs_peer = cchs_df$phu_vs_peer)
            }
        })
        
        observeEvent(input$submit_analysis, {
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Running analyses", detail= "This will take awhile. Maybe take a walk, do something else and come back in a bit.", value = 0.1)
            # Close the progress when this reactive exits (even if there's an error)
            on.exit(progress$close())
            
            update_progress <- function(value = NULL, detail = NULL) {
                if (is.null(value)) {
                    value <- progress$getValue()
                    value <- value + (progress$getMax() - value) / 5
                }
                progress$set(value = value, detail = detail)
            }
            
            in_questions <- unlist(cchs_dd[input$questions,"variable_name"])
            
            if(!is.null(input$stratifiers)){
                in_byvars <- unlist(cchs_dd[input$stratifiers,"variable_name"])}
            else in_byvars <- NULL
            
            cchs_svy <- 
                subset(
                    svy_design, 
                    (DHH_SEX %in% input$choose_sex) & DHH_AGE>=min(input$age_range) & DHH_AGE<=max(input$age_range))
            
            if(input$stand==TRUE){
                in_standdata <- std_pop
                in_standpop <- "stdpop"
                in_standvar <- "std_agegrp"
            }
            else {
                in_standdata <- NULL
                in_standpop <- NULL
                in_standvar <- NULL}
            
            cchs_svy <<- update(cchs_svy, std_agegrp=cchs_df$std_agegrp)
            print(table(cchs_svy[["variables"]][["std_agegrp"]]))
            
            cchs_svy_phu <-
                subset(cchs_svy, phu=="Yes")
            
            cchs_svy_peer <-
                subset(cchs_svy, peer=="Yes")
            
            results <<- 
                cchs_table(questions = in_questions,
                           svy_design_phu = cchs_svy_phu, 
                           svy_design_peer = cchs_svy_peer, 
                           svy_design_prov = cchs_svy,
                           by_vars = in_byvars,
                           crude = input$crude,
                           standardize=input$stand, stand_data=in_standdata, stand_pop=in_standpop, stand_var=in_standvar, 
                           dataframe = cchs_df,
                           update_progress = update_progress)
            
            clean_results <- 
                dplyr::bind_rows(results) %>% dplyr::select(-CV) %>%
                dplyr::mutate_at(c("Estimate", "Lower 95% CI", "Upper 95% CI"), ~round(., digits= 1))
            
            clean_wide <-
                clean_results %>%
                reshape2::melt(measure.vars=c("Estimate", "Lower 95% CI", "Upper 95% CI", "Quality Indicator"))
                
            clean_wide <- 
                mutate(
                    clean_wide,
                    variable=str_replace(variable,"_Crude|_Std|Std ",""))

            if(!is.null(in_byvars)){
                clean_wide <-
                    dcast(clean_wide, est_type + indicator + ind_level + stratifier + strat_level ~ geo_area + variable, value.var="value") 
            }
            else {
                clean_wide <-
                    dcast(clean_wide, est_type + indicator + ind_level ~ geo_area + variable, value.var="value")
            }
            
            clean_wide <<-
                mutate(
                    clean_wide, 
                    `Significance Compared to Ontario`=ifelse(
                        `phu_Lower 95% CI`>`prov_Upper 95% CI`, "Higher", ifelse(
                            `phu_Upper 95% CI`< `prov_Lower 95% CI`, "Lower", "None" 
                    )),
                    `Significance Compared to Peer`=ifelse(
                        `phu_Lower 95% CI`>`peer_Upper 95% CI`, "Higher", ifelse(
                            `phu_Upper 95% CI`< `peer_Lower 95% CI`, "Lower", "None" 
                        )),
                    `phu_95% CI` = paste0(`phu_Lower 95% CI`,"-", `phu_Upper 95% CI`),
                    `peer_95% CI` = paste0(`peer_Lower 95% CI`,"-", `peer_Upper 95% CI`),
                    `prov_95% CI` = paste0(`prov_Lower 95% CI`,"-", `prov_Upper 95% CI`)
                    ) %>%
                select(-contains("Lower"), -contains("Upper")) %>%
                select(`Estimate Type`="est_type", Indicator="indicator", `Indicator Level`="ind_level",
                       phu_Estimate, `phu_95% CI`, `phu_Quality Indicator`,
                       `Significance Compared to Ontario`, `Significance Compared to Peer`,
                       peer_Estimate, `peer_95% CI`, `peer_Quality Indicator`, 
                       prov_Estimate, `prov_95% CI`, `prov_Quality Indicator`) %>%
                rename_all(~stringr::str_replace(., "phu_", paste0(input$phu_label," "))) %>%
                rename_all(~stringr::str_replace(., "peer_", paste0("Peer Group ", stringr::str_sub(input$peer, -1, -1)," "))) %>%
                rename_all(~stringr::str_replace(., "prov_", "ON "))
                
            
            output$results_table <- renderDataTable({
                clean_wide
            })
        }, priority = -10)
        
        observe({
        tablepath <- parseSavePath(volumes, input$save_table)
        print(tablepath)
        
        if(length(tablepath$datapath)>0) {
            
            if(stringr::str_detect(tablepath$datapath, "xlsx")){
                writexl::write_xlsx(clean_wide, tablepath$datapath)
            }
            if(stringr::str_detect(tablepath$datapath, "csv")){
                write.csv(clean_wide, tablepath$datapath)
            }
            if(stringr::str_detect(tablepath$datapath, "rds")){
                saveRDS(write.csv(clean_wide, tablepath$datapath))
            }
        }
        })
        
          
        
    })

# Run the application 
shinyApp(ui = ui, server = server)