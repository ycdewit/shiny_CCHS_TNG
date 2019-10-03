package_list <- c("shiny", "shinyFiles", "shinyjs", "readxl", "foreign", "dplyr", "tibble", "stringr", "DT", "forcats")

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
                checkboxInput("load_prev", label = "Load my data from previous session", value = TRUE),
                h4("Please select data files"),
                "Current format accepted include SPSS, STATA, R, Excel, and Comma-Separated data files (i.e. with a .dta, .sav, .rds, .xlsx, or .csv file extension). You may add new variables to the data file(s) but ", 
                strong("do not modify any of the original variables"), 
                " or this tool may not work.",
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
                selectInput("select_phu", label="Please select PHU", choices = 
                            c("The District of Algoma HU", 
                            "Brant County HU",                                
                            "Durham Regional HU", "Elgin-St Thomas HU", "Grey Bruce HU", "Haldimand-Norfolk HU",                           
                            "Haliburton, Kawartha, Pine Ridge District HU",   
                            "Halton Regional HU",                             
                            "City of Hamilton HU",                           
                            "Hastings and Prince Edward Counties HU",         
                            "Huron County HU",                                
                            "Chatham-Kent HU",                                
                            "Kingston, Frontenac and Lennox and Addington HU",
                            "Lambton HU",                                     
                            "Leeds, Grenville and Lanark District HU",        
                            "Middlesex-London HU",                            
                            "Niagara Regional Area HU",                     
                            "North Bay Parry Sound District HU",              
                            "Northwestern HU",                                
                            "City of Ottawa HU",                              
                            "Oxford County HU",                              
                            "Peel Regional HU",                              
                            "Perth District HU",                              
                            "Peterborough County-City HU",                    
                            "Porcupine HU",                                   
                            "Renfrew County and District HU",                 
                            "Eastern Ontario HU",                             
                            "Simcoe Muskoka District HU",                     
                            "Sudbury and District HU",                        
                            "Thunder Bay District HU",                        
                            "Timiskaming HU",                                 
                            "Waterloo HU",                                    
                            "Wellington-Dufferin-Guelph HU",                  
                            "Windsor-Essex County HU",                        
                            "York Regional HU",                               
                            "City of Toronto HU")),
                selectInput("select_peer", label="Please select Peer Group", choices = 
                                c("Health Region Peer Group A", "Health Region Peer Group B",
                                  "Health Region Peer Group C", "Health Region Peer Group D",
                                  "Health Region Peer Group E", "Health Region Peer Group G",
                                  "Health Region Peer Group H")),
                "Not sure what Statistics Canada Peer Group your public health unit is in?", 
                a("Click here", href="https://www150.statcan.gc.ca/n1/pub/82-402-x/2015001/regions/tbl/tbl8-eng.htm"), 
                "to check.",
                br(),
                br(),
                checkboxGroupInput("clean_options", label = "Options for data processing", 
                                   choices = list("Fix errata" = 1, "Include addition variable creation" = 2),
                                   selected = c(1,2)),
                a("Click here",target="_blank",href="CCHS_Errata 2015-2017_Feb 2019.pdf"), "to view the version of the CCHS 2015-2016 errata used in this tool.",
                a("Click here",target="_blank",href="additional_vars.xlsx"), "to view data dictionary for additional computed variables.",
                br(),
                br(),
                actionButton("submit", "Prepare CCHS Data", width = "100%")
                ),
            tabPanel(
                "Setup Analysis",
                strong("Please select the question(s) to analyze."),
                em("start typing in the box below search by either by description or by the name used in the CCHS data (including custom variables you included in your imported data)."),
                selectizeInput("questions", NULL, NULL, multiple = TRUE, options=list(placeholder = 'No data detected. Please use sidebar to prepare data')),
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
                sliderInput("age_range", label = "Age Range", min = 10, 
                max = 105, value = c(12, 105), step = 10)
                )
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Explore Data",
                         DT::dataTableOutput("cchs_df")),
                tabPanel("Table"),
                tabPanel("Graph") 
            )
        )
    )
)



# Define server logic 
server <- 
    
    shinyServer(function(input, output, session) {
        
        cchs_var_desc <- read_xlsx("data/CCHS_var_desc.xlsx")%>%
            select(variable_name, variable_desc)
        
        if (file.exists("cchs_data.rds")){
            show("load_prev")
            cchs_df <- readRDS("cchs_data.rds")
        }
        else updateCheckboxInput(session, "load_prev", value=FALSE)
    
        observe({
            if(input$load_prev==TRUE){
                prev_phu <- levels(droplevels(cchs_df[cchs_df$phu=="Yes",]$GEODVHR4))
                updateSelectInput(session, "select_phu", selected = prev_phu)
                prev_peer <- levels(droplevels(cchs_df[cchs_df$peer=="Yes",]$GEODVPG))
                updateSelectInput(session, "select_peer", selected = prev_peer)
            }
        })
        
        volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
        shinyFileChoose(input, "hs_file", roots = c(volumes, "wd"="."), defaultRoot = "wd", session = session)
        shinyFileChoose(input, "bs_file", roots = volumes, session = session)
        shinyFileSave(input, "save", roots = volumes, session = session, restrictions = system.file(package = "base"))
        
        observe({
            output$txt_hs <- renderText(paste0("Selected file: ",parseFilePaths(volumes, input$hs_file)$datapath))
        })
        
        observe({
            output$txt_bs <- renderText(paste0("Selected file: ",parseFilePaths(volumes, input$bs_file)$datapath))
        })
        
        observe({
            if(1 %in% input$clean_options){
                errata <<- TRUE
            }
            
            if (2 %in% input$clean_options) {
                include_recode <<- TRUE
            }
        })
        
        observeEvent(input$boot_yn, {
            if(input$boot_yn==TRUE){
                hide("bs_file")  # hide is a shinyjs function
                hide("txt_bs")
                bs_filepath <- NULL
            }
            else{
                show("bs_file")  # hide is a shinyjs function
                show("txt_bs")
                bs_filepath <- parseFilePaths(volumes, input$bs_file)$datapath
            }
            
        source("cchs_cleanup.R")
        source("cchs_errata.R")
        source("cchs_prep.R")
            
        observeEvent(input$submit, {
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Preparing data", detail= "This may take awhile, go stretch your legs or something.", value = 0.1)
            # Close the progress when this reactive exits (even if there's an error)
            on.exit(progress$close())
            
            update_progress <- function(value = NULL, detail = NULL) {
                if (is.null(value)) {
                    value <- progress$getValue()
                    value <- value + (progress$getMax() - value) / 3
                }
                progress$set(value = value, detail = detail)
            }
            
            cchs_newdf <<-    
                cchs_prep(
                    cchsfile = parseFilePaths(volumes, input$hs_file)$datapath,
                    includes_bootwts = input$boot_yn,
                    bootwts = bs_filepath,
                    phu_name = input$select_phu,
                    peer_group = input$select_peer,
                    run_errata = errata,
                    prov_name = "Ontario",
                    update_progress = update_progress)
        
            if (include_recode==TRUE){
                update_progress(detail = "Calculating additional variables")
                source("cchs_recode.R")
                cchs_newdf <- cchs_recode(cchs_newdf)
                update_progress(detail = "Finishing up")
                }
        
            saveRDS(cchs_newdf, "cchs_data.rds")
        })
        
        observe ({
            if (include_recode==TRUE){
                cchs_var_desc <<- 
                    bind_rows(cchs_var_desc, read_xlsx("data/additional variables.xlsx")) %>%
                    select(variable_name, variable_desc)
            }
        })
        
        output$cchs_df <- DT::renderDataTable({
            input$submit
            input$load_prev
            if(input$load_prev==FALSE) {
                if(!exists("cchs_newdf")) {
                    cchs_newdf <- data.frame(v1="Please use sidebar to prepare data") %>% rename(`No data found`="v1")
                    }
                return(cchs_newdf)
            }
            if(!file.exists("cchs_data.rds")) {
                cchs_df <- data.frame(v1="Please use sidebar to prepare data") %>% rename(`No data found`="v1")
            }
            return(cchs_df)
        })
        
        observe({
            input$submit
            input$load_prev
            
            if(exists("cchs_newdf")) {
                ready_cchs <<- cchs_newdf
            }
            else if(exists("cchs_df")){
                ready_cchs <<- cchs_df
            }
            else ready_cchs <- NULL
        })
        
        source("utils.R")
        source("cchs_table.R")
        source("cchs_rr.R")
        
        age_rows <- 5
        
        observeEvent(input$more_ag, {
            age_rows <<- min(age_rows+1,10)
        })
        
        observeEvent(input$less_ag, {
            age_rows <<- max(age_rows-1,1)
        })
        
        observe({
            input$more_ag
            input$less_ag
            
            if(input$stand==TRUE){
                
                show("std_ag")
            
                for (i in 1:10) {
                    if(i<=age_rows){
                        show(eval(paste0("std_ag",i)))
                    }
                    else {
                        hide(eval(paste0("std_ag",i)))
                    }
                    
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
                minage <- min(agegrp_starts, na.rm = TRUE)
                maxage <- min(agegrp_starts, na.rm = TRUE)
                updateSliderInput(session, "age_range", value=c(minage, maxage))
                ready_cchs <<- make_std_agegrp(ready_cchs, agegrp_starts = agegrp_starts, agegrp_ends = agegrp_ends, agegrp_names = agegrp_names)
                std_pop <<- cchs_can2011(minage = minage, maxage = maxage, agegrp_starts = agegrp_starts, agegrp_ends = agegrp_ends, agegrp_names = agegrp_names) 
            }
            
        })
        function(dontrun){
        observe({
            input$stand
            input$submit
            if(!is.null(ready_cchs)){
            cchs_dd <- 
                names(ready_cchs) %>%
                left_join(cchs_var_desc)
            updateSelectizeInput(session, "questions", choices = cchs_dd$variable_name)
            }
        })
        observe({
            if(!is.null(input$quest_desc)){
                if(!is.null(input$quest_name)){
                    cchs_dd_filter <- 
                        filter(cchs_dd, variable_desc %in% c(input$quest_desc, input$quest_name))
                    updateSelectizeInput(session, "questions", selected = cchs_dd_filter$variable_name)}
                
                
                updateSelectInput(session, "quest_desc", choices = cchs_dd$variable_desc)}
            })
        }
    })
})


# Run the application 
shinyApp(ui = ui, server = server)
#runExample("10_download")
