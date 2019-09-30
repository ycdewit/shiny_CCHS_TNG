package_list <- c("shiny", "shinyFiles", "shinyjs", "readxl", "foreign", "dplyr", "tibble", "stringr", "DT")

get_packages <- function (packages) {
    for(package in packages) {
        if(! require(package, character.only = TRUE)){
            install.packages(package)
            require(package, character.only = TRUE)
        }
    }
}

get_packages(package_list)


cchs_df <- 
    if (exists("cchs_data.rds")) {
        readRDS("cchs_data.rds")
        }

# Define UI 
ui <- fluidPage(
    useShinyjs(),
    titlePanel(title=div(img(src = "HPE_LogoH_COL.png", width = 220), "CCHS 2015-16: The Next GenRation")),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel("Data",
                h4("Please select data files"),
                "Stata files are recommended but other formats will work. You may add new variables to the data file(s) but ", 
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
                img(src = "HPE_LogoH_COL.png", width = 220),
                checkboxGroupInput("phu", label="Select PHU"),
                sliderInput("slider1", label = h4("Age Range"), min = 12, 
                max = 100, value = c(12, 100))
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
                    value <- value + (progress$getMax() - value) / 5
                }
                progress$set(value = value, detail = detail)
            }
            
            cchs_df <<-    
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
                withProgress(message = "Recoding and creating new variables", value = 1/3, {
                source(cchs_recode)
                    incProgress(2/3)
                cchs_df <- cchs_recode(cchs_df) })
            }
        
            saveRDS(cchs_df,"cchs_data.rds")
        })    
        
        output$cchs_df <- DT::renderDataTable({
            input$submit
            return(cchs_df)
        })
        
        ## print to browser
        output$filepaths <- renderPrint({
            parseFilePaths(volumes, input$file)
        })
        
        output$savefile <- renderPrint({
            parseSavePath(volumes, input$save)
        })
    })
})


# Run the application 
shinyApp(ui = ui, server = server)
#runExample("10_download")
