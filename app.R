library(shiny)
library(shinyFiles)
library(shinyjs)
library(readxl)
library(mime)
library(foreign)
library(dplyr)
library(tibble)
library(stringr)
library(DT)


# Define UI 
ui <- fluidPage(
    useShinyjs(),
    titlePanel(title=div(img(src = "HPE_LogoH_COL.png", width = 220), "CCHS 2015-16: The Next GenRation")),
    
    sidebarLayout(
        sidebarPanel(
            tabsetPanel(
                tabPanel("Data",
                bookmarkButton(),
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
                            choices = list("Yes" = 1, "No" = 2), 
                            selected = 2),
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
                actionButton("updateButton", "Prepare CCHS Data", width = "100%")
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
                         DT::dataTableOutput("mytable")),
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
        
        observeEvent(input$boot_yn, {
            if(input$boot_yn==1){
                hide("bs_file")  # hide is a shinyjs function
                hide("txt_bs")
            }
            else{
                show("bs_file")  # hide is a shinyjs function
                show("txt_bs")
            }
            
        source("cchs_cleanup.R")
        source("cchs_errata.R")
        source("cchs_prep.R")
            
        observeEvent(input$updateButton, {
            
            output$mytable = DT::renderDataTable({
            cchs_prep(
                stata_cchsfile = parseFilePaths(volumes, input$hs_file)$datapath,
                stata_bootwts = parseFilePaths(volumes, input$bs_file)$datapath,
                phu_name = input$select_phu,
                peer_group = input$select_peer,
                prov_name = "Ontario")   
            })
        })    
        
        ## print to console to see how the value of the shinyFiles 
        ## button changes after clicking and selection
        
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
