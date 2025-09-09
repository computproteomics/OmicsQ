library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(DT)
library(data.table)
library(readxl)
library(openxlsx)
library(stringdist)
library(limma)
library(matrixStats)
library(MsCoreUtils)
library(jsonlite)
library(BEclear)
library(sva)
library(gridExtra)
library(ggplot2)
library(gplots) 
library(viridis)
library(UniProt.ws)
library(ggrepel)
library(ggiraph)



source("DataInput.R")
source("ExpDesign.R")
source("PreProcessing.R")
source("SendRetrieve.R")

options(shiny.maxRequestSize = 200 * 1024^2, shiny.fullstacktrace=TRUE)
enableBookmarking(store = "server")

###### Start UI
ui <- function(req) {
    navbarPage(
        id="mainpage",
        windowTitle = "OmicsQ",
        title = tags$span(
            tags$img(src = "Logo_OmicsQ.svg", height = "20px", style = "vertical-align:middle;"),
            ": a toolkit for quantitative Omics analysis"), 
        header = list(wellPanel(
            style= "border-color: #333333; border: black solid 1px; padding: 1px;display: inline-block;float:right",
            actionBttn(
                inputId = "custom_bookmark",
                label = "Temporarily save settings and data",
                icon = icon("bookmark"),
                style = "pill",
                color = "default",
                size = "s"
            ),
            # bookmarkButton(
            #     label = "Temporarily save settings and data",
            #     title = "Bookmark this page to temporarily save your current settings and data.",
            #     icon = icon("bookmark"),
            #     style = "pill",
            #     color = "white",
            #     size = "s"),
            actionBttn("h_log",
                       label="Summary of operations",    
                       icon=icon("book"),
                       style="pill",
                       color = "default", size = "s"),
            actionBttn("h_about",
                       label="About",
                       icon=icon("info"),
                       style="pill", 
                       color = "default", size = "s"),
            actionBttn("h_tutorial",
                       label="Documentation",
                       icon=icon("question"),
                       style="pill", 
                       color = "default", size = "s")
            )
        ),
        
        theme = shinythemes::shinytheme("spacelab"),
        useShinyjs(),
        
        extendShinyjs(script="CallShiny.js", functions=c("retrieve_results","send_message","run_button")),
        
        tags$head(tags$script(src="CallShiny.js"),
                  tags$style(HTML("
      body {
    background-color: #f3e5f566; /* Replace with your chosen color code */
      }
#background-container {
    background-image: url('Background.svg');  /* Place in www/ folder */
 background-size: 60%;
  background-position: center;
  background-attachment: fixed;
  background-repeat: no-repeat;
  min-height: 100vh;       /* Ensures the container is at least full viewport height */
  width: 100%;        }
    .navbar-nav > li > a:hover {
    font-weight: bold;
    background-color: #f5f5f5;
  }
  .no-background {
    background-image: none !important;
  }   
    "))),
        
        # Define tabs
        tabPanel("Step I: Data Input", value = "read", # reading file and experimental design (add/delete replicates), 
                 useSweetAlert(),
                 div(id = "background-container",  # wrap with this div
                     fluidPage(
                         dataInputUI("dataInput")
                     )
                 )
        ),
        tabPanel("Step II: Experimental Design", value = "exp_design", 
                 fluidPage(
                     expDesignUI("expDesign")
                 )
        ),
        tabPanel("Step III: Pre-processing", value = "process",  
                 fluidPage(
                     h3("Data treatment pre-submission"),
                     preProcessingUI("preProcessing")
                 )
        ), 
        tabPanel("Step IV: Analysis via External Apps", value = "apps",
                 fluidPage(
                     sendRetrieveUI("sendRetrieve")
                 )
                 
        )
    )
}


