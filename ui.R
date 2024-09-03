library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(DT)
library(data.table)
library(readxl)
library(stringdist)
library(limma)
library(matrixStats)
library(MsCoreUtils)
library(jsonlite)
library(BEclear)
source("DataInput.R")
source("ExpDesign.R")
source("PreProcessing.R")
source("SendRetrieve.R")

options(shiny.maxRequestSize = 200 * 1024^2, shiny.fullstacktrace=TRUE)

###### Start UI
ui <- navbarPage(
  id="mainpage",
  title = "OmicsQ: a toolkit for quantitative proteomics",
  header = list(div(
    actionBttn("h_log",
               label="Summary of operations",    
               icon=icon("book"),
               style="pill",
               color = "default", size = "s"),
    actionBttn("h_about",
               label="About",
               icon=icon("question"),
               style="pill", 
               color = "default", size = "s"),
    style="float:right;")),
  theme = shinythemes::shinytheme("spacelab"),
  tags$head(tags$script(src="CallShiny.js")),
  useShinyjs(),
  extendShinyjs(script="CallShiny.js", functions=c("retrieve_results","send_message","run_button")),
  # Define tabs
  tabPanel("Reading data", value = "read", # reading file and experimental design (add/delete replicates), 
           useSweetAlert(),
           fluidPage(
             dataInputUI("dataInput")
           )
  ),
  tabPanel("Experimental design", value = "exp_design", 
           fluidPage(
             expDesignUI("expDesign")
           )
  ),
  tabPanel("Pre-processing", value = "process",  
           fluidPage(
             h3("Data treatment pre-submission"),
             preProcessingUI("preProcessing")
           )
  ), 
  tabPanel("Send and retrieve", value = "apps",
           fluidPage(
             sendRetrieveUI("sendRetrieve")
           )
           
  )
)

