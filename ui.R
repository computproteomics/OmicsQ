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

###### Start UI
ui <- navbarPage(
  id="mainpage",
  title = "OmicsQ: a toolkit for quantitative proteomics",
  theme = shinythemes::shinytheme("spacelab"),
  tags$head(tags$script(src="CallShiny.js")),
  useShinyjs(),  # Include shinyjs
  extendShinyjs(script="CallShiny.js", functions=c("send_message")),
  # primary_theme_color = "#69DDFF", 
  # secondary_theme_color = "#DBBADD",  
  # Place side-nav in the beginning of the UI
  # material_side_nav(
  #   fixed = FALSE,
  #   tags$h3("Side-Nav Content")
  # ),
  # Define tabs
  tabPanel("Reading data", value = "read", # reading file and experimental design (add/delete replicates), 
           useSweetAlert(),
           #gene names, protein names, PTMs, fasta? Check for inconsistent columns 
           # and offer to remove artifacts, check for identical values and variance=0
           fluidPage(
             fluidRow(
               column(3,
                      h3("File input"), 
                      fluidRow(column(6,fileInput("pfile", label = "Data table")),
                               column(6,actionBttn("h_pfile",
                                                   icon=icon("info-circle"),
                                                   style="pill", 
                                                   color = "royal", size = "xs"))
                      )),
               hidden(column(3,id="in_c1",
                             h4("File specific settings"),
                             fluidRow(column(10,h5("(modify if necessary)")),
                                      column(2, actionBttn("h_csv_input",
                                                           icon=icon("info-circle"),
                                                           style="pill", 
                                                           color = "royal", size = "xs")
                                      )), 
                             uiOutput("file_options"), 
                             style = 'border-left: 1px solid'
               )),
               hidden(column(3,id="in_c2",
                             h4("Select and adjust"), 
                             fluidRow(column(10,pickerInput("sel_col", "Manipulate which columns?", choices=NULL,  multiple=T, 
                                                            options = list(
                                                              `live-search` = TRUE,
                                                              `actions-box` = TRUE))),
                                      column(2, actionBttn("h_sel_col",
                                                           icon=icon("info-circle"),
                                                           style="pill", 
                                                           color = "royal", size = "xs")
                                      )),                                       
                             hr(),
                             fluidRow(column(10,p("Select id and data columns:"),
                             actionButton("sel_id_col", 
                                          label=HTML("Select ID column")),
                             actionButton("sel_quant_cols", 
                                          label=HTML("Select quantitative columns")),
                             ),
                             column(2, actionBttn("h_sel_id_col",
                                                  icon=icon("info-circle"),
                                                  style="pill", 
                                                  color = "royal", size = "xs")
                             )),                             
                             hr(),
                             fluidRow(column(10,p("Simple manipulations and corrections:"),
                             actionButton("remove_zeroes", label="Make zeroes to missing values"),
                             actionButton("remove_char", label="Make non-numeric entries missing values"),
                             ),column(2, actionBttn("h_remove_zeroes",
                                                    icon=icon("info-circle"),
                                                    style="pill", 
                                                    color = "royal", size = "xs")
                             )),
                             style = 'border-left: 1px solid'
               )),
               hidden(column(3,id="in_c3",
                             h4("Proceed to experimental design"),
                             fluidRow(column(10,textOutput("txt_proceed_expdesign"),
                             ),column(2, actionBttn("h_proceed_expdesign",
                                                    icon=icon("info-circle"),
                                                    style="pill", 
                                                    color = "royal", size = "xs")
                             )),                                             
                             disabled(actionButton("proceed_to_expdesign", "Proceed")),
                             style = 'border-left: 1px solid'
               ))
             ),
             hr(),
             fluidRow(
               DTOutput('ptable')
             )
           )
  ),
  tabPanel("Experimental design", value = "exp_design", 
           fluidPage(
             fluidRow(
               column(width=6,
                      h3("Automatic selection of experimental groups"),
                      p("Change accordingly. You  can edit the experimental design below. Replicates with equal 
               number and of the same sample type will be summarized."),
                      hidden(
                        sliderInput("dist_thresh", "Threshold to distinguish groups", min=0, max=1, value=0)
                      ),  
                      hidden(
                        selectInput("dist_type", "Which string distance type?", 
                                    choices = c("Optimal string alignment"="osa",
                                                "Levenshtein"="lv",
                                                "Damerau-Levenshtein"="dl",
                                                "Hamming"="hamming",
                                                "Longest common substring"="lcs",
                                                "q-gram"="qgram",
                                                "cosine"="cosine",
                                                "Jaccard"="jaccard",
                                                "Jaro-Winkler"="jw",
                                                "soundex"="soundex"), selected="")
                      )
               ),
               hidden(column(5,id="ed_c3",
                             h4("Proceed to data pre-processing"),
                             p("This will summarize replicates with the number (and the same condition)"),
                             actionButton("proceed_to_process", "Proceed"),
                             style = 'border-left: 1px solid')
               )
             ),
             fluidRow(
               DTOutput('etable')
             )
           )
           
           # try to retrieve automatically experimental design 
           #(there are functions for that), allow assigning them, add 
           # emtpy columns
  ),
  tabPanel("Pre-processing", value = "process",  
           fluidPage(
             h3("Prepare the data for submission to the different apps"),
             fluidRow(
               hidden(column(width=4, id="pr_c1",
                             p("The different experimental conditions (sample types) need to be at least nearly balanced. This means 
                        that the number of replicates per sample should be the same for each of them. You can balance the data
                        by adding empty columns and/or removing excess replicates. Replicates with the same number need to be summarized."),
                             pickerInput("remove_reps", "Pick the samples you want to remove", choices = NULL, multiple=T,
                                         options = list(
                                           `live-search` = TRUE,
                                           `actions-box` = TRUE)),
                             p("Beware that the following option should only applied when few replicates are missing:"),
                             switchInput("add_na_columns", "Add empty columns for full balance", value=FALSE),
                             p(textOutput("res_num_reps"), style="text-color:#AA2222")
               )),
               hidden(column(width=3, id="pr_c2",
                             p("Here you can filter rows with too many missing values, select the normalization method, 
                               and summarize via sum or using the VIQoR"),
                             checkboxInput("logtrafo", "Is the data already log-transformed?", value=F),
                             numericInput("max_na", label="Maximum number of missing values per feature", 
                                          min=0, max=0, step=1, value=100),
                             selectInput("normalization", label="Normalization method",
                                         choices=c(None="none", Median="median", "Mean"="mean","Cyclic LOESS (LIMMA)"="cyclicloess",
                                                   selected="median")),
                             selectInput("summarize", label="Summarize to id features", 
                                         choices=c(None="none","By sum"="colSums",
                                                   "By mean"="colMeans", "By median"="colMedians", 
                                                   "Robust median (medpolish)"="medianPolish"
                                                   #"Robust summary (rlm)"="robustSummary" Does not work with missing values
                                         )),
                             style = 'border-left: 1px solid' 
               )              
               ),
               hidden(column(4,id="pr_c3",
                             h4("Summary:"),
                             htmlOutput("ptable_summary"),
                             h4("Proceed to interaction with apps"),
                             textOutput("txt_proceed_apps"),
                             disabled(actionButton("proceed_to_apps", "Proceed")),
                             style = 'border-left: 1px solid'    
               )           
               ),
               hidden(fluidRow(id="pr_plots",
                               column(5,
                                      plotOutput("pca")),
                               column(5,
                                      plotOutput("corrplot")
                               )))
             )
             # simple processing, PCA, missing values
             # Show PCA and retrieve some simple statistics like correlation between replicates, missing values, total numbers, homogeneity, 
             # double entries, high variance levels in PTMs 
             # Create list of suggested operations on the data: 1) log trafo, 2) remove some features due to missingness, 3) remove redundant rows 4) filter PTMs
             # 5) normalize?
           ), 
           tabPanel("Send and retrieve", value = "apps",
                    fluidPage(
                      h3("Analyze the table with the different apps"),
                      fluidRow(
                        hidden(column(width=4, id="app_c1",
                                      h4("Statistical testing"),
                                      actionButton("send_polystest", "Send to PolySTest"),
                                      textOutput("connection_polystest"),
                                      textInput("url_polystest",label="URL",value="http://computproteomics.bmb.sdu.dk:443/app_direct/PolySTest/"),
                                      hidden(actionButton("retrieve_polystest", "Retrieve results from PolySTest"))
                        )),
                        hidden(column(width=4, id="app_c2",
                                      h4("Clustering"),
                                      actionButton("send_vsclust", "Send to VSClust"),
                                      span(textOutput("connection_vsclust"), style="color:#33DD33;"),
                                      textInput("url_vsclust",label="URL",value="http://computproteomics.bmb.sdu.dk:443/app_direct/VSClust/"),
                                      hidden(actionButton("retrieve_vsclust", "Retrieve results from VSClust")),
                                      style = 'border-left: 1px solid'    
                        )
                        ),
                        hidden(column(width=4, id="app_c3",
                                      h4("Investigate protein complex behavior"),
                                      actionButton("send_polystest", "Send to ComplexBrowser"),
                                      textOutput("connection_complexbrowser"),
                                      textInput("url_complexbrowser",label="URL",value="http://computproteomics.bmb.sdu.dk:443/app_direct/ComplexBrowser/"),
                                      hidden(actionButton("retrieve_complexbrowser", "Retrieve results from ComplexBrowser")),
                                      style = 'border-left: 1px solid'    
                        ))
                      )
                    ),
                    hidden(textInput("app_log", "app_log", value=NULL))
           )
           # send to other apps, button of "active" and then allow retrieving results?
  )
) 
