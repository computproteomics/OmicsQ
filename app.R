library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(shinythemes)
library(shinyjs)
library(DT)
library(data.table)
library(readxl)
library(stringdist)

## useful functions and javascript code
# distances between column names
expd_dist <- function(cnames, ...) {
  out_m <- matrix(1, ncol=length(cnames), nrow=length(cnames), 
                  dimnames=list(x=cnames, y=cnames))
  for (i in cnames) {
    out_m[i,] <- 1-stringdist(i, cnames, ...)
  }
  1-out_m
}



###### Start UI
ui <- navbarPage(
  id="mainpage",
  title = "OmicsQ: a toolkit for quantitative proteomics",
  theme = shinythemes::shinytheme("spacelab"),
  useShinyjs(),  # Include shinyjs
  # primary_theme_color = "#69DDFF", 
  # secondary_theme_color = "#DBBADD",  
  # Place side-nav in the beginning of the UI
  # material_side_nav(
  #   fixed = FALSE,
  #   tags$h3("Side-Nav Content")
  # ),
  # Define tabs
  tabPanel("Reading data", value = "read", # reading file and experimental design (add/delete replicates), 
           #gene names, protein names, PTMs, fasta? Check for inconsistent columns 
           # and offer to remove artifacts, check for identical values and variance=0
           fluidPage(
             fluidRow(
               column(3,
                      h4("File input"),
                      fileInput("pfile", label = "Data table")
               ),
               hidden(column(3,id="in_c1",
                             h4("File specific settings (modify if necessary"), 
                             uiOutput("file_options")
               )),
               hidden(column(3,id="in_c2",
                             h4("Select and adjust"), 
                             pickerInput("sel_col", "Manipulate which columns?", choices=NULL,  multiple=T, 
                                         options = list(
                                           `live-search` = TRUE,
                                           `actions-box` = TRUE)),
                             hr(),
                             p("Select id and data columns:"),
                             actionButton("sel_id_col", 
                                          label=HTML("Select one column with main features<br/>(e.g. gene ids, prootein ids, peptide sequences)")),
                             actionButton("sel_quant_cols", 
                                          label=HTML("Select columns quantitative feature values<br/>(e.g. protein abundances or gene expressions)")),
                             hr(),
                             p("Simple manipulations and corrections:"),
                             actionButton("remove_zeroes", label="Make zeroes to missing values"),
                             actionButton("remove_char", label="Make non-numeric entries missing values")
               )),
               hidden(column(3,id="in_c3",
                             h4("Proceed to experimental design"),
                             textOutput("txt_proceed_expdesign"),
                             disabled(actionButton("proceed_to_expdesign", "Proceed"))
               )),
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
                      h4("Automatic selection of experimental groups"),
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
                             textOutput("txt_proceed_preprocess"),
                             actionButton("proceed_to_process", "Proceed")
               ))),
             fluidRow(
               DTOutput('etable')
             ),
             
             # try to retrieve automatically experimental design 
             #(there are functions for that), allow assigning them, add 
             # emtpy columns
           ),
  ),
  tabPanel("Pre-processing", value = "process",  
           fluidPage(
             h4("Prepare the data for submission to the different apps"),
             fluidRow(
               hidden(column(width=6, id="pr_c1",
                             p("The different experimental conditions (sample types) need to be at least nearly balanced. This means 
                        that the number of replicates per sample should be the same for each of them. You can balance the data
                        by adding empty columns and/or removing excess replicates. Replicates with the same number need to be summarized."),
                             hidden(
                               uiOutput("todo")
                               
                             ),  
               ),
               hidden(column(5,id="ed_c3",
                             h4("Proceed to interaction with apps"),
                             textOutput("txt_proceed_apps"),
                             actionButton("proceed_to_apps", "Proceed")
               )))
             ),
             fluidRow(
               DTOutput('qtable')
             ),
           )
           # simple processing, PCA, missing values
           # Show PCA and retrieve some simple statistics like correlation between replicates, missing values, total numbers, homogeneity, 
           # double entries, high variance levels in PTMs 
           # Create list of suggested operations on the data: 1) log trafo, 2) remove some features due to missingness, 3) remove redundant rows 4) filter PTMs
           # 5) normalize?
           
  ), 
  tabPanel("Send and retrieve", value = "apps"  # send to other apps, button of "active" and then allow retrieving results?
           
  ),
)

server <- function(input, output, session) {
  ## main data sets
  indata <- reactiveVal(NULL)
  exp_design <- reactiveVal(NULL)
  process_table <- reactiveVal(NULL)
  fasta <- protdata <- pepdata <- statdata <- NULL
  
  # ###### SET FOR TESTING 
  # 
  # tdata <- as.data.frame(fread("Myo.csv"))
  # for (col in grep("C[0-9]_", colnames(tdata), value=T))
  #   class(tdata[,col]) <- "quant"
  # indata(tdata)
  # show(id="in_c3")
  # enable(id="in_c3")
  
  ##########################
  
  ##### READING DATA
  ## reading file
  observe({
    tdata <- NULL
    # Read file
    in_file <- input$pfile
    if (is.null(in_file))
      return(NULL)
    if (tools::file_ext(in_file$datapath) %in% c("xls", "xlsx", "XLS", "XLSX")) {
      # Set Options for file input
      currsheet <- ifelse(is.null(input$in_sheet), 1, input$in_sheet)
      
      tdata <- try({
        sheets <- excel_sheets(in_file$datapath)
        read_excel(in_file$datapath, sheet = currsheet)
      })
      
      # file options
      show(id="in_c1")
      output$file_options <- renderUI({
        input$in_sheet
        selectInput("in_sheet", "Which table sheet?", choices = sheets)
      })
    } else {
      # file options
      currdel <- ifelse(is.null(input$in_delimiter), "auto", input$in_delimiter)
      currdec <- ifelse(is.null(input$in_dec), ".", input$in_dec)
      currskip <- ifelse(is.null(input$in_skip), 0, input$in_skip)
      currheader <- ifelse(is.null(input$in_header), T, input$in_header)
      
      tdata <- (try(fread(in_file$datapath, sep=currdel, skip=currskip, header=currheader, dec=currdec)))
      show(id="in_c1")
      output$file_options <- renderUI({
        tagList(
          selectInput("in_delimiter", label="delimiter", choices =
                        c(auto="auto",comma=",",semicolon=";",tabulator="\t", colon=":",
                          bar="|", space=" "), selected = currdel , multiple = FALSE,
          ),
          selectInput("in_dec", label="decimal separator", choices = c(comma=",",point="."),
                      selected = currdec),
          numericInput("in_skip", label="remove lines at beginning?", min = 0,
                       max = 100, step = 1, value = currskip),
          checkboxInput("in_header", label="Does file have a header?", value = currheader)
        )
      })
    }
    
    show(id="in_c2")
    show(id="in_c3")
    updatePickerInput(session,"sel_col", choices=names(tdata))
    
    
    
    
    ## feedbeck via moda
    log_upload <- as.character(geterrmessage())
    if (inherits(tdata, "try-error")) {
      print("error reading file")      
      showModal(modalDialog(title="file upload", log_upload, size="s", easyClose = T))
    } else {
      # set id column
      tdata <- data.frame(tdata)
      class(tdata[,1]) <- "id"
      indata(tdata)
    }
    
  })
  
  #### Input data data table
  output$ptable <- DT::renderDT({
    print("dttable")
    show_table <- indata()
    
    ## create header and footer of table
    
    header.style <- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #008080;}"
    #pull header names from the table
    header.names <- c("Columns", colnames(show_table))
    header.classes <- c("Type", sapply(show_table,class))
    header.counts <- c("Duplicated values", sapply(show_table, function(x) sum(duplicated(x, incomparables = NA))))
    # The container parameter allows us to design the header of the table using CSS
    column_cols <- c(numeric="#808000",character="#008080",id="#800080", quant="#33AA33", factor="#008080", logical="#008080")
    datfile_container <- withTags(table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names, th, style = "text-align: center; border-right-width: 1px; 
                 border-right-style: solid; border-right-color: white; border-bottom-width: 1px; 
                 border-bottom-style: solid; border-bottom-color: white")
        ),
        tr(
          lapply(header.classes, function(x) 
            th(x, style = paste0("text-align: center; border-right-width: 1px; 
                                 border-right-style: solid; background-color: ",
                                 column_cols[x], 
                                 "; border-right-color: white; border-bottom-width: 1px; 
                                border-bottom-style: solid; border-bottom-color: white")))
        ),
        tr(
          lapply(header.counts, function(x) 
            th(x, style = paste0("text-align: center; border-right-width: 1px; 
                                 border-right-style: solid; background-color: ",
                                 ifelse(x>0, "#800000", "#008000"), 
                                 "; border-right-color: white; border-bottom-width: 1px; 
                                border-bottom-style: solid; border-bottom-color: white")))
        )
        
      )
    ))    
    # sketch <- tags$table(
    #   class = "row-border stripe hover compact",
    #   tableHeader(c("row names",  names(show_table))
    #   ),
    #   tableHeader(c("row names", lapply(show_table,class)
    #   ))
    #   # tableFooter(c("", buttons))
    # )
    datatable(show_table, container=datfile_container
              # ,
              #           callback=JS(js_change_columnname)
    )
    
    
  }
  )
  
  ## select column with ids
  observeEvent(input$sel_id_col, {
    isolate({
      get_cols <- make.names(input$sel_col)[1]
      if (!is.null(get_cols)) {
        print("select id columns")
        tdata <- data.frame(indata())
        # remove id class from all
        for (col in colnames(tdata)) {
          if (class(tdata[,col]) == "id")
            tdata[,col] <- unclass(tdata[,col])
        }
        class(tdata[,get_cols]) <- "id"
        indata(tdata)
      }
    })
    
  })
  
  ## select columns with quant
  observeEvent(input$sel_quant_cols, {
    isolate({
      get_cols <- make.names(input$sel_col)
      if (!is.null(get_cols)) {
        print("select quant columns")
        tdata <- data.frame(indata())
        # remove id class from all
        for (col in colnames(tdata)) {
          if (class(tdata[,col]) == "quant")
            tdata[,col] <- unclass(tdata[,col])
        }
        for (col in get_cols) {
          if (class(tdata[,col]) == "numeric")
            class(tdata[,col]) <- "quant"
        }
        
        # Control button
        if (sum(sapply(tdata, class) == "quant") > 0) {
          enable("proceed_to_expdesign")
          output$proceed_to_expdesign <- renderText("Ready to go")
        } else {
          disable("proceed_to_expdesign")
          output$proceed_to_expdesign <- renderText("You need to select at least one column with quantified features. 
                                                    This column needs to the \"numeric\".")
        }
        
        indata(tdata)
      }
    })
    
  })
  
  ## Manipulate input table
  observeEvent(input$remove_zeroes, {
    print(input$remove_zeroes)
    isolate({
      get_cols <- make.names(input$sel_col)
      if (!is.null(get_cols)) {
        print("removing zeroes")
        tdata <- data.frame(indata())
        for (col in get_cols)
          tdata[, col] <- replace(tdata[,col], tdata[,col] == 0, NA)
        indata(tdata)
      }
    })
    
  })
  
  ## Make characters NA
  observeEvent(input$remove_char, {
    isolate({
      get_cols <- make.names(input$sel_col)
      if (!is.null(get_cols)) {
        print("removing chars")
        tdata <- data.frame(indata())
        for (col in get_cols)
          tdata[, col] <- as.numeric(tdata[,col])
        indata(tdata)
      }
    })
    
  })
  
  ## Send further to next tab
  observeEvent(input$proceed_to_expdesign, {
    updateTabsetPanel(session, "mainpage",
                      selected = "exp_design")
    cnames <- colnames(indata())[sapply(indata(), class) == "quant"]
    ted <- rbind(rep(NA,length(cnames)), NA)
    colnames(ted) <- cnames
    rownames(ted) <- c("Group","Replicate")
    print(ted)    
    exp_design(ted)
    updateSelectInput(session, "dist_type", selected="jw")
    show("dist_thresh")
    show("dist_type")
    show("ed_c3")
  })
  
  ##### EXPERIMENTAL DESIGN
  
  # update threshold
  observe({
    input$dist_type
    isolate({
      if (!is.null(exp_design())) {
        print("dist_type")
        expd_d <- expd_dist(colnames(exp_design()), method=input$dist_type, p=0.1) # p=0.1 prioritizes the start of the strings
        median_dist <- median(expd_d[expd_d != 0], na.rm=T)
        print(median_dist)
        updateSliderInput(session, "dist_thresh", value=median_dist, min=min(expd_d, na.rm=T), max=max(expd_d, na.rm = T))
      }
    })
  })
  
  # update exp. design
  observe({
    input$dist_thresh
    isolate({
      tdesign <- exp_design()
      if (!is.null(tdesign)) {
        print("dist_thres")
        expd_d <- expd_dist(colnames(tdesign), method=input$dist_type, p=0.1) # p=0.1 prioritizes the start of the strings
        median_dist <- input$dist_thresh
        groups <- cutree(hclust(as.dist(expd_d)), h=median_dist)
        tdesign[1,] <- groups
        for (j in unique(groups)) {
          tdesign[2, groups == j] <- 1:sum(groups==j)
        }
        
        exp_design(tdesign)
      }
    })
  })
  
  # Table for editing design
  output$etable <- DT::renderDT({
    if (!is.null(exp_design())) {
      print("edtable")
      show_table <- exp_design()
      print("done")
      datatable(show_table, editable=T)
    }
  })
  
  observeEvent(input$etable_cell_edit, {
    tdata <- exp_design()
    tdata[input$etable_cell_edit$row,input$etable_cell_edit$col] <- input$etable_cell_edit$value
    exp_design(tdata)
  })
  
  
  ## Send further to next tab
  observeEvent(input$proceed_to_process, {
    updateTabsetPanel(session, "mainpage",
                      selected = "process")
    
    final_exp_design <- exp_design()
    exp_design(final_exp_design[,order(final_exp_design[1,], final_exp_design[2,])])
    
    tdata <- indata()
    icol <- colnames(tdata)[grep("id",sapply(tdata, class))]
    ccols <- colnames(tdata)[grep("quant",sapply(tdata, class))]
    ocols <- colnames(tdata)[which(!(colnames(tdata) %in% c(icol, ccols)))]
    process_table(tdata[,c(icol, colnames(final_exp_design), ocols)])
    # updateSelectInput(session, "dist_type", selected="jw")
    # show("dist_thresh")
    # show("dist_type")
  })
  
  
}

shinyApp(ui = ui, server = server)

