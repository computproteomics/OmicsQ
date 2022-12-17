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

server <- function(input, output, session) {
  ## main data sets
  indata <- reactiveVal(NULL)
  exp_design <- reactiveVal(NULL)
  pexp_design <- reactiveVal(NULL)
  process_table <- reactiveVal(NULL)
  processed_table <- reactiveVal(NULL)
  log_vsclust <- reactiveVal(NULL)
  log_complexbrowser <- reactiveVal(NULL)
  log_polystest <- reactiveVal(NULL)
  result_table <- reactiveVal(NULL)
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
  
  ##### READING DATA #########################################################
  ## reading file
  observe({
    tdata <- NULL
    # Read file
    print("Reading file")
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
      shinyjs::show(id="in_c1")
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
      shinyjs::show(id="in_c1")
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
    
    shinyjs::show(id="in_c2")
    shinyjs::show(id="in_c3")
    updatePickerInput(session,"sel_icol", choices=names(tdata))
    updatePickerInput(session,"sel_qcols", choices=names(tdata))
    
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
  
  ### Read example file and push through
  observeEvent( input$run_example, ({
    tdata <- read.csv("Myo_Res.csv")
    class(tdata[,1]) <- "id"
    for (i in 2:19)
      class(tdata[,i]) <- "quant"
    indata(tdata)
    enable("proceed_to_expdesign")
    js$run_button(button="proceed_to_expdesign", number=1)
    tdata2 <- read.csv("Myo.csv", row.names=1)
    result_table(cbind(tdata,tdata))
  }))
  
  #### Input data data table
  output$ptable <- DT::renderDT({
    print("dttable")
    show_table <- indata()
    if (!is.null(show_table)) {
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
      print(head(show_table))
      datatable(show_table, container=datfile_container, options = list(scrollX = TRUE)
                # ,
                #           callback=JS(js_change_columnname)
      )
      
    } else {
      NULL
    }
  }
  )
  
  ## select column with ids
  observeEvent(input$sel_icol, {
    isolate({
      get_cols <- make.names(input$sel_icol)[1]
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
  observeEvent(input$sel_qcols, {
    isolate({
      get_cols <- make.names(input$sel_qcols)
      if (!is.null(get_cols)) {
        print("select quant columns")
        tdata <- data.frame(indata())
        # remove id class from all
        for (col in colnames(tdata)) {
          if (class(tdata[,col]) == "quant")
            tdata[,col] <- unclass(tdata[,col])
        }
        for (col in get_cols) {
          if (class(tdata[,col]) == "numeric" || class(tdata[,col]) == "integer")
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
        tclasses <- sapply(indata(), class)
        tdata <- data.frame(indata())
        for (col in get_cols) {
          tdata[, col] <- as.numeric(tdata[,col])
          class(tdata[, col]) <- tclasses[col]
        }
        indata(tdata)
      }
    })
    
  })
  
  ## Send further to next tab
  observeEvent(input$proceed_to_expdesign, isolate({
    print("proceed to expdesign")
    updateTabsetPanel(session, "mainpage",
                      selected = "exp_design")
    cnames <- colnames(indata())[sapply(indata(), class) == "quant"]
    ted <- rbind(rep(NA,length(cnames)), NA)
    colnames(ted) <- cnames
    rownames(ted) <- c("Group","Replicate")
    print(ted)    
    exp_design(ted)
    updateSelectInput(session, "dist_thresh", selected=NA)
    updateSelectInput(session, "dist_type", selected="jw")
    updatePickerInput(session, "ed_sel_samples", choices = cnames)
    updateSliderInput(session, "ed_number", max=length(cnames))
    shinyjs::show("dist_thresh")
    shinyjs::show("dist_type")
    shinyjs::show("ed_c3")
    shinyjs::show("ed_c2")
  }))
  
  ##### EXPERIMENTAL DESIGN #########################################################
  
  # update threshold
  observe({
    input$dist_type
    isolate({
      if (!is.null(exp_design())) {
        print("dist_type")
        expd_d <- expd_dist(colnames(exp_design()), method=input$dist_type, p=0.2) # p=0.1 prioritizes the start of the strings
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
  
  
  # Manually change design
  observeEvent(input$ed_sel_samples, isolate({
    ted <- exp_design()
    if (length(input$ed_sel_samples > 0)) {
      ted[1, input$ed_sel_samples] <- input$ed_number
      idx <- (ted[1, ] == input$ed_number)
      ted[2, idx] <- 1:sum(idx)
      exp_design(ted)
    }
  }))
  
  # Table for editing design
  output$etable <- DT::renderDT({
    if (!is.null(exp_design())) {
      print("edtable")
      show_table <- t(exp_design())
      show_table[is.na(show_table)] <- 1
      datatable(show_table, editable=T) %>% 
        formatStyle("Group", target="row", 
                    backgroundColor=styleEqual(unique(show_table[,'Group']), 
                                               rainbow(max(show_table[,'Group']), alpha = 0.7)))
    }
  })
  
  observeEvent(input$etable_cell_edit, {
    tdata <- t(exp_design())
    tdata[input$etable_cell_edit$row,input$etable_cell_edit$col] <- input$etable_cell_edit$value
    exp_design(t(tdata))
  })
  
  
  ## Send further to next tab
  observeEvent(input$proceed_to_process, {
    # reordering sample names for easier treatment
    final_exp_design <- exp_design()
    exp_design(final_exp_design[,order(final_exp_design[1,], final_exp_design[2,])])
    pexp_design(exp_design())
    tdata <- indata()
    
    icol <- colnames(tdata)[grep("id",sapply(tdata, class))]
    ccols <- colnames(tdata)[grep("quant",sapply(tdata, class))]
    ocols <- colnames(tdata)[which(!(colnames(tdata) %in% c(icol, ccols)))]
    process_table(tdata[,c(icol, colnames(final_exp_design), ocols)])
    processed_table(process_table())
    
    shinyjs::show("pr_c1")
    shinyjs::show("pr_c2")
    shinyjs::show("pr_c3")
    shinyjs::show("pr_plots")
    updatePickerInput(session,"remove_reps", choices=colnames(exp_design()))
    # check whether unique ids
    if (sum(duplicated(tdata[,icol])) == 0)
      enable("proceed_apps")
    
    tdata <- tdata[,colnames(final_exp_design)]
    # try to find out whether already log-transformed
    if (max(tdata, na.rm=T)/min(tdata, na.rm=T) < 100 && min(tdata, na.rm=T) < 0)
      updateCheckboxInput(session, "logtrafo", value = TRUE)
    updateNumericInput(session, "max_na", min=0, max=ncol(tdata), value=ncol(tdata))
    
    updateTabsetPanel(session, "mainpage",
                      selected = "process")
    
  })
  
  ##### PRE-PROCESSING #########################################################
  ## PCA plot of all selected samples
  output$pca <- renderPlot({
    print("pca")
    tdata <- processed_table()
    tdata <- tdata[, grep("quant",sapply(tdata, class))]
    tdata <- tdata[, colSums(!is.na(tdata)) > 0]
    tdata <- (tdata[complete.cases(tdata), ])
    print(length(tdata))
    shiny::validate(need(
      length(tdata) > 0,
      "Principal component analysis not calculated as too many missing values"
    ))
    shiny::validate(need(
      nrow(tdata) > 20,
      "Principal component analysis not calculated as too many missing values"
    ))
    pca <- prcomp(t(tdata), scale = TRUE, retx = TRUE)
    loadings <- pca$x
    texp_design <- pexp_design()
    plot(loadings, pch = 19, col = rainbow(max(texp_design[1,]))[texp_design[1,]])
    title(main = "Principal component analysis of data set (loadings)", sub =
            "Colors denote different conditions")
    text(loadings, pos=2, labels=colnames(texp_design))
  })
  
  output$corrplot <- renderPlot({
    print("corrplot")
    tdata <- processed_table()
    tdata <- tdata[, grep("quant",sapply(tdata, class))]
    shiny::validate(need(
      nrow(tdata) > 10 & ncol(tdata) > 2,
      "Data matrix too small"
    ))
    gplots::heatmap.2(cor(tdata, use="pairwise.complete.obs"), main="Pairwise correlations between samples",
                      symm=T, scale="none", col=gplots::redblue, breaks=seq(-1,1,0.01), trace = "none")
  })
  
  
  ## Check for balanced exp. design
  output$res_num_reps <- renderText({
    print("check for balancing")
    print(pexp_design())
    # Check whether balanced
    texp_design <- pexp_design()
    ed_stats <- unique(as.vector(table(texp_design[1,])))
    if (length(ed_stats) > 1) {
      disable("proceed_to_apps")
      tout <- paste("This unbalanced design has between ", min(table(pexp_design()[1,])), " and maximally", max(table(pexp_design()[1,])),"replicates for each experimental condition (sample type).")
    } else {
      enable("proceed_to_apps")
      tout <- paste("Experimental design is balanced.")
    }
    tout 
  })
  
  ## Summary of main properties of data table
  output$ptable_summary <- renderText({
    tdata <- processed_table()
    ids <- tdata[, grep("id",sapply(tdata, class))]
    tdata <- tdata[, grep("quant",sapply(tdata, class))]
    shiny::validate(need(
      nrow(tdata) > 50 & ncol(tdata) > 2,
      "Data matrix too small"
    ))
    paste("<p><b>Size: </b>The current data table contains",ncol(tdata),"different samples in ",length(unique(pexp_design()[1,]))," conditions, and is comprised of",nrow(tdata),
          "features.<br/><b>Missingness: </b>The proportion of missing values is",round(sum(is.na(tdata)) / length(as.matrix(tdata)),digits = 3),
          "and the number of missing values varies from",min(colSums(is.na(tdata))),
          "to", max(colSums(is.na(tdata))),"per sample.<br/><b>Range: </b>The dynamic range is from", round(min(tdata, na.rm=T),2),
          "to", round(max(tdata,na.rm=T),digits=2),".<br/><b>Summarization: </b> The id column has",ifelse(sum(duplicated(ids)) > 0,
                                                                                                           "non-unique ids and thus needs to be summarized.",
                                                                                                           "unique ids and thus does not need to be summarized</p>"))
  })
  
  ## data operations like removing reps, log, nas, and normalization
  observe({
    input$remove_reps
    input$logtrafo
    input$normalization
    input$max_na
    input$add_na_columns
    input$summarize
    tdata <- process_table()
    isolate({
      if (!is.null(tdata)) {
        withProgress(message="Calculating...", value=0, min=0, max=1, {
          
          cnames <- colnames(exp_design())
          icol <- colnames(processed_table())[grep("id",sapply(tdata, class))]
          
          # log-transformation
          if (!input$logtrafo) {
            for (cn in cnames) {
              tdata[,cn] <- log2(tdata[,cn])
              tdata[!is.finite(tdata[,cn]), cn] <- NA
              incProgress(0.3, detail="log-transformation")
            }
          }
          
          
          # move out to avoid repeated calculations and complications?
          # summarize replicates with the same number
          candreps <- paste(exp_design()[1,],exp_design()[2,], sep="_")
          print("summarizing replicates")
          if (sum(duplicated(candreps)) > 0) {
            incProgress(0.5, detail="Summarizing replicates")
            
            # Initialize summarized_reps matrix with NA values
            summarized_reps <- matrix(NA, nrow = nrow(tdata), ncol = length(unique(candreps)))
            
            # Loop over each row of tdata
            for (r in 1:nrow(tdata)) {
              # Group elements in row by candreps values and sum them, replacing 0 with NA
              tsumm <- as.vector(unlist(by(unlist(tdata[r, cnames]), candreps, sum, na.rm = T)))
              tsumm[tsumm == 0] <- NA
              
              # Assign tsumm as a row in summarized_reps
              summarized_reps[r,] <- tsumm
            }
            
            # Assign new column names to summarized_reps
            new_cnames <- cnames[!duplicated(candreps)]
            colnames(summarized_reps) <- new_cnames
            
            # Update exp_design and tdata with summarized_reps
            exp_design(exp_design()[,new_cnames])
            tdata <- tdata[,-which(names(tdata) %in% cnames),drop = F]
            tdata <- cbind(tdata, summarized_reps)
            cnames <- new_cnames
            
            # Update picker input with new column names
            updatePickerInput(session, "remove_reps", choices = colnames(exp_design()))            new_cnames <- cnames[!duplicated(candreps)]
          }
          
          # removing reps
          print(paste0("removing", input$remove_reps))
          rem <- -which(names(tdata) %in% input$remove_reps)
          if (length(rem) > 0) {
            incProgress(0.1, detail="Removing replicates")
            tdata <- tdata[,rem]
            rem2 <- -which(cnames %in% input$remove_reps)
            pexp_design(exp_design()[,rem2])
            cnames <- cnames[rem2]
          }            
          
          
          # NAs
          tdata <- tdata[rowSums(is.na(tdata[,cnames])) <= input$max_na, ]
          
          # Normalize
          incProgress(0.6, detail="Normalizing")
          if (input$normalization == "colMedians") {
            tdata[,cnames] <- t(t(tdata[,cnames]) - colMedians(as.matrix(tdata[,cnames]), na.rm=T))
          } else if(input$normalization == "colMeans") {
            tdata[,cnames] <- t(t(tdata[,cnames]) - colMeans(as.matrix(tdata[,cnames]), na.rm=T))
          } else {
            tdata[,cnames] <- limma::normalizeBetweenArrays(tdata[,cnames], method=input$normalization)
          }
          
          
          # Summarization (from MsCoreUtils package)
          if (input$summarize != "none") {
            incProgress(0.8, detail="Summarization")
            
            if (input$summarize ==  "sum") {
              summarized <- MsCoreUtils::aggregate_by_vector(2^as.matrix(tdata[,cnames]), 
                                                             tdata[,icol], 
                                                             input$summarize,
                                                             na.rm=T)
              summarized <- log2(summarized)
            } else {
              summarized <- MsCoreUtils::aggregate_by_vector(as.matrix(tdata[,cnames]), 
                                                             tdata[,icol], 
                                                             input$summarize,
                                                             na.rm=T)
            }
            tdata <- data.frame(summarized_ids=rownames(summarized), summarized)
            colnames(tdata)[1] <- icol
          }
          
          ## add empty columns if requested
          if (input$add_na_columns) {
            print("adding NA columns")
            reps <- table(pexp_design()[1,])
            max_reps <- max(reps)
            tedes <- pexp_design()
            for (cond in unique(tedes[1,])) {
              tt <- tedes[, tedes[1,] == cond, drop=F]
              print(reps)
              print(max_reps)
              for (i in seq_len(max_reps - reps[cond])) {
                tedes <- cbind(tedes, c(cond, max(tt[2,]) + i))
                colnames(tedes)[ncol(tedes)] <- paste0("new", reps[cond], "_", i)
              }
            }
            print(tedes)
            pexp_design(tedes)
          }
          
          # set class
          for (cn in cnames) {
            class(tdata[,cn]) <- "quant"
          }
          class(tdata[,icol]) <- "id"
          
          # check whether unique ids and balanced design
          ed_stats <- unique(as.vector(table(pexp_design()[1,])))        
          if (sum(duplicated(is.na(tdata[,icol]))) == 0 && ed_stats == 1) {
            enable("proceed_apps")
          } else {
            disable("proceed_apps")
          }
          processed_table(tdata)
          
        })
      }
    })
  })
  
  ## Send further to next tab
  observeEvent(input$proceed_to_apps, {
    updateTabsetPanel(session, "mainpage",
                      selected = "apps")
    # reordering sample names for easier treatment
    final_exp_design <- pexp_design()
    tdata <- processed_table()
    
    icol <- colnames(tdata)[grep("id",sapply(tdata, class))]
    ccols <- colnames(tdata)[grep("quant",sapply(tdata, class))]
    ocols <- colnames(tdata)[which(!(colnames(tdata) %in% c(icol, ccols)))]
    
    
    shinyjs::show("app_c1")
    shinyjs::show("app_c2")
    shinyjs::show("app_c3")
    # shinyjs::show("pr_plots")
    # updatePickerInput(session,"remove_reps", choices=colnames(exp_design()))
    
  })
  
  
  ##### SEND TO APPS #########################################################
  ## Download table
  output$downloadTable <- downloadHandler(
    filename = function() {
      validate(
        need(NULL,"No data")
      )      
      paste("Results", Sys.Date(), ".csv", sep="");
    },
    content = function(file) {
      write.csv(result_table(), file)
    }
  )
  
  ## Show table
  output$rtable <- DT::renderDT({
    datatable(result_table(), options = list(scrollX = TRUE))
  })
  
  
  ## VSClust
  # Sent data to VSClust
  observeEvent(input$send_vsclust, isolate({
    # make table in right format
    tdata <- processed_table()
    outdat <- as.matrix(tdata[,grep("quant",sapply(tdata, class))])
    outdat <- cbind(tdata[,grep("id",sapply(tdata, class))], outdat)
    final_exp_design <- pexp_design()
    NumCond <- length(unique(final_exp_design[1,]))
    NumReps <- table(final_exp_design[1,])[1]
    # print(outdat)
    VSClustMessage <- toJSON(list(numrep=NumReps, numcond=NumCond, grouped=F, paired=input$paired, modsandprots=F, 
                                  expr_matrix=as.list(as.data.frame(outdat))))
    updateTextInput(session, "app_log", value="Opening VSClust and data upload ...")
    js$send_message(url=input$url_vsclust, dat=VSClustMessage, tool="VSClust")
    enable("retrieve_vsclust")
  }))
  
  # Log for VSClust
  output$connection_vsclust <- renderText({
    toutput <- log_vsclust()
    #print(input$app_log)
    if (input$app_log != "" & !is.null(input$app_log)) {
      if (grepl("vsclust", tolower(input$app_log))) {
        toutput <- input$app_log
        #print(toutput)
        log_vsclust(toutput)
        updateTextInput(session, "app_log", value="")
      }
    }
    toutput
  })
  
  # Sending message to retrieve results
  observeEvent(input$retrieve_vsclust, isolate({
    updateTextInput(session, "app_log", value="Getting VSClust results")
    js$retrieve_results(url=input$url_vsclust, dat="Retrieve results", tool="VSClust",date=date())
  }))
  
  
  # Merging PolySTest results into result r_table
  observeEvent(input$vsclust_results, isolate({
    print("Processing VSClust results")
    if (is.list(input$vsclust_results)) {
      print("data table received")
      tdata <- NULL
      for (n in names(input$vsclust_results[[1]]))
        tdata <- cbind(tdata, as.numeric(input$vsclust_results[[1]][[n]]))
      colnames(tdata) <- names(input$vsclust_results[[1]])
      # print(head(tdata))
      # print(summary(input$polystest_results[[1]]))
      # print(dim(as.data.frame(input$polystest_results[[1]])))
      if (is.null(result_table)) {
        result_table(cbind(processed_table(),tdata))
      } else {
        result_table(cbind(result_table(),tdata))
      }
      updateTextInput(session, "app_log", value="Processed VSClust results")
    }
  }))
  
  
  ## PolySTest
  # Sent data to PolySTest
  observeEvent(input$send_polystest, isolate({
    # make table in right format
    tdata <- processed_table()
    outdat <- as.matrix(tdata[,grep("quant",sapply(tdata, class))])
    outdat <- cbind(tdata[,grep("id",sapply(tdata, class))], outdat)
    final_exp_design <- pexp_design()
    NumCond <- length(unique(final_exp_design[1,]))
    NumReps <- table(final_exp_design[1,])[1]
    # print(outdat)
    PolySTestMessage <- toJSON(list(numrep=NumReps, numcond=NumCond, grouped=F, paired=input$paired, firstquantcol=2, 
                                    expr_matrix=as.list(as.data.frame(outdat))))
    updateTextInput(session, "app_log", value="Opening PolySTest and data upload ...")
    js$send_message(url=input$url_polystest, dat=PolySTestMessage, tool="PolySTest")
    enable("retrieve_polystest")
  }))
  
  # Log for PolySTest
  output$connection_polystest <- renderText({
    toutput <- log_polystest()
    #print(input$app_log)
    if (!is.list(input$app_log) & input$app_log != "" & !is.null(input$app_log)) {
      if (grepl("polystest", tolower(input$app_log))) {
        toutput <- input$app_log
        #print(toutput)
        log_polystest(toutput)
        updateTextInput(session, "app_log", value="")
      }
    }
    toutput
  })
  
  # Sending message to retrieve results
  observeEvent(input$retrieve_polystest, isolate({
    updateTextInput(session, "app_log", value="Getting PolySTest results")
    js$retrieve_results(url=input$url_polystest, dat="Retrieve results", tool="PolySTest")
  }))
  
  # Merging PolySTest results into result r_table
  observeEvent(input$polystest_results, isolate({
    print("Processing PolySTest results")
    if (is.list(input$polystest_results)) {
      print("data table received")
      # jsonmessage <- fromJSON(input$polystest_results)
      # print(head(jsonmessage[["expr_matrix"]]))
      tdata <- NULL
      for (n in names(input$polystest_results[[1]]))
        tdata <- cbind(tdata, as.numeric(input$polystest_results[[1]][[n]]))
      colnames(tdata) <- names(input$polystest_results[[1]])
      # print(head(tdata))
      # print(summary(input$polystest_results[[1]]))
      # print(dim(as.data.frame(input$polystest_results[[1]])))
      if (is.null(result_table())) {
        result_table(cbind(processed_table(),tdata))
      } else {
        result_table(cbind(result_table(),tdata))
      }
      updateTextInput(session, "app_log", value="Processed PolySTest results")
    }
  }))
  
  ## ComplexBrowser
  # Sending data to ComplexBrowser
  observeEvent(input$send_complexbrowser, isolate({
    # make table in right format
    tdata <- processed_table()
    outdat <- as.matrix(tdata[,grep("quant",sapply(tdata, class))])
    outdat <- cbind(tdata[,grep("id",sapply(tdata, class))], outdat)
    final_exp_design <- pexp_design()
    NumCond <- length(unique(final_exp_design[1,]))
    NumReps <- table(final_exp_design[1,])[1]
    # print(outdat)
    ##TODO allow PolySTest input for including statistics
    ComplexBrowserMessage <- toJSON(list(numrep=NumReps, numcond=NumCond, grouped=F, paired=input$paired, withstats=F, 
                                         expr_matrix=as.list(as.data.frame(outdat))))
    updateTextInput(session, "app_log", value="Opening ComplexBrowser and data upload ...")
    js$send_message(url=input$url_complexbrowser, dat=ComplexBrowserMessage, tool="ComplexBrowser")
  }))
  
  # Log for ComplexBrowser
  output$connection_complexbrowser <- renderText({
    toutput <- log_complexbrowser()
    #print(input$app_log)
    if (input$app_log != "" & !is.null(input$app_log)) {
      if (grepl("complexbrowser", tolower(input$app_log))) {
        toutput <- input$app_log
        #print(toutput)
        log_complexbrowser(toutput)
        updateTextInput(session, "app_log", value="")
      }
    }
    toutput
  })
  
  
  ############### Help messages #########################################################
  observeEvent(input$h_pfile, sendSweetAlert(
    session,
    title="File types",
    text=HTML("<p align='justify'>OmicsQ can read <i>Excel</i> files, and tables in <i>textual format</i> like csv, tsv and others.<br/> 
    The underlying function tries to automatically determine delimiters and digit separators.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_csv_input, sendSweetAlert(
    session,
    title="Options of textual input file",
    text=HTML("<p align='justify'><i>Delimiter:</i> Specify the character that separates the values. Change only if 'auto' does not provide the correct table.<br/>
              <i>Decimal separator:</i> Specify the character to denote decimals.<br/>
              <i>Remove lines at beginning:</i> Sometimes, a textual format has a header spanning more than one line. You have the option to ignore
              a number of lines at the start of the files. This information will be lost.<br/>
              <i>Does file have a header: </i>In case that the file does not have a first row with information about the data columns, deselect this option.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_sel_id_col, sendSweetAlert(
    session,
    title="Select relevant columns",
    text=HTML("<p align='justify'>Select the columns you want to make the ID column and the quantitative columns. You can search for multiple colums. 
              This is particularly useful when your quantitative columns have similar columns names.<br/><i>ID column:</i> Select column with the main features. These can be e.g. gene ids, protein ids, or peptide sequences. 
              They do not need to be unique as we offer summarization in the following analysis. The main analysis will take place on a unique set of IDs.<br/>
              <i>Quantitative columns: </i> These are the columns with values we will use in the analyses. They usually are quantifications of the features 
              in the ID column (e.g. protein abundances or gene expressions).<br>
              ID and quant columns are marked in the table below according to your selection. You need to select them
              sto proceed to the next analysis step.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_remove_zeroes, sendSweetAlert(
    session,
    title="Simple data manipulation",
    text=HTML("<p align='justify'><i>Zeroes to missing values: </i>Missed measurements are often given by 
              zeroes. As the actual value of the features is not known, we make them missing values 
              (NA or not available in R)<br/>
              <i>Non-numeric to missing values: </i>Data manipulations with software like Excel can lead to values like 
              '#DIV/0!'. This buttons converts any non-numeric value into a missing values. Only purely numeric <i>quant</i> columns
              are accepted to continue the analysis.</p>"),
    type="info",
    html = T
  ))
  
  
  observeEvent(input$h_proceed_expdesign, sendSweetAlert(
    session,
    title="Ready to proceed?",
    text=HTML("<p align='justify'>In order to change to define the experimental design, you need to have selected
            an <i>ID</i> column and multiple numeric <i>quant</i> columns.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_exp_design, sendSweetAlert(
    session,
    title="Estimate design",
    text=HTML("<p align='justify'><i>General: </i>We estimate the experimental design from the similarity
              between column names. Try the different string distances below and play with the threshold
              to find the optimal setting. <br/>
              You can further <i>modify</i> the experimental design by double clicking on the respective entry in
              the table.<br/>
              <i>Explanation of table: </i>Group denote the experimental condition, i.e. the group of sample
              of the same type like drug, disease or time points. Different conditions are given by different numbers.
              Replicates correspons to biological or technical samples of the same type such as different mice with the 
              same mutation or the same sample being rerun on the instrument. Here, a different replicate needs to be 
              given by a different number (within all replicates of a condition). Replicates with the same number within
              the same conditions will be summarized in the next step. Summarization means the values of the resulting
              replicate will be given by the sum of the summarized replicates.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_balancing, sendSweetAlert(
    session,
    title="Create a balanced design",
    text=HTML("<p align='justify'>The different experimental conditions (sample types) need to be at least nearly balanced. For the 
              further analysis, this means that the number of replicates per sample should be the same for each of them. You can balance the data
              by adding empty columns and/or removing excess replicates. Replicates with the same number have already been summarized.<br/>
              <i>Select beliow</i> the samples you wnat to exclude.<br/>
              <i>Fill with empty columns: </i>Use this switch to reach the same number of replicates per condition. This does <b>not make a data set more balanced</b>. 
              We recommend removing samples from groups that have a much larger number of replicates.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_logtrafo, sendSweetAlert(
    session,
    title="Log transformation",
    text=HTML("<p align='justify'>In general, quantitative omics data is log-transformed. 
              OmicsQ estimates whether the data was transformed from the values (data range and 
              negative values). Deselecting this box will transform the data by taking the logarithm
              with base 2.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_max_na, sendSweetAlert(
    session,
    title="Deleting features with low coverage",
    text=HTML("<p align='justify'>Despite of the capability of PolySTest and VSClust to include features
              with missing values, we recommend features that have very low coverage as their measurements
              are usually very noisy. Filter for the maximum number of missing values here. The default is 
              to take all features.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_normalization, sendSweetAlert(
    session,
    title="Normalization",
    text=HTML("<p align='justify'>The total amount of molecules per sample can vary when loading them 
              into the instrument. This systematic error can be corrected by assuming that most 
              of the features do not change between samples. We offer the main normalization methods
              used in the field.<br/>
              <i>None:</i> Do no normalize (or has already been normalized)<br/>
              <i>Median: </i>Subtract the median of each sample from the log-transformed values (recommended 
              as more outlier insensitive)<br/>
              <i>Mean: </i>Subtract the mean of the sample from the log-transformed values<br/>
              <i>Cyclic Loess: </i>Apply a non-linear transformation to accommodate for non-linear effects and 
              large changes and batch effects in the data (see <a href='http://web.mit.edu/~r/current/arch/i386_linux26/lib/R/library/limma/html/normalizeCyclicLoess.html'>cyclicLoess</a>. <br/></p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_summarize, sendSweetAlert(
    session,
    title="Summarization of feature values",
    text=HTML("<p align='justify'><i>This option is only needed for id columns with duplicated values.</i> 
              This can be multiple features measured over all samples like peptides of the same protein.
              Check the data summary on the right side whether there are any duplicates.
              The here given options summarize the rows with the same feature name using one of the following
              methods:<br/>
              <i>None: </i>Do not summarize. In the case of duplicated id values, you won't be able to proceed (default)<br/>
              <i>Sum: </i>Here we take the sum of the non-log-transformed values. This can have impact on the normalization.<br/>
              <i>Mean: </i>Take the mean of log-transformed values<br/>
              <i>Median: </i>Take the median of log-transformed values<br/>
              <i>Robust median: </i>Take the median of log-transformed values but remove outliers 
              (see <a href='https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/medpolish'>medpolish</a>)<br/>
              <i>Note: </i>Summarization happens after normalization.</p>"),
    type="info",
    html = T
  ))
  
  observeEvent(input$h_apps, sendSweetAlert(
    session,
    title="Call apps for further analysis",
    text=HTML("<p align='justify'>You can submit your data set to the apps 
              <a href='http://computproteomics.bmb.sdu.dk/Apps/PolySTest'>PolySTest</a>, 
              <a href='http://computproteomics.bmb.sdu.dk/Apps/PolySTest'>VSClust</a>, and 
              <a href='http://computproteomics.bmb.sdu.dk/Apps/PolySTest'>ComplexBrowser</a>. The
              URLs are set to the public servers which should be fully functional but 
              might be busy due to limited user access.<br/>
              If you, for example due to privacy reasons, want to call the apps on a local or another 
              server, please change the URL fields to the respective addresses.<br/>
              <i>Paired vs unpaired design: </i>Do you have pairwise relations between samples (e.g. before 
              and after treatment of the same patients)? If yes, remember to keep the replicates in the 
              right order when selecting paired design.<br/>
              <i>Note:</i>When using OmicsQ the first time, your security settings might require you to allow
              opening new tabs!</br/>
              <i>Note:</i> Depending on the size of the data set, the data upload could fail due to a too slow 
              internet connection.</p>"),
    type="info",
    html = T
  ))
  
}
