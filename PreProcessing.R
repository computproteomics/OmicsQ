#################### UI ##################
preProcessingUI <- function(id, prefix="") {
  ns <- NS(id)
  tagList(
    fluidRow(
      hidden(column(width=4, id=ns("pr_c1"),
                    h4("Add or delete data columns"),
                    fluidRow(column(10,pickerInput(ns("remove_reps"), "Pick the samples you want to remove", choices = NULL, multiple=T,
                                                   options = list(
                                                     `live-search` = TRUE,
                                                     `actions-box` = TRUE))),
                             column(2, actionBttn(ns("h_balancing"),
                                                  icon=icon("info-circle"),
                                                  style="pill", 
                                                  color = "royal", size = "xs")
                             )),
                    switchInput(ns("add_na_columns"), "Fill with empty columns", 
                                value=FALSE, labelWidth = 50),
                    h4("The current state:"),
                    p(textOutput(ns("res_num_reps")), style="text-color:#AA2222")
      )),
      hidden(column(width=3, id=ns("pr_c2"),
                    h4("Data manipulation and adjustment"),
                    fluidRow(column(10,checkboxInput(ns("logtrafo"), "Is the data already log-transformed?", value=F)),
                             column(2, actionBttn(ns("h_logtrafo"),
                                                  icon=icon("info-circle"),
                                                  style="pill", 
                                                  color = "royal", size = "xs")
                             )),
                    fluidRow(column(10,numericInput(ns("max_na"), label="Maximum number of missing values per feature", 
                                                    min=0, max=0, step=1, value=100)),
                             column(2, actionBttn(ns("h_max_na"),
                                                  icon=icon("info-circle"),
                                                  style="pill", 
                                                  color = "royal", size = "xs")
                             )),
                    fluidRow(column(10,selectInput(ns("normalization"), label="Normalization method",
                                                   choices=c(None="none", Median="colMedians", "Mean"="colMeans","Cyclic LOESS (LIMMA)"="cyclicloess"),
                                                   selected="none")),
                             column(2, actionBttn(ns("h_normalization"),
                                                  icon=icon("info-circle"),
                                                  style="pill", 
                                                  color = "royal", size = "xs")
                             )),
                    fluidRow(column(10,selectInput(ns("summarize"), label="Summarize to id features", 
                                                   choices=c(None="none","By sum"="colSums",
                                                             "By mean"="colMeans", "By median"="colMedians", 
                                                             "Robust median (medpolish)"="medianPolish"
                                                             #"Robust summary (rlm)"="robustSummary" Does not work with missing values
                                                   ))),
                             column(2, actionBttn(ns("h_summarize"),
                                                  icon=icon("info-circle"),
                                                  style="pill", 
                                                  color = "royal", size = "xs")
                             )),
                    style = 'border-left: 1px solid' 
      )              
      ),
      hidden(column(4,id=ns("pr_c3"),
                    h4("Summary:"),
                    htmlOutput(ns("ptable_summary"),style="border:solid;border-width:1px;"),
                    h5("Proceed to interaction with apps"),
                    textOutput(ns("txt_proceed_apps")),
                    disabled(actionButton(ns("proceed_to_apps"), "Proceed")),
                    style = 'border-left: 1px solid'    
      ))),        
    
    hidden(fluidRow(id=ns("pr_plots"),
                    column(5,
                           plotOutput(ns("pca"))),
                    column(5,
                           plotOutput(ns("corrplot"))
                    ))
    )
    
  )
}


###### Server ###########
preProcessingServer <- function(id, parent, expDesign, log_operations) {
  moduleServer(
    id,
    function(input, output, session) {
      
      process_table <- reactiveVal(NULL)
      processed_table <- reactiveVal(NULL)
      result_table <- reactiveVal(NULL)
      pexp_design <- reactiveVal(NULL)
      exp_design <- reactiveVal(NULL)
      next_tab <-reactiveVal(NULL)
      
      
      observeEvent(expDesign$next_tab(), {
        if (!is.null(expDesign$next_tab())) {
          processed_table(expDesign$process_table())
          process_table(expDesign$process_table())
          pexp_design(expDesign$pexp_design())
          exp_design(expDesign$pexp_design())
          
          shinyjs::show("pr_c1")
          shinyjs::show("pr_c2")
          shinyjs::show("pr_c3")
          shinyjs::show("pr_plots")
          updatePickerInput(session, "remove_reps", choices = colnames(exp_design()))
          # # check whether unique ids
          # if (sum(duplicated(tdata[, icol])) == 0) {
          #   enable("proceed_apps")
          # }
          
          tdata <- process_table()
          tdata <- tdata[, colnames(exp_design())]
          # try to find out whether already log-transformed
          tlog <- log_operations()
          if (max(tdata, na.rm = T) / min(tdata, na.rm = T) < 100 ||
              min(tdata, na.rm = T) <
              0) {
            updateCheckboxInput(session, "logtrafo", value = TRUE)
            tlog[["preprocess_take_log2"]] <- FALSE
          } else {
            tlog[["preprocess_take_log2"]] <- TRUE
          }
          updateNumericInput(session, "max_na",
                             min = 0, max = ncol(tdata),
                             value = ncol(tdata)
          )
          log_operations(tlog)
          
        }
      })
      
      ##### all selected samples
      output$pca <- renderPlot({
        print("pca")
        tdata <- processed_table()
        texp_design <- pexp_design()
        tdata <- tdata[, grep("quant", sapply(tdata, class))]
        tdata <- tdata[, colSums(!is.na(tdata)) > 0]
        texp_design <- texp_design[, colnames(tdata)]
        tdata <- (tdata[complete.cases(tdata), ])
        print(length(tdata))
        shiny::validate(need(length(tdata) > 0, "Principal component analysis not
    calculated as too many missing values"))
        shiny::validate(need(nrow(tdata) > 20, "Principal component analysis not
    calculated as too many missing values"))
        pca <- prcomp(t(tdata), scale = TRUE, retx = TRUE)
        loadings <- pca$x
        plot(loadings,
             pch = 19,
             col = rainbow(max(texp_design[1, ]))[texp_design[1, ]]
        )
        title(
          main = "Principal component analysis of data set (loadings)",
          sub = "Colors denote different conditions"
        )
        text(loadings, pos = 2, labels = colnames(texp_design))
      })
      
      output$corrplot <- renderPlot({
        print("corrplot")
        tdata <- processed_table()
        tdata <- tdata[, grep("quant", sapply(tdata, class))]
        tdata <- tdata[, colSums(!is.na(tdata)) > 0]
        shiny::validate(need(nrow(tdata) > 10 &
                               ncol(tdata) > 2, "Data matrix too small"))
        gplots::heatmap.2(cor(tdata, use = "pairwise.complete.obs"),
                          main = "Pairwise correlations between samples",
                          symm = TRUE, scale = "none", col = gplots::redblue, breaks = seq(
                            -1, 1,
                            0.01
                          ), trace = "none"
        )
      })
      
      
      ## Check for balanced exp. design
      output$res_num_reps <- renderText({
        print("check for balancing")
        print(pexp_design())
        # Check whether balanced
        texp_design <- pexp_design()
        ed_stats <- unique(as.vector(table(texp_design[1, ])))
        if (length(ed_stats) > 1) {
          disable("proceed_to_apps")
          tout <- paste(
            "This unbalanced design has between ",
            min(table(pexp_design()[1, ])),
            " and maximally", max(table(pexp_design()[1, ])),
            "replicates for each experimental condition (sample type)."
          )
        } else {
          enable("proceed_to_apps")
          tout <- paste("Experimental design is balanced.")
        }
        tout
      })
      
      ## Summary of main properties of data table
      output$ptable_summary <- renderText({
        tdata <- processed_table()
        ids <- tdata[, grep("id", sapply(tdata, class))]
        tdata <- tdata[, grep("quant", sapply(tdata, class))]
        shiny::validate(need(nrow(tdata) > 50 & ncol(tdata) > 2, "Data matrix
    too small"))
        paste(
          "<p><b>Size: </b>The current data table contains", ncol(tdata),
          "different samples in ",
          length(unique(pexp_design()[1, ])), " conditions, and is comprised of",
          nrow(tdata), "features.<br/><b>Missingness: </b>The proportion of
      missing values is",
          round(sum(is.na(tdata)) / length(as.matrix(tdata)), digits = 3),
          "and the number of missing values varies from",
          min(colSums(is.na(tdata))), "to", max(colSums(is.na(tdata))),
          "per sample.<br/><b>Range: </b>The dynamic range is from",
          round(min(tdata, na.rm = T), 2), "to", round(max(tdata, na.rm = T),
                                                       digits = 2
          ),
          ".<br/><b>Summarization: </b> The id column has",
          ifelse(sum(duplicated(ids)) >
                   0, "non-unique ids and thus needs to be summarized.",
                 "unique ids and thus does not need to be summarized</p>"
          )
        )
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
            withProgress(message = "Calculating...", value = 0, min = 0, max = 1, {
              cnames <- colnames(exp_design())
              icol <- colnames(process_table())[grep("id", sapply(
                tdata,
                class
              ))]
              
              # log-transformation
              tlog <- log_operations()
              if (!input$logtrafo) {
                for (cn in cnames) {
                  tdata[, cn] <- log2(tdata[, cn])
                  tdata[!is.finite(tdata[, cn]), cn] <- NA
                  incProgress(0.3, detail = "log-transformation")
                }
                tlog[["preprocess_take_log2"]] <- TRUE
              } else {
                tlog[["preprocess_take_log2"]] <- FALSE
              }
              
              # move out to avoid repeated calculations and
              # complications?  summarize replicates with the same number
              candreps <- paste(exp_design()[1, ], exp_design()[2, ], sep = "_")
              print("summarizing replicates")
              if (sum(duplicated(candreps)) > 0) {
                incProgress(0.5, detail = "Summarizing replicates")
                
                # Initialize summarized_reps matrix with NA values
                summarized_reps <- matrix(NA,
                                          nrow = nrow(tdata),
                                          ncol = length(unique(candreps))
                )
                
                # Loop over each row of tdata
                for (r in 1:nrow(tdata)) {
                  # Group elements in row by candreps values and sum
                  # them, replacing 0 with NA
                  tsumm <- as.vector(unlist(by(unlist(tdata[r, cnames]), candreps,
                                               sum,
                                               na.rm = T
                  )))
                  tsumm[tsumm == 0] <- NA
                  
                  # Assign tsumm as a row in summarized_reps
                  summarized_reps[r, ] <- tsumm
                }
                
                # Assign new column names to summarized_reps
                new_cnames <- cnames[!duplicated(candreps)]
                colnames(summarized_reps) <- new_cnames
                
                # Update exp_design and tdata with summarized_reps
                exp_design(exp_design()[, new_cnames])
                tdata <- tdata[, -which(names(tdata) %in% cnames), drop = F]
                tdata <- cbind(tdata, summarized_reps)
                cnames <- new_cnames
                
                # Update picker input with new column names
                updatePickerInput(session, "remove_reps",
                                  choices = colnames(exp_design())
                )
                new_cnames <- cnames[!duplicated(candreps)]
              }
              
              # removing reps
              print(paste0("removing", input$remove_reps))
              rem <- -which(names(tdata) %in% input$remove_reps)
              if (length(rem) > 0) {
                incProgress(0.1, detail = "Removing replicates")
                tdata <- tdata[, rem]
                rem2 <- -which(cnames %in% input$remove_reps)
                pexp_design(exp_design()[, rem2])
                cnames <- cnames[rem2]
                tlog[["preprocess_removed_replicates"]] <- input$remove_reps
              } else {
                # make sure that no relict column is still there in pexp_design?
                pexp_design(exp_design())
              }
              
              
              # NAs
              tdata <- tdata[rowSums(is.na(tdata[, cnames])) <= input$max_na, ]
              tlog[[paste0("preprocess_max_missing_values")]] <- input$max_na
              
              # Normalize
              incProgress(0.6, detail = "Normalizing")
              if (input$normalization == "colMedians") {
                tdata[, cnames] <- t(t(tdata[, cnames]) -
                                       colMedians(as.matrix(tdata[
                                         ,
                                         cnames
                                       ]), na.rm = T))
              } else if (input$normalization == "colMeans") {
                tdata[, cnames] <- t(t(tdata[, cnames]) -
                                       colMeans(as.matrix(tdata[
                                         ,
                                         cnames
                                       ]), na.rm = T))
              } else {
                tdata[, cnames] <- limma::normalizeBetweenArrays(tdata[, cnames],
                                                                 method = input$normalization
                )
              }
              tlog[["preprocess_normalization"]] <- input$normalization
              
              
              # Summarization (from MsCoreUtils package)
              if (input$summarize != "none") {
                incProgress(0.8, detail = "Summarization")
                
                if (input$summarize == "sum") {
                  summarized <- MsCoreUtils::aggregate_by_vector(2^as.matrix(tdata[
                    ,
                    cnames
                  ]), tdata[, icol], input$summarize, na.rm = T)
                  summarized <- log2(summarized)
                } else {
                  summarized <- MsCoreUtils::aggregate_by_vector(as.matrix(tdata[
                    ,
                    cnames
                  ]), tdata[, icol], input$summarize, na.rm = T)
                }
                tdata <- data.frame(
                  summarized_ids = rownames(summarized),
                  summarized
                )
                colnames(tdata)[1] <- icol
              }
              tlog[["preprocess_summarization"]] <- input$summarize
              
              ## add empty columns if requested
              if (input$add_na_columns) {
                print("adding NA columns")
                reps <- table(pexp_design()[1, ])
                max_reps <- max(reps)
                tedes <- pexp_design()
                for (cond in unique(tedes[1, ])) {
                  tt <- tedes[, tedes[1, ] == cond, drop = F]
                  for (i in seq_len(max_reps - reps[cond])) {
                    tedes <- cbind(tedes, c(cond, max(tt[2, ]) + i))
                    colnames(tedes)[ncol(tedes)] <- paste0(
                      "new", cond, "_",
                      i
                    )
                    tdata[paste0("new", cond, "_", i)] <- NA
                    # print(head(tdata, 1))
                    print(paste0("new", cond, "_", i))
                  }
                }
                # reorder columns according to experimental design
                tedes <- tedes[, order(tedes[1, ], tedes[2, ])]
                cnames <- colnames(tedes)
                pexp_design(tedes)
              }
              # print(-(colnames(tdata) %in% cnames))
              # print(cnames)
              tdata <- data.frame(
                tdata[, !(colnames(tdata) %in% cnames), drop = F],
                tdata[, cnames]
              )
              tlog[["preprocess_number_of_empty_columns"]] <- sum(
                colSums(is.na(tdata[, cnames])) == nrow(tdata)
              )
              log_operations(tlog)
              
              # set class
              for (cn in cnames) {
                class(tdata[, cn]) <- "quant"
              }
              class(tdata[, icol]) <- "id"
              
              # check whether unique ids and balanced design
              ed_stats <- unique(as.vector(table(pexp_design()[1, ])))
              if (sum(duplicated(is.na(tdata[, icol]))) == 0 && ed_stats ==
                  1) {
                enable("proceed_apps")
              } else {
                disable("proceed_apps")
              }
              processed_table(tdata)
              
              # print(head(processed_table(), 1))
              # print(exp_design())
            })
          }
        })
      })
      
      ## Send further to next tab
      observeEvent(input$proceed_to_apps, {
        updateTabsetPanel(parent, "mainpage", selected = "apps")
        
        if (!is.null(next_tab())) {
          next_tab(paste0(next_tab(), "_new"))
        } else {
          next_tab("ready")
        }

                # reordering sample names for easier treatment
        # final_exp_design <- pexp_design()
        # tdata <- processed_table()
        # 
        # icol <- colnames(tdata)[grep("id", sapply(tdata, class))]
        # ccols <- colnames(tdata)[grep("quant", sapply(tdata, class))]
        # ocols <- colnames(tdata)[which(!(colnames(tdata) %in% c(icol, ccols)))]
        
      })
      
      
      
      ############### Help messages
      observeEvent(input$h_balancing, sendSweetAlert(session,
                                                     title = "Create a balanced design",
                                                     text = HTML("<p align='justify'>The different experimental conditions
    (sample types) need to be at least nearly balanced. For the
              further analysis, this means that the number of replicates
              per sample should be the same for each of them. You can balance the data
              by adding empty columns and/or removing excess replicates.
              Replicates with the same number have already been summarized.<br/>
              <i>Select beliow</i> the samples you wnat to exclude.<br/>
              <i>Fill with empty columns: </i>Use this switch to reach
              the same number of replicates per condition. This does
              <b>not make a data set more balanced</b>.
              We recommend removing samples from groups that have a
              much larger number of replicates.</p>"),
                                                     type = "info", html = T
      ))
      
      observeEvent(input$h_logtrafo, sendSweetAlert(session,
                                                    title = "Log transformation",
                                                    text = HTML("<p align='justify'>In general, quantitative omics data is log-transformed.
              OmicsQ estimates whether the data was transformed from the values (data range and
              negative values). Deselecting this box will transform the data by taking the logarithm
              with base 2.</p>"),
                                                    type = "info", html = T
      ))
      
      observeEvent(input$h_max_na, sendSweetAlert(session,
                                                  title = "Deleting features with low coverage",
                                                  text = HTML("<p align='justify'>Despite of the capability of PolySTest and VSClust to include features
              with missing values, we recommend features that have very low coverage as their measurements
              are usually very noisy. Filter for the maximum number of missing values here. The default is
              to take all features.</p>"),
                                                  type = "info", html = T
      ))
      
      observeEvent(input$h_normalization, sendSweetAlert(session,
                                                         title = "Normalization",
                                                         text = HTML("<p align='justify'>The total amount of molecules per sample can vary when loading them
              into the instrument. This systematic error can be corrected by assuming that most
              of the features do not change between samples. We offer the main normalization methods
              used in the field.<br/>
              <i>None:</i> Do no normalize (or has already been normalized)<br/>
              <i>Median: </i>Subtract the median of each sample from the log-transformed values (recommended
              as more outlier insensitive)<br/>
              <i>Mean: </i>Subtract the mean of the sample from the log-transformed values<br/>
              <i>Cyclic Loess: </i>Apply a non-linear transformation to accommodate for non-linear effects and
              large changes and batch effects in the data (see <a href='http://web.mit.edu/~r/current/arch/i386_linux26/lib/R/library/limma/html/normalizeCyclicLoess.html'>cyclicLoess</a>. <br/></p>"),
                                                         type = "info", html = T
      ))
      
      observeEvent(input$h_summarize, sendSweetAlert(session,
                                                     title = "Summarization of feature values",
                                                     text = HTML("<p align='justify'><i>This option is only needed for id columns with duplicated values.</i>
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
                                                     type = "info", html = T
      ))
      
      return(list(
        next_tab = next_tab,
        processed_table = processed_table,
        result_table = result_table,
        pexp_design = pexp_design
      ))
      
    }
  )
}
