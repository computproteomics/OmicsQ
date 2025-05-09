## useful function
expd_dist <- function(cnames, ...) {
    out_m <- matrix(1,
                    ncol = length(cnames),
                    nrow = length(cnames), dimnames = list(
                        x = cnames,
                        y = cnames
                    )
    )
    for (i in cnames) {
        out_m[i, ] <- 1 - stringdist(i, cnames, ...)
    }
    1 - out_m
}

#################### UI ##################
expDesignUI <- function(id, prefix="") {
    ns <- NS(id)
    tagList(
        h3("Automatic selection of experimental groups"),
        fluidRow(column(10, 
                        p("Find most suitable settings. You  can manually edit the experimental design in the table below.
                      Replicates with equal number and of the same sample type will be summarized to one replicate.")),
                 column(2, 
                        actionBttn(ns("h_exp_design"),
                                   icon=icon("info-circle"),
                                   style="pill", 
                                   color = "royal", size = "xs"))),
        fluidRow(
            hidden(column(width=4, id=ns("ed_c1"),
                          
                          fluidRow(
                              column(10,
                                     sliderInput(ns("dist_thresh"), "Threshold for string distance", value=0, min=0, max=1)),
                              column(2, 
                                     actionBttn(ns("h_dist_thresh"),
                                                icon=icon("info-circle"),
                                                style="pill", 
                                                color = "royal", size = "xs"))),
                          column(10,
                                 selectInput(ns("dist_type"), "String distance type", 
                                             choices = c("Optimal string alignment"="osa",
                                                         "Levenshtein"="lv",
                                                         "Damerau-Levenshtein"="dl",
                                                         "Longest common substring"="lcs",
                                                         "q-gram"="qgram",
                                                         "cosine"="cosine",
                                                         "Jaccard"="jaccard",
                                                         "Jaro-Winkler"="jw",
                                                         "soundex"="soundex"), selected="")
                                 
                          ),
                          column(2, 
                                 actionBttn(ns("h_dist_type"),
                                            icon=icon("info-circle"),
                                            style="pill", 
                                            color = "royal", size = "xs")),
            style = 'border-left: 1px solid' # Adding a left border to align with other sections
            )),
            hidden(column(width=4, id=ns("ed_c2"),
                          h4("Assign Sample Types and Batch Number"),
                          fluidRow(
                              column(10,
                                     pickerInput(ns("ed_sel_samples"), "Select columns for setting sample type", 
                                                 choices=NULL,  multiple=T, 
                                                 options = list(
                                                     `live-search` = TRUE,
                                                     `actions-box` = TRUE))
                              ),
                              column(2, 
                                     actionBttn(ns("h_sel_samples"),
                                                icon=icon("info-circle"),
                                                style="pill", 
                                                color = "royal", size = "xs")),
                              
                              sliderInput(ns("ed_number"), "Set to this sample type", min=1, max=1, value=1, step=1)),
                          
                          
                          fluidRow(
                              column(10,
                                     pickerInput(ns("ed_sel_batches"), "Select columns for setting batch number", 
                                                 choices=NULL, multiple=T, 
                                                 options = list(
                                                     `live-search` = TRUE,
                                                     `actions-box` = TRUE))
                              ),
                              column(2, 
                                     actionBttn(ns("h_sel_batches"),
                                                icon=icon("info-circle"),
                                                style="pill", 
                                                color = "royal", size = "xs"))
                          ),
                          sliderInput(ns("batch_number"), "Set to this batch number", min=1, max=2, value=2, step=1),
                          style = 'border-left: 1px solid' # Adding a left border to align with other sections
            )),
            
            hidden(column(3,id=ns("ed_c3"),
                          h4("Proceed to data pre-processing"),
                          p("Note: Samples with equal group and replicate number will be merged."),
                          actionButton(ns("proceed_to_process"), "Proceed"),
                          style = 'border-left: 1px solid')
            )
        ),
        ### Show table for exp. design
        fluidRow(
            fluidRow(
                column(10, downloadBttn(ns("downloadeTable"), label = "Download table")),
                column(2,
                       actionBttn(ns("h_etable"), icon=icon("info-circle"), style="pill", color="royal", size="xs"))
            ),
            DTOutput(ns('etable')
            )
        )
    )
}


########### Server #########

expDesignServer <- function(id, parent, dataInput, log_operations) {
    moduleServer(
        id,
        function(input, output, session) {
            exp_design <- reactiveVal(NULL)
            pexp_design <- reactiveVal(NULL)
            next_tab <- reactiveVal(NULL)
            process_table <- reactiveVal(NULL)
            
            observeEvent(dataInput$next_tab(), {
                print(dataInput$next_tab())
                if (!is.null(dataInput$next_tab())) {
                    
                    exp_design(dataInput$exp_design())
                    cnames <- colnames(exp_design())
                    print("init ExpDesign")
                    
                    # Ensure exp_design has 3 rows with Batch row initialized to 1 if not present
                    current_design <- exp_design()
                    if (nrow(current_design) < 3) {
                        if (nrow(current_design) == 2) {
                            # Add a third row named "Batch" initialized with 1s
                            current_design <- rbind(current_design, Batch = rep(1, ncol(current_design)))
                            rownames(current_design)[3] <- "Batch"
                        } else if (nrow(current_design) == 1) {
                            # Add second row for Replicate and third row for Batch initialized with 1s
                            current_design <- rbind(current_design, Replicate = rep(1, ncol(current_design)), Batch = rep(1, ncol(current_design)))
                            rownames(current_design)[2:3] <- c("Replicate", "Batch")
                        }
                        exp_design(current_design)  # Update exp_design with the modified matrix
                    }
                    
                    updateSelectInput(session, "dist_type", selected = "jaccard")
                    updateSelectInput(session, "dist_thresh", selected = 0)
                    updateSelectInput(session, "dist_type", selected = "jw")
                    updatePickerInput(session, "ed_sel_samples", choices = cnames)
                    updateSliderInput(session, "ed_number", max = length(cnames))
                    updatePickerInput(session, "ed_sel_batches", choices = cnames)
                    updateSliderInput(session, "batch_number", max = length(cnames))
                    shinyjs::show("ed_c3")
                    shinyjs::show("ed_c2")
                    shinyjs::show("ed_c1")  # Show the combined sample type and batch assignment UI
                }
            })
            
            # Update threshold
            observe({
                input$dist_type
                isolate({
                    if (!is.null(exp_design())) {
                        print("dist_type")
                        expd_d <- expd_dist(colnames(exp_design()),
                                            method = input$dist_type,
                                            p = 0.2
                        ) # p=0.1 prioritizes the start of the strings
                        th_vals <- sort(unique(as.vector(expd_d)))
                        median_dist <- median(th_vals[th_vals != 0], na.rm = T)
                        updateSliderInput(session, "dist_thresh", value=median_dist,
                                          min=round(min(th_vals), digits=3), 
                                          max=round(max(th_vals), digits=3), 
                                          step = round(diff(range(th_vals)/100), digits=3)
                        )
                    }
                })
            })
            
            # Update experimental design based on threshold
            observe({
                input$dist_thresh
                isolate({
                    tdesign <- exp_design()
                    if (!is.null(tdesign)) {
                        print("dist_thres")
                        expd_d <- expd_dist(colnames(tdesign),
                                            method = input$dist_type,
                                            p = 0.2
                        ) # p=0.1 prioritizes the start of the strings
                        median_dist <- input$dist_thresh
                        groups <- cutree(hclust(as.dist(expd_d)), h = median_dist)
                        print(groups)
                        tdesign[1, ] <- groups
                        for (j in unique(groups)) {
                            tdesign[2, groups == j] <- 1:sum(groups == j)
                        }
                        
                        exp_design(tdesign)
                    }
                })
            })
            
            # Manually change sample types
            observeEvent(input$ed_sel_samples, {
                isolate({
                    ted <- exp_design()
                    ted[is.na(ted)] <- 0
                    
                    if (!is.null(ted) && length(input$ed_sel_samples) > 0) {
                        print("Updating sample types")
                        ted[1, input$ed_sel_samples] <- input$ed_number
                        idx <- (ted[1, ] == input$ed_number)
                        idx <- idx[!is.na(idx)]
                        if (any(idx)) {
                            ted[2, idx] <- 1:sum(idx)
                        }
                        exp_design(ted)
                    } else {
                        print("No samples selected or invalid selection for sample types.")
                    }
                })
            })
            
            # Update the experimental design based on the selected batch number
            observeEvent(input$ed_sel_batches, {
                isolate({
                    ted <- exp_design()
                    
                    if (!is.null(ted) && length(input$ed_sel_batches) > 0) {
                        print("Updating batch numbers")
                        
                        if (nrow(ted) < 3) {
                            ted <- rbind(ted, Batch = rep(1, ncol(ted)))
                            rownames(ted)[3] <- "Batch"
                            print("Added third row named 'Batch' to 'ted' matrix for batch numbers.")
                        }
                        
                        print("Structure of ted after adding 'Batch' row:")
                        print(ted)
                        
                        valid_batches <- input$ed_sel_batches[input$ed_sel_batches %in% colnames(ted)]
                        
                        if (length(valid_batches) > 0) {
                            tryCatch({
                                ted["Batch", valid_batches] <- input$batch_number
                                exp_design(ted)
                                print("Batch number assignment successful.")
                                print(ted)
                            }, error = function(e) {
                                print(paste("Error during batch number assignment:", e$message))
                            })
                        } else {
                            print("No valid columns selected for batch number assignment.")
                        }
                    } else {
                        print("Invalid operation: ExpDesign matrix does not have enough rows or invalid batch number selection.")
                    }
                })
            })
            
            # Table for editing design
            output$etable <- DT::renderDT({
                if (!is.null(exp_design())) {
                    isolate({
                        print("edtable")
                        show_table <- t(exp_design())
                        show_table[is.na(show_table)] <- 1
                        print(input$etable_rows_current)
                        if (!is.null(input$etable_rows_current)) {
                            rows <- length(input$etable_rows_current)
                        } else {
                            rows <- 10
                        }
                        datatable(show_table, editable = T, options = list(pageLength = rows)) %>%
                            formatStyle("Group",
                                        target = "row", backgroundColor =
                                            styleEqual(unique(show_table[
                                                ,
                                                "Group"
                                            ]), viridis(length(unique(show_table[, "Group"])), alpha = 0.7))
                            )
                    })
                }
            })
            
            observeEvent(input$etable_cell_edit, {
                tdata <- t(exp_design())
                tdata[input$etable_cell_edit$row, input$etable_cell_edit$col] <-
                    input$etable_cell_edit$value
                exp_design(t(tdata))
            })
            
            output$downloadeTable <- downloadHandler(filename = function() {
                validate(need(NULL, "No data"))
                paste("ExpDesign", Sys.Date(), ".csv", sep = "")
            }, content = function(file) {
                write.csv(exp_design(), file)
            })
            
            # Send further to next tab
            observeEvent(input$proceed_to_process, {
                print("send to processing")
                final_exp_design <- exp_design()
                exp_design(final_exp_design[, order(
                    final_exp_design[1, ],
                    final_exp_design[2, ]
                )])
                pexp_design(exp_design())
                tdata <- dataInput$indata()
                
                icol <- colnames(tdata)[grep("id", sapply(tdata, class))]
                ccols <- colnames(tdata)[grep("quant", sapply(tdata, class))]
                ocols <- colnames(tdata)[which(!(colnames(tdata) %in% c(icol, ccols)))]
                process_table(tdata[, c(icol, colnames(final_exp_design), ocols)])
                
                updateTabsetPanel(parent, "mainpage", selected = "process")
                
                if (!is.null(next_tab())) {
                    next_tab(paste0(next_tab(), "_new"))
                } else {
                    next_tab("ready")
                }
            })
            
            ############### Help messages
            observeEvent(input$h_proceed_expdesign, 
                         sendSweetAlert(session,
                                        title = "Ready to proceed?",
                                        text = HTML("<p align='justify'>In order to change to define the
    experimental design, you need to have selected
            an <i>ID</i> column and multiple numeric <i>quant</i> columns.</p>"),
                                        type = "info", html = T
                         ))
            
            observeEvent(input$h_exp_design, 
                         sendSweetAlert(session,
                                        title = "Estimate design",
                                        text = HTML("<p align='justify'><i>General: </i>We estimate the
    experimental design from the similarity
              between column names. Try the different string distances
              below and play with the threshold
              to find the optimal setting. <br/>
              The sample
              type is the group of samples that are
              of the same type, e.g. drug, disease or
              time points. The sample type is used to
              group the samples in the next step. <br/>
              You can further annotate different batches of samples groups.
              "),
                                        type = "info", html = T
                         ))
            
            observeEvent(input$h_dist_thresh, 
                         sendSweetAlert(session,
                                        title = "Threshold for string distance",
                                        text = HTML("<p align='justify'>The threshold for the string distance
    is used to adjust the similarity between
              column names. The threshold is used to
              cluster the columns into groups. The
              threshold is initialized by the median of the
              distances between all column names.</p>"),
                                        type = "info", html = T
                         ))
            
            
            observeEvent(input$h_dist_type, 
                         sendSweetAlert(session,
                                        title = "String distance type",
                                        text = HTML("<p align='justify'>The string distance type is used for different
                                        similarity measures between column names. Change the type to 
                                        see whether you get an automatic separation into the correctd sample groups.
              </p>"),
                                        type = "info", html = T
                         ))
            
            observeEvent(input$h_sel_samples,
                         sendSweetAlert(session,
                                        title = "Select columns for setting the sample type",
                                        text = HTML("<p align='justify'>Select the columns that should become the sample type
                                        given by the ruler below. You can use
                                        this to adjust the sample type after the automatic estimation </p>"),
                                        type = "info", html = T
                         ))
            
            observeEvent(input$h_sel_batches,
                         sendSweetAlert(session,
                                        title = "Select columns for setting batch number",
                                        text = HTML("<p align='justify'>Select the columns that should be
    used to set the batch number given by the number in the ruler below. The batch
              number is used to group the samples
              into different batches. </p>"),
                                        type = "info", html = T
                         ))
            
            observeEvent(input$h_etable,
                         sendSweetAlert(session,
                                        title = "Edit the experimental design",
                                        text = HTML("<p align='justify'>
                                        <b>Explanation of table: </b></br/>
                                        <i>Group</i> denotes the experimental
              condition, i.e. the group of sample
              of the same type like drug, disease or time points. <br/>
              <i>Replicate</i> denotes the replicate number starting with one and strictly incrementing.<br/>
              <i>Batch</i> denotes the batch number of the samples, corresponding for example to different times or 
              instruments where the samples were measured. <br/>
              Different conditions, replicates and batches are all given by different numbers.
              Replicates correspond to biological or technical samples
              of the same type such as different mice with the
              same mutation or the same sample being rerun on the instrument.
              Here, a different replicate needs to be
              given by a different number (within all replicates of a condition).
              Replicates with the same number within
              the same conditions will be summarized in the next step.
              Summarization means the values of the resulting
              replicate will be given by the sum of the summarized replicates.</p>"),
                                        type = "info", html = T
                         ))
            
            return(list(
                next_tab = next_tab,
                pexp_design = pexp_design,
                process_table = process_table
            ))
        }
    )
}


