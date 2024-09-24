#################### UI ##################
preProcessingUI <- function(id, prefix="") {
    ns <- NS(id)
    tagList(
        useShinyjs(),  # Include shinyjs to enable/disable UI elements
        fluidRow(
            hidden(column(width = 4, id = ns("pr_c1"),
                          h4("Add or delete data columns"),
                          fluidRow(column(10, pickerInput(ns("remove_reps"), "Pick the samples you want to remove", 
                                                          choices = NULL, multiple = T,
                                                          options = list(
                                                              `live-search` = TRUE,
                                                              `actions-box` = FALSE,
                                                              `max-options` = -2, # Prevent selecting more than n-2 columns
                                                              `max-options-text` = "Cannot select more than n-2 columns"
                                                          ))),
                                   column(2, actionBttn(ns("h_balancing"),
                                                        icon = icon("info-circle"),
                                                        style = "pill", 
                                                        color = "royal", size = "xs")
                                   )),
                          h4("The current state:"),
                          p(textOutput(ns("res_num_reps")), style = "text-color:#AA2222"),
                          checkboxInput(ns("add_na_columns"), "Fill with empty columns"),
            )),
            hidden(column(width = 3, id = ns("pr_c2"),
                          h4("Data manipulation and adjustment"),
                          fluidRow(column(10, checkboxInput(ns("logtrafo"), "Is the data already log-transformed?", value = F)),
                                   column(2, actionBttn(ns("h_logtrafo"),
                                                        icon = icon("info-circle"),
                                                        style = "pill", 
                                                        color = "royal", size = "xs")
                                   )),
                          fluidRow(column(10, numericInput(ns("max_na"), label = "Maximum number of missing values per feature", 
                                                           min = 0, max = 0, step = 1, value = 100)),
                                   column(2, actionBttn(ns("h_max_na"),
                                                        icon = icon("info-circle"),
                                                        style = "pill", 
                                                        color = "royal", size = "xs")
                                   )),
                          fluidRow(column(10, selectInput(ns("normalization"), label = "Normalization method",
                                                          choices = c(None = "none", Median = "colMedians", "Mean" = "colMeans", "Cyclic LOESS (LIMMA)" = "cyclicloess"),
                                                          selected = "none")),
                                   column(2, actionBttn(ns("h_normalization"),
                                                        icon = icon("info-circle"),
                                                        style = "pill", 
                                                        color = "royal", size = "xs")
                                   )),
                          fluidRow(column(10, selectInput(ns("summarize"), label = "Summarize to id features", 
                                                          choices = c(None = "none", "By sum" = "colSums",
                                                                      "By mean" = "colMeans", "By median" = "colMedians", 
                                                                      "Robust median (medpolish)" = "medianPolish"
                                                          ))),
                                   column(2, actionBttn(ns("h_summarize"),
                                                        icon = icon("info-circle"),
                                                        style = "pill", 
                                                        color = "royal", size = "xs")
                                   )),
                          
                          fluidRow(column(10, 
                                          selectInput(ns("batch_correction_method"), "Correction method", 
                                                      choices = c("None", "limma", "Combat"), selected = "none") # New input for batch correction method
                          ),
                          column(2, actionBttn(ns("h_batch_effect"),
                                               icon = icon("info-circle"),
                                               style = "pill", 
                                               color = "royal", size = "xs")
                          )),
                          
                          
                          style = 'border-left: 1px solid' 
                          
            ),
            hidden(column(4, id = ns("pr_c3"),
                          h4("Summary:"),
                          htmlOutput(ns("ptable_summary"), style = "border:solid;border-width:1px;"),
                          h5("Proceed to interaction with apps"),
                          textOutput(ns("txt_proceed_apps")),
                          actionButton(ns("proceed_to_apps"), "Proceed"),
                          style = 'border-left: 1px solid'    
            ))),        
            
            hidden(fluidRow(id = ns("pr_plots"),
                            column(6, 
                                   plotOutput(ns("pca_combined"), height = "400px")), # Combined PCA Plot
                            column(6, 
                                   plotOutput(ns("corrplot"), height = "400px") # Keep existing correlation plot
                            )
            ))
            
        )
    )
}





#################### Server ##################
preProcessingServer <- function(id, parent, expDesign, log_operations) {
    moduleServer(
        id,
        function(input, output, session) {
            
            process_table <- reactiveVal(NULL)
            processed_table <- reactiveVal(NULL)
            other_cols <- reactiveVal(NULL)
            pexp_design <- reactiveVal(NULL)
            exp_design <- reactiveVal(NULL)
            next_tab <- reactiveVal(NULL)
            
            # Reactive values to store group, batch and replicate information
            group_info <- reactiveVal(NULL)
            batch_info <- reactiveVal(NULL)
            replicate_info <- reactiveVal(NULL)
            
            # Reactive values to added columns
            added_columns <- reactiveVal(c())
            
            init_data <- function() {
                # Initialize processed_table with 'id' and 'quant' columns from process_table
                initial_data <- process_table()
                id_column <- initial_data[, grep("id", sapply(initial_data, class)), drop = FALSE]
                quant_columns <- initial_data[, grep("quant", sapply(initial_data, class)), drop = FALSE]
                processed_table(cbind(id_column, quant_columns)) # Initialize processed_table
                # Keep all other columns separate, only to merge when summarizing
                # This is why we need to reload them
                other_cols(initial_data[, !(colnames(initial_data) %in% c(colnames(id_column), colnames(quant_columns)))])
                
            }
            
            ##########################################################################
            ##########################################################################
            observeEvent(expDesign$next_tab(), {
                if (!is.null(expDesign$next_tab())) {
                    process_table(expDesign$process_table())
                    pexp_design(expDesign$pexp_design())
                    exp_design(expDesign$pexp_design())
                    
                    init_data()
                    
                    shinyjs::show("pr_c1")
                    shinyjs::show("pr_c2")
                    shinyjs::show("pr_c3")
                    shinyjs::show("pr_plots")
                    
                    id_column <- processed_table()[, 1]
                    quant_columns <- processed_table()[, -1]
                    
                    # Check if there are duplicate IDs and hide/show the summarize input accordingly
                    print(id_column)
                    if (sum(duplicated(id_column)) > 0) {
                        shinyjs::show("summarize")  # Show the 'Summarize to id features' selector if duplicates are present
                    } else {
                        shinyjs::disable("summarize")  # Hide the 'Summarize to id features' selector if no duplicates
                        #shinyjs::disable("h_summarize") 
                    }
                    
                    
                    # Update pickerInput with new column names and set max-options dynamically
                    total_columns <- ncol(expDesign$pexp_design())
                    updatePickerInput(session, "remove_reps", 
                                      choices = colnames(exp_design()),
                                      options = list(
                                          `max-options` = total_columns - 2, # Set max-options dynamically
                                          `max-options-text` = "Cannot select more than n-2 columns"
                                      ))
                    
                    # Initialize group, batch and replicate information
                    group_info(pexp_design()[1,]) # Assuming Group info is in the first row
                    replicate_info(pexp_design()[2,]) # Adjust index based on data structure
                    batch_info(pexp_design()[3,]) # Adjust index based on data structure
                    
                    # Check for different batches and batch sample number >= 2
                    if (length(unique(batch_info())) > 1 && min(table(batch_info()) >= 2)) {
                        shinyjs::enable("batch_correction_method")
                        updatePickerInput(session, "batch_correction_method", 
                                          selected = "None")
                    } else {
                        shinyjs::disable("batch_correction_method")
                    }
                    
                    # Determine if the data has been log-transformed
                    tlog <- log_operations()
                    if (max(quant_columns, na.rm = T) / min(quant_columns, na.rm = T) < 100 || min(quant_columns, na.rm = T) < 0) {
                        updateCheckboxInput(session, "logtrafo", value = TRUE)
                        tlog[["preprocess_take_log2"]] <- FALSE
                    } else {
                        tlog[["preprocess_take_log2"]] <- TRUE
                    }
                    updateNumericInput(session, "max_na",
                                       min = 0, max = ncol(quant_columns),
                                       value = ncol(quant_columns)
                    )
                    log_operations(tlog)
                }
            })
            
            
            ##########################################################################
            ##########################################################################
            # Add or delete data columns
            remove_cols <- function(tdata, remove_reps, expdesign) {
                print("Removing columns...")
                withProgress(message = "Processing...", value = 0, min = 0, max = 1, {
                    cnames <- colnames(expdesign)
                    
                    # Removing selected columns
                    rem <- -which(names(tdata) %in% remove_reps)
                    if (length(rem) > 0) {
                        incProgress(0.1, detail = "Removing replicates")
                        tdata <- tdata[, rem]
                        rem2 <- -which(cnames %in% remove_reps)
                        pexp_design(expdesign[, rem2])
                        cnames <- cnames[rem2]
                        tlog <- log_operations()
                        tlog[["preprocess_removed_replicates"]] <- input$remove_reps
                        log_operations(tlog)
                    } else {
                        pexp_design(exp_design())
                    }
                    
                    # Update processed_table to keep only 'id' and 'quant' columns
                    id_column <- tdata[, grep("id", sapply(tdata, class)), drop = FALSE]
                    quant_columns <- tdata[, grep("quant", sapply(tdata, class)), drop = FALSE]
                    processed_table(cbind(id_column, quant_columns))
                })
            }
            
            
            
            ##########################################################################
            ##########################################################################
            ## Check for balanced exp. design
            check_balance <- function(expdesign) {
                print("check for balancing")
                #print(expdesign)
                
                # Calculate the number of replicates per experimental condition
                ed_stats <- as.vector(table(expdesign[1, ]))
                
                # Check if all experimental conditions have the same number of replicates
                if (length(unique(ed_stats)) > 1) {
                    # If the design is unbalanced, disable the Proceed button
                    disable("proceed_to_apps")
                    enable("add_na_columns")  # Enable fill with empty columns when unbalanced
                    tout <- paste(
                        "This unbalanced design has between ",
                        min(ed_stats),
                        " and maximally", max(ed_stats),
                        "replicates for each experimental condition (sample type). Please click below button to make data balanced."
                    )
                } else {
                    # If the design is balanced, enable the Proceed button
                    enable("proceed_to_apps")
                    disable("add_na_columns")  # Disable fill with empty columns when balanced
                    tout <- paste("Experimental design is balanced.")
                }
                
                output$res_num_reps <- renderText(tout)
            }
            
            
            
            ##########################################################################
            ##########################################################################
            # "Fill with empty columns" button
            add_na_columns <- function(tdata, expdesign) {
                print("Adding NA columns...")
                
                reps <- table(expdesign[1, ])
                max_reps <- max(reps)
                tedes <- expdesign
                
                added_columns <- c()  # Track added columns for the summary
                
                # Add the new columns
                for (cond in unique(tedes[1, ])) {
                    tt <- tedes[, tedes[1, ] == cond, drop = F]
                    
                    for (i in seq_len(max_reps - reps[cond])) {
                        # Get the number of rows in tedes
                        n_rows <- nrow(tedes)
                        
                        # Create a new column that matches the number of rows in tedes
                        new_col <- c(rep(cond, n_rows - 1), max(tt[2, ]) + i)
                        
                        # Name the new column as new_oldname_numbering
                        oldname <- colnames(tt)[1]  # Assuming the first column is the old name
                        new_col_name <- paste0("new_", oldname, "_", i)  # Add numbering for each new column
                        
                        # Add the new column to tedes
                        tedes <- cbind(tedes, new_col)
                        
                        # Rename the new column in tedes
                        colnames(tedes)[ncol(tedes)] <- new_col_name
                        
                        # Add an NA column to tdata (the actual data)
                        tdata[new_col_name] <- NA
                        
                        # Track the added column name for the summary
                        added_columns <- c(added_columns, new_col_name)
                    }
                }
                
                # Store the added columns in the reactive value
                added_columns(added_columns)
                
                # log
                tlog <- log_operations()
                tlog[["added_columns"]] <- added_columns
                log_operations(tlog)
                
                
                # Reorder columns according to experimental design
                tedes <- tedes[, order(tedes[1, ], tedes[2, ])]
                pexp_design(tedes)
                
                # Update the processed tables
                id_column <- tdata[, grep("id", sapply(tdata, class)), drop = FALSE]
                quant_columns <- tdata[, grep("quant", sapply(tdata, class)), drop = FALSE]
                
                # Update the processed table
                processed_table(cbind(id_column, quant_columns))  # Make sure new columns are added here
                
            }
            ##########################################################################
            ##########################################################################
            # Apply maximum number of missing values per feature
            filter_nas <- function(tdata,  max_na) {
                print("Filtering NAs...")
                if (!is.null(max_na)) {
                    to_delete <- rowSums(is.na(tdata[, -1])) <= max_na
                    tdata <- tdata[to_delete, ]
                    oc <- other_cols()
                    if (!is.null(oc)) {
                        oc <- oc[to_delete, ]
                    }
                    other_cols(oc)
                    tlog <- log_operations()
                    tlog[["max_na"]] <- max_na
                    log_operations(tlog)
                    
                }
                processed_table(tdata)
            }
            
            ##########################################################################
            ##########################################################################
            # Log transformation
            log_transformation <- function(tdata, logtrafo) {
                print("Log transformation...")
                if (!logtrafo) {
                    ttt <- as.matrix(tdata[, -1])
                    ttt <- log2(ttt)
                    ttt[!is.finite(ttt)] <- NA
                    tdata[, -1] <- ttt
                }
                processed_table(tdata)
            }
            
            ##########################################################################
            ##########################################################################
            # Normalization
            normalize_data <- function(tdata, method) {
                print("Normalizing data...")
                # Apply normalization method
                if (method != "none") {
                    if (method == "colMedians") {
                        tdata[, -1] <- t(t(tdata[, -1]) - colMedians(as.matrix(tdata[, -1]), na.rm = TRUE))
                    } else if (method == "colMeans") {
                        tdata[, -1] <- t(t(tdata[, -1]) - colMeans(as.matrix(tdata[, -1]), na.rm = TRUE))
                    } else if (method == "cyclicloess") {
                        tdata[, -1] <- limma::normalizeBetweenArrays(as.matrix(tdata[, -1]), method = "cyclicloess")
                    }
                    tlog <- log_operations()
                    tlog[["normalization"]] <- method
                    log_operations(tlog)
                    
                    processed_table(tdata)
                }
            }
            
            
            ##########################################################################
            ##########################################################################
            # Summarization
            summarize_data <- function(tdata, method) {
                if (sum(duplicated(tdata[,1])) > 0) {
                    print("Summarizing data...")
                    
                    # update other_cols by merging values using bars
                    o_cols <- other_cols()
                    if (!is.null(o_cols)) {
                        to_rbind <- by(o_cols, tdata[, 1], function(x) {
                            apply(x, 2, function(y) paste(as.character(y), collapse="|"))
                        })
                        o_cols <- do.call(rbind, to_rbind)
                    }
                    
                    # Apply summarization method
                    if (method != "none") {
                        if (method == "colSums") {
                            tdata <- aggregate(. ~ tdata[, 1], data = tdata[, -1], FUN = sum, na.rm = TRUE)
                        } else if (method == "colMeans") {
                            tdata <- aggregate(. ~ tdata[, 1], data = tdata[, -1], FUN = mean, na.rm = TRUE)
                        } else if (method == "colMedians") {
                            tdata <- aggregate(. ~ tdata[, 1], data = tdata[, -1], FUN = median, na.rm = TRUE)
                        } else if (method == "medianPolish") {
                            tdata <- MsCoreUtils::aggregate_by_vector(as.matrix(tdata[, -1]), tdata[, 1], FUN = medianPolish, na.rm = TRUE)
                            tdata <- data.frame(ids=rownames(tdata), tdata)
                        }
                        tlog <- log_operations()
                        tlog[["summarization"]] <- method
                        log_operations(tlog)
                    }
                    
                    if (!is.null(o_cols))
                        other_cols(o_cols[as.character(tdata[, 1]), ])

                    # Update processed table after adjustments
                    processed_table(tdata)
                }
            }
            
            ##########################################################################
            ##########################################################################
            # Batch Effect Detection
            check_batch_effect <- function(tdata, expdesign) {
                print("Checking for batch effects...")
                # Ensure that there is enough data
                if (is.null(tdata) || ncol(tdata) < 2 || nrow(tdata) < 10) {
                    return()
                }
                
                # Store total number of features before processing
                total_features <- nrow(tdata)
                
                # Prepare the data
                texp_design <- expdesign
                
                # Select only quantitative columns and remove columns with all NAs
                tdata <- tdata[, -1, drop = FALSE]
                # Filter NA: we go full in to avoid strange effect from potential
                # imputation in the method
                tdata <- tdata[, colSums(!is.na(tdata)) > 0, drop=F]
                tdata <- tdata[complete.cases(tdata), , drop=F]
                
                texp_design <- texp_design[, colnames(tdata), drop = FALSE]
                
                # # Store number of features after removing NAs
                # features_without_na <- nrow(tdata[complete.cases(tdata), ])
                
                # # Filter out rows with missing values
                # tdata <- tdata[complete.cases(tdata), ]
                
                # Get batch labels
                batch_labels <- batch_info()
                
                # Ensure batch_labels has at least two levels
                if (length(unique(batch_labels)) < 2) {
                    return()
                }
                
                # Align `batch_labels` to match the data columns
                batch_labels <- batch_labels[match(colnames(tdata), colnames(texp_design))]
                
                # Filter rows for having at least one value per batch
                for (b in unique(batch_labels)) {
                    tdata <- tdata[rowSums(tdata[, batch_labels == b, drop=F]) > 0, , drop=F]
                }
                
                # Create a samples data frame required for BEclear
                sample_ids <- colnames(tdata)  # Assuming column names of tdata are the sample IDs
                samples <- data.frame(sample_id = sample_ids, batch_id = batch_labels)
                
                
                # Use BEclear to calculate batch effects
                # batch_effect_results <- tryCatch({
                batch_effect_results <-  BEclear::calcBatchEffects(
                    data = tdata, 
                    samples = samples,
                    adjusted = TRUE, 
                    method = "fdr"
                )
                
                if (is.null(batch_effect_results)) return() # Exit if error occurred
                
                # Extract median differences and p-values from the results
                mdifs <- batch_effect_results$med
                pvals <- batch_effect_results$pval
                summary <- calcSummary(medians = mdifs, pvalues = pvals, pvaluesTreshold = 0.01, mediansTreshold = 0.5)
                return(list(summary=summary, outdata=tdata))
            }
            
            
            #########################################################################
            #################### Batch Effect Correction ###########################
            batch_correction <- function(tdata, expdesign, batch_labels, method) {
                print("Correcting batch effects...")
                
                withProgress(message = 'Running batch correction', value = 0, {
                    # Align `batch_labels` to match the data columns
                    batch_labels <- batch_info()
                    batch_labels <- batch_labels[colnames(expdesign) %in% colnames(tdata)]
                    
                    id_col <- NULL
                    
                    # Determine the number of features with significant batch effects (p < 0.01)
                    if (method == "None") {
                        return(NULL)
                    } 
                    # Filter rows for having at least one value per batch
                    rownames(tdata) <- paste("r", 1:nrow(tdata))
                    red_for_correction <- tdata
                    for (b in unique(batch_labels)) {
                        red_for_correction <- red_for_correction[rowSums(!is.na(red_for_correction[, names(batch_labels[batch_labels == b]), drop=F])) > 1, , drop=F]
                    }
                    if (nrow(red_for_correction) < 10) {
                        sendSweetAlert(session,
                                       title = "Batch Effect Correction Error",
                                       text = "Not enough data after filtering for missing values to run batch effect correction.",
                                       type = "error")
                        return(NULL)
                    }
                    
                    id_col <- tdata[, 1]
                    if (method == "limma") {
                        
                        # Use limma to calculate and correct batch effects
                        batch_effect_corrected <- tryCatch({
                            removeBatchEffect(red_for_correction[, -1], batch = as.factor(batch_labels))
                        }, error = function(e) {
                            sendSweetAlert(session,
                                           title = "Batch Effect Correction Error",
                                           text = paste("An error occurred during batch effect correction with limma:", e$message),
                                           type = "error")
                            return(NULL)
                        })
                    } else if (method == "Combat") {
                        
                        # Use Combat to correct batch effects
                        batch_effect_corrected <- tryCatch({
                            sva::ComBat(dat = red_for_correction[, -1], batch = as.factor(batch_labels), mod = NULL, par.prior = TRUE, prior.plots = FALSE)
                        }, error = function(e) {
                            sendSweetAlert(session,
                                           title = "Batch Effect Correction Error",
                                           text = paste("An error occurred during batch effect correction with Combat:", e$message),
                                           type = "error")
                            return(NULL)
                        })
                    } else {
                        sendSweetAlert(session,
                                       title = "Batch Effect Correction Error",
                                       text = "Unknown batch correction method selected.",
                                       type = "error")
                        return()
                    }
                    
                    if (is.null(batch_effect_corrected)) return() # Exit if error occurred
                    
                    tlog <- log_operations()
                    tlog[["batch_correction"]] <- method
                    log_operations(tlog)
                    
                    # Increment progress to 90%
                    incProgress(0.9, detail = "Updating processed table with corrected data")
                    
                    # # Get the ID column (which is non-numeric column in processed_table)
                    # origdata <- processed_table()
                    # # Get data after batch correction
                    # origdata[, colnames(batch_effect_corrected)] <- batch_effect_corrected
                    
                    # write data back
                    tdata[rownames(batch_effect_corrected), -1] <- batch_effect_corrected
                    processed_table(tdata)
                    
                    # Increment progress to 100%
                    incProgress(1, detail = "Batch correction completed")
                    
                })
            }
            
            
            ## Delay reaction to selecting rows in data table
            triggerUpdate <- debounce(reactive(list( input$normalization,
                                                     input$summarize,
                                                     input$max_na,
                                                     input$add_na_columns,
                                                     input$remove_reps,
                                                     input$logtrafo,
                                                     input$batch_correction_method)
                                               
            ),1000)
            
            
            
            ##########################################################################
            ##########################################################################
            # Wrapper for data manipulation and adjustment
            
            # Separate observer for normalization to avoid multiple reactivity loops
            observe({
                triggerUpdate()
                
                isolate({
                    init_data()
                    
                    tdata <- processed_table()
                    
                    if (is.null(tdata)) return() # Exit if tdata is NULL
                    
                    remove_cols(tdata, input$remove_reps, exp_design())
                    
                    if (input$add_na_columns)
                        add_na_columns(processed_table(), pexp_design())
                    
                    check_balance(pexp_design())
                    
                    filter_nas(processed_table(), input$max_na)
                    
                    log_transformation(processed_table(), input$logtrafo)
                    
                    normalize_data(processed_table(), input$normalization)
                    
                    summarize_data(processed_table(), input$summarize)
                    
                    # set classes of processed_table to id and quant
                    id_column <- processed_table()[,1, drop = FALSE]
                    class(id_column[,1]) <- "id"
                    quant_columns <- processed_table()[, -1]
                    for (i in 1:ncol(quant_columns)) {
                        class(quant_columns[,i]) <- "quant"
                    }
                    processed_table(cbind(id_column, quant_columns))
                    
                    batch_correction(processed_table(), pexp_design(), batch_info(), input$batch_correction_method)
                    
                })
                
            })
            
            
            ##########################################################################
            ##########################################################################
            # "Summary:"
            output$ptable_summary <- renderText({
                print("Generating summary...")
                
                # Get the processed table, which reflects any changes made in data treatment
                tdata <- processed_table()
                isolate({
                    
                    # Ensure processed_table is available and valid
                    if (is.null(tdata) || ncol(tdata) < 2 || nrow(tdata) < 10) { 
                        shiny::validate(
                            need(FALSE, "Data matrix too small for generating summary")
                        )
                        return()
                    }
                    
                    # Identify "id" and "quant" columns
                    id_column <- tdata[,1]
                    quant_columns <- tdata[, -1, drop = FALSE]
                    
                    
                    # Ensure there is still enough data after filtering quantitative columns
                    if (ncol(quant_columns) < 2 || nrow(quant_columns) < 10) {
                        shiny::validate(
                            need(FALSE, "Not enough quantitative columns or rows for summary generation")
                        )
                        return()
                    }
                    
                    added_columns <- added_columns()  # Store the result in a variable
                    
                    # check for batch effect
                    tout <- check_batch_effect(processed_table(), pexp_design())
                    summary <- tout$summary
                    
                    # Determine the number of features with significant batch effects (p < 0.01)
                    significant_batches <-  ifelse(is.null(summary), 0, nrow(summary)) 
                    
                    
                    # Prepare the summary text
                    paste(
                        "<p><b>Before Filling:</b>
          <br/><b>Size:</b> The current data table contains ", 
                        ncol(quant_columns), " samples and ", nrow(quant_columns), " features.
          <br/><b>Missingness: </b>The proportion of missing values is ",
                        round(sum(is.na(quant_columns)) / length(as.matrix(quant_columns)), 3), 
                        ", with the number of missing values ranging from ", min(colSums(is.na(quant_columns))), 
                        " to ", max(colSums(is.na(quant_columns))), " per sample.
          <br/><b>Range:</b> The dynamic range is from ",
                        round(min(quant_columns, na.rm = TRUE), 2), " to ", round(max(quant_columns, na.rm = TRUE), 2), 
                        ".<br/><b>Summarization:</b> The ID column contains ", 
                        ifelse(sum(duplicated(id_column)) > 0, "non-unique IDs, and thus needs summarization.", "unique IDs, so summarization is not required."), 
                        "<br/><br/><b>Batch Effect:</b> ", ifelse(
                            length(unique(batch_info())) > 1, 
                            ifelse(min(table(batch_info())) >= 2, 
                                   ifelse(
                                       significant_batches > 0, 
                                       paste("There are currently", significant_batches, " features potentially affected by batch effects (p < 0.01)."), 
                                       "No significant batch effects detected."),
                                   "There need to be at least 2 samples in each batch for running batch effect correction."),
                            "Only one batch detected, no batch effect correction needed."),
                        "<br/>Used batch effect correction method: ", input$batch_correction_method, ".",
                        "<br/><br/><b>After Filling:</b>", 
                        ifelse(length(added_columns) == 0, " No columns added", paste(" New empty columns added: ", paste(added_columns(), collapse = ", "))),  "</p>"
                    )
                })
            })
            
            
            ##########################################################################
            ##########################################################################
            ## Send further to next tab
            observeEvent(input$proceed_to_apps, {
                # Ensure the processed_table is up to date
                tdata <- processed_table()  # Fetch the current processed_table data
                if (!is.null(tdata)) {
                    processed_table(tdata)  # Reassign the same data to ensure it is up-to-date
                    print("processed_table has been updated before proceeding.")
                }
                
                # Proceed to the next tab
                updateTabsetPanel(parent, "mainpage", selected = "apps")
                next_tab("ready")
                
                print("Proceeding to interaction with apps, processed_table has been updated.")
            })
            
            
            
            
            
            ##########################################################################
            ##########################################################################
            ### PCA Plot
            output$pca_combined <- renderPlot({
                print("PCA Plot")
                
                # Get the processed table, which reflects any changes made in data treatment
                tdata <- processed_table()
                
                # Check if processed_table is available and valid
                if (is.null(tdata) || ncol(tdata) < 3 || nrow(tdata) < 10) { 
                    shiny::validate(
                        need(FALSE, "Data matrix too small to perform PCA after batch correction")
                    )
                    return()
                }
                
                # Convert the processed_table to the format before batch correction
                # Identify "id" and "quant" columns
                id_column <- tdata[, 1]
                quant_columns <- tdata[,-1, drop = FALSE]
                
                
                # Ensure there is still enough data after filtering quantitative columns
                if (ncol(quant_columns) < 2) {
                    shiny::validate(
                        need(FALSE, "Not enough quantitative columns for PCA calculation")
                    )
                    return()
                }
                
                # Remove columns with all NAs
                quant_columns <- quant_columns[, colSums(!is.na(quant_columns)) > 0, drop = FALSE]
                
                # Remove constant columns to avoid PCA errors
                constant_columns <- apply(quant_columns, 2, function(col) var(col, na.rm = TRUE) == 0)
                quant_columns <- quant_columns[, !constant_columns]
                
                # Remove rows with missing or infinite values
                quant_columns <- quant_columns[complete.cases(quant_columns),]
                
                # Check if there is still enough data for PCA
                shiny::validate(
                    need(ncol(quant_columns) > 2, "Not enough columns with variance for PCA calculation"),
                    need(nrow(quant_columns) > 20, "Not enough samples for PCA calculation")
                )
                
                # Perform PCA
                pca <- prcomp(t(quant_columns), scale = TRUE, retx = TRUE)
                loadings <- pca$x
                
                # Get the current experimental design to align replicate and batch information
                texp_design <- pexp_design()
                
                # Ensure replicate_info() and batch_info() match the data after processing
                # Find the common indices based on column names that are retained after processing
                valid_indices <- match(colnames(quant_columns), colnames(texp_design))
                replicate_info_filtered <- replicate_info()[valid_indices]
                batch_info_filtered <- batch_info()[valid_indices]
                group_info_filtered <- group_info()[valid_indices]
                
                # Check if the valid_indices has NAs which mean the alignment was not successful
                if (any(is.na(valid_indices))) {
                    shiny::validate(
                        need(FALSE, "Mismatch between data and experimental design. Ensure alignment of data columns.")
                    )
                    return()
                }
                
                # Create data frame for plotting
                pca_df <- data.frame(
                    PC1 = loadings[, 1], 
                    PC2 = loadings[, 2], 
                    Group = as.factor(group_info_filtered),
                    Replicate = as.factor(replicate_info_filtered), 
                    Batch = as.factor(batch_info_filtered)
                )
                
                # Use ggplot2 for combined PCA plot
                library(ggplot2)
                ggplot(pca_df, aes(x = PC1, y = PC2, color = Group, shape = Batch)) +
                    geom_point(size = 3) +
                    labs(title = paste0("PCA Plot"),
                         x = "PC1", y = "PC2") +
                    theme_minimal() +
                    theme(legend.position = "right",
                          plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Set title font size to 14
                    )
            })
            
            
            
            ##########################################################################
            ##########################################################################
            ##### Correlation Plot
            output$corrplot <- renderPlot({
                print("corrplot")
                tdata <- processed_table()
                
                # Check if processed_table is available and valid
                if (is.null(tdata) || ncol(tdata) < 3 || nrow(tdata) < 10) { 
                    shiny::validate(
                        need(FALSE, "Data matrix too small to perform correlation analysis")
                    )
                    return()
                }
                
                # Identify "quant" columns
                quant_columns <- tdata[, -1, drop = FALSE]
                
                # Remove columns with all NAs
                quant_columns <- quant_columns[, colSums(!is.na(quant_columns)) > 0, drop = FALSE]
                
                # Remove constant columns to avoid errors in correlation calculation
                constant_columns <- apply(quant_columns, 2, function(col) var(col, na.rm = TRUE) == 0)
                quant_columns <- quant_columns[, !constant_columns]
                
                # Check for missing or infinite values
                quant_columns <- quant_columns[complete.cases(quant_columns),]
                
                # Ensure there is still enough data for correlation plot
                shiny::validate(
                    need(ncol(quant_columns) > 2, "Not enough columns with variance for correlation calculation"),
                    need(nrow(quant_columns) > 20, "Not enough samples for correlation calculation")
                )
                
                # Compute correlation matrix
                correlation_matrix <- cor(quant_columns, use = "pairwise.complete.obs")
                
                # Plot the correlation matrix with customized color scale and legend position
                gplots::heatmap.2(correlation_matrix,
                                  main = "Pairwise correlations between samples",
                                  symm = TRUE,        # Symmetrical plot
                                  scale = "none",     # No scaling
                                  col = gplots::redblue,  # Blue to red color scale
                                  breaks = seq(-1, 1, 0.01),  # Breaks from -1 to 1 for colors
                                  trace = "none",     # No trace lines
                                  cex.main = 1.5,     # Size of the main title
                                  
                                  # Add cell borders
                                  sepwidth = c(0.01, 0.01),  # Width of the separation between cells
                                  sepcolor = "white",        # Color of the separation lines (white)
                                  colsep = 1:ncol(correlation_matrix),  # Add separation for all columns
                                  rowsep = 1:nrow(correlation_matrix),  # Add separation for all rows
                                  
                                  margins = c(5, 5),  # Margins for the plot
                                  dendrogram = "both" # Show dendrograms on both axes
                )
                
                
            })
            
            
            
            
            
            ##########################################################################
            ##########################################################################
            ############### Help messages
            observeEvent(input$h_balancing, sendSweetAlert(session,
                                                           title = "Create a balanced design",
                                                           text = HTML("<p align='justify'>The different experimental conditions
    (sample types) need to be at least nearly balanced. For the
              further analysis, this means that the number of replicates
              per sample should be the same for each of them. You can balance the data
              by adding empty columns and/or removing excess replicates.
              Replicates with the same number have already been summarized.<br/>
              <i>Select below</i> the samples you want to exclude.<br/>
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
            
            observeEvent(input$h_batch_effect, sendSweetAlert(session,
                                                              title = "Batch Effect Detection and Correction",
                                                              text = HTML("<p align='justify'>
    Batch effects occur when systematic technical variations (e.g., due to different experimental conditions, days, or instrument runs) 
    introduce biases into your data, making it difficult to discern true biological differences. 
    Detecting and correcting for these effects is essential for reliable downstream analysis.<br/>
    <b>Important:</b>Apply batch correction only when you have a good reason to assume that there are batch effects!<br/><br/>
    
    <b>Detection:</b> The <i>BEclear</i> package is used to detect batch effects by calculating the median differences and 
    p-values for each feature across different batches. Features with significant differences between batches (p < 0.01 and median difference > 0.5) 
    are flagged as being affected by batch effects.<br/><br/>
    
    <b>Correction:</b> You can select one of the following batch correction methods:<br/>
    <i>None: </i>No batch correction is applied.<br/>
    <i>limma: </i>Batch effects are corrected using the linear modeling approach from the limma package 
    (<a href='https://bioconductor.org/packages/release/bioc/html/limma.html'>limma</a>), which estimates and removes the batch effect.<br/>
    <i>ComBat: </i>This method uses an empirical Bayes framework to adjust for batch effects by shrinking the batch means and variances 
    towards the overall mean (<a href='https://bioconductor.org/packages/release/bioc/html/sva.html'>sva: ComBat</a>).<br/>
    <b>Note:</b> Batch correction is only applied when there are at least two samples per batch.<br/><br/>

    Both methods are widely used for batch correction in omics data and help reduce false positives 
    and ensure that biological signal is retained. Select the method that best fits your experimental setup.
    </p>"),
                                                              type = "info", html = T
            ))
            
            
            
            return(list(
                next_tab = next_tab,
                processed_table = processed_table,
                processed_table = processed_table,
                other_cols = other_cols,
                pexp_design = pexp_design
            ))
            
        }
    )
}
