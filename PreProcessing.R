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
                    actionButton(ns("add_na_columns"), "Fill with empty columns", icon = icon("plus-circle"), style = "margin-top: 10px;"),
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
                    
                    # UI section for batch effect check button and indicator
                    fluidRow(column(10, h5(strong("Batch effect detection/correction")),
                                    actionButton(ns("batch_detection_button"), label = "Batch effect detection"),
                                    span(textOutput(ns("batch_effect_indicator")), style = "font-weight:bold; margin-left:10px;")
                    ),
                    column(2, actionBttn(ns("batch_effect"),
                                         icon = icon("info-circle"),
                                         style = "pill", 
                                         color = "royal", size = "xs")
                    )
                    ),
                    
                    # Added margin between Batch effect detection and correction
                    tags$div(style = "margin-top: 20px;"),
                    
                    fluidRow(column(10, 
                                    actionButton(ns("batch_correction_button"), label = "Batch effect correction")  # Added button for checking batch effect
                    )),
                    
                    # Batch correction method moved here, below the button
                    fluidRow(column(10, 
                                    selectInput(ns("batch_correction_method"), "Method", 
                                                choices = c("limma", "Combat"), selected = "limma") # New input for batch correction method
                    )),
                    style = 'border-left: 1px solid' 
      )              
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
                           plotOutput(ns("pca_combined"))), # Combined PCA Plot
                    column(6, 
                           plotOutput(ns("corrplot")) # Keep existing correlation plot
                    )
    ))
    
  )
}





#################### Server ##################
preProcessingServer <- function(id, parent, expDesign, log_operations) {
  moduleServer(
    id,
    function(input, output, session) {
      
      process_table <- reactiveVal(NULL)
      processed_table <- reactiveVal(NULL)
      uncorrected_table <- reactiveVal(NULL) # Reactive value to store the uncorrected state of the data
      result_table <- reactiveVal(NULL)
      pexp_design <- reactiveVal(NULL)
      exp_design <- reactiveVal(NULL)
      next_tab <- reactiveVal(NULL)
      
      # Reactive values to store group, batch and replicate information
      group_info <- reactiveVal(NULL)
      batch_info <- reactiveVal(NULL)
      replicate_info <- reactiveVal(NULL)
      
      
      ##########################################################################
      ##########################################################################
      observeEvent(expDesign$next_tab(), {
        if (!is.null(expDesign$next_tab())) {
          process_table(expDesign$process_table())
          pexp_design(expDesign$pexp_design())
          exp_design(expDesign$pexp_design())
          
          # Initialize processed_table with 'id' and 'quant' columns from process_table
          initial_data <- process_table()
          id_column <- initial_data[, grep("id", sapply(initial_data, class)), drop = FALSE]
          quant_columns <- initial_data[, grep("quant", sapply(initial_data, class)), drop = FALSE]
          uncorrected_table(cbind(id_column, quant_columns)) # Store initial uncorrected data
          processed_table(uncorrected_table()) # Initialize processed_table from uncorrected_table
          
          shinyjs::show("pr_c1")
          shinyjs::show("pr_c2")
          shinyjs::show("pr_c3")
          shinyjs::show("pr_plots")
          
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
      # Reactive observer to update processed_table when user modifies max_na, normalization, or summarization options
      observe({
        input$max_na
        input$normalization
        input$summarize
        tdata <- uncorrected_table()
        
        isolate({
          if (!is.null(tdata)) {
            withProgress(message = "Processing...", value = 0, min = 0, max = 1, {
              
              # Step 1: Filter based on maximum number of missing values per feature
              if (!is.null(input$max_na)) {
                tdata <- tdata[rowSums(is.na(tdata[, -1])) <= input$max_na, ]
                incProgress(0.3, detail = "Filtering based on missing values")
              }
              
              # Update processed_table after filtering
              processed_table(tdata)
              
              # Step 2: Apply normalization method
              if (input$normalization != "none") {
                if (input$normalization == "colMedians") {
                  tdata[, -1] <- t(t(tdata[, -1]) - colMedians(as.matrix(tdata[, -1]), na.rm = TRUE))
                } else if (input$normalization == "colMeans") {
                  tdata[, -1] <- t(t(tdata[, -1]) - colMeans(as.matrix(tdata[, -1]), na.rm = TRUE))
                } else if (input$normalization == "cyclicloess") {
                  tdata[, -1] <- limma::normalizeBetweenArrays(as.matrix(tdata[, -1]), method = "cyclicloess")
                }
                incProgress(0.4, detail = "Applying normalization")
              }
              
              # Update processed_table after normalization
              processed_table(tdata)
              
              # Step 3: Apply summarization method
              if (input$summarize != "none") {
                if (input$summarize == "colSums") {
                  tdata <- aggregate(. ~ id, data = tdata, FUN = sum, na.rm = TRUE)
                } else if (input$summarize == "colMeans") {
                  tdata <- aggregate(. ~ id, data = tdata, FUN = mean, na.rm = TRUE)
                } else if (input$summarize == "colMedians") {
                  tdata <- aggregate(. ~ id, data = tdata, FUN = median, na.rm = TRUE)
                } else if (input$summarize == "medianPolish") {
                  tdata <- MsCoreUtils::aggregate_by_vector(tdata, tdata$id, FUN = medianPolish, na.rm = TRUE)
                }
                incProgress(0.3, detail = "Applying summarization")
              }
              
              # Final update of processed_table after summarization
              processed_table(tdata)
            })
          }
        })
      })
      
      
      ##########################################################################
      ##########################################################################
      ## Check for balanced exp. design
      output$res_num_reps <- renderText({
        print("check for balancing")
        print(pexp_design())
        
        # Check whether balanced
        texp_design <- pexp_design()
        
        # Calculate the number of replicates per experimental condition
        ed_stats <- as.vector(table(texp_design[1, ]))
        
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
        
        tout
      })
      
      
      ##########################################################################
      ##########################################################################
      # Observe event for the "Fill with empty columns" button
      observeEvent(input$add_na_columns, {
        print("Adding NA columns...")
        
        tdata <- uncorrected_table()
        if (is.null(tdata)) return()
        
        reps <- table(pexp_design()[1, ])
        max_reps <- max(reps)
        tedes <- pexp_design()
        
        added_columns <- c()  # Track added columns for the summary
        
        # Generate a summary of the data before filling with empty columns
        before_filling_summary <- {
          # Identify "id" and "quant" columns
          id_column <- tdata[, which(sapply(tdata, function(col) all(is.character(col) | is.factor(col)))), drop = FALSE]
          quant_columns <- tdata[, which(sapply(tdata, is.numeric)), drop = FALSE]
          
          # Prepare the summary text
          paste(
            "<p><b>Before Filling:</b><br/><b>Size:</b> The current data table contains ", 
            ncol(quant_columns), " samples and ", nrow(quant_columns), " features.<br/><b>Missingness: </b>The proportion of missing values is ",
            round(sum(is.na(quant_columns)) / length(as.matrix(quant_columns)), 3), 
            ", with the number of missing values ranging from ", min(colSums(is.na(quant_columns))), 
            " to ", max(colSums(is.na(quant_columns))), " per sample.<br/><b>Range:</b> The dynamic range is from ",
            round(min(quant_columns, na.rm = TRUE), 2), " to ", round(max(quant_columns, na.rm = TRUE), 2), 
            ".<br/><b>Summarization:</b> The ID column contains ", 
            ifelse(sum(duplicated(id_column)) > 0, "non-unique IDs, and thus needs summarization.",
                   "unique IDs, so summarization is not required."), "</p>"
          )
        }
        
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
        
        # Reorder columns according to experimental design
        tedes <- tedes[, order(tedes[1, ], tedes[2, ])]
        pexp_design(tedes)
        
        # Disable batch correction when filling with empty columns
        shinyjs::disable("batch_correction_button")
        
        # Update the uncorrected and processed tables
        id_column <- tdata[, grep("id", sapply(tdata, class)), drop = FALSE]
        quant_columns <- tdata[, grep("quant", sapply(tdata, class)), drop = FALSE]
        
        # Update the uncorrected table
        uncorrected_table(cbind(id_column, quant_columns))
        
        # Update the processed table
        processed_table(cbind(id_column, quant_columns))  # Make sure new columns are added here
        
        ### Data Treatment Pre-submission and Data Manipulation Adjustment ###
        
        # Apply maximum number of missing values per feature
        if (!is.null(input$max_na)) {
          tdata <- tdata[rowSums(is.na(tdata[, -1])) <= input$max_na, ]
        }
        
        # Apply normalization method
        if (input$normalization != "none") {
          if (input$normalization == "colMedians") {
            tdata[, -1] <- t(t(tdata[, -1]) - colMedians(as.matrix(tdata[, -1]), na.rm = TRUE))
          } else if (input$normalization == "colMeans") {
            tdata[, -1] <- t(t(tdata[, -1]) - colMeans(as.matrix(tdata[, -1]), na.rm = TRUE))
          } else if (input$normalization == "cyclicloess") {
            tdata[, -1] <- limma::normalizeBetweenArrays(as.matrix(tdata[, -1]), method = "cyclicloess")
          }
        }
        
        # Apply summarization method
        if (input$summarize != "none") {
          if (input$summarize == "colSums") {
            tdata <- aggregate(. ~ id, data = tdata, FUN = sum, na.rm = TRUE)
          } else if (input$summarize == "colMeans") {
            tdata <- aggregate(. ~ id, data = tdata, FUN = mean, na.rm = TRUE)
          } else if (input$summarize == "colMedians") {
            tdata <- aggregate(. ~ id, data = tdata, FUN = median, na.rm = TRUE)
          } else if (input$summarize == "medianPolish") {
            tdata <- MsCoreUtils::aggregate_by_vector(tdata, tdata$id, FUN = medianPolish, na.rm = TRUE)
          }
        }
        
        # Update processed table after adjustments
        processed_table(tdata)
        
        # Regenerate summary after all adjustments
        after_adjustments_summary <- {
          paste(
            "<p><b>After Filling:</b><br/>New empty columns added: ", 
            paste(added_columns, collapse = ", "), "</p>"
          )
        }
        
        # Update the summary output
        output$ptable_summary <- renderUI({
          HTML(paste(before_filling_summary, after_adjustments_summary))
        })
      })
      
      
      
      
      
      ##########################################################################
      ##########################################################################
      # Observe event for removing columns
      observe({
        input$remove_reps
        tdata <- process_table()
        
        isolate({
          if (!is.null(tdata)) {
            withProgress(message = "Processing...", value = 0, min = 0, max = 1, {
              cnames <- colnames(exp_design())
              
              # Removing selected columns
              rem <- -which(names(tdata) %in% input$remove_reps)
              if (length(rem) > 0) {
                incProgress(0.1, detail = "Removing replicates")
                tdata <- tdata[, rem]
                rem2 <- -which(cnames %in% input$remove_reps)
                pexp_design(exp_design()[, rem2])
                cnames <- cnames[rem2]
                tlog <- log_operations()
                tlog[["preprocess_removed_replicates"]] <- input$remove_reps
                log_operations(tlog)
              } else {
                pexp_design(exp_design())
              }
              
              # Update uncorrected_table to keep only 'id' and 'quant' columns
              id_column <- tdata[, grep("id", sapply(tdata, class)), drop = FALSE]
              quant_columns <- tdata[, grep("quant", sapply(tdata, class)), drop = FALSE]
              uncorrected_table(cbind(id_column, quant_columns))
              processed_table(uncorrected_table()) # Keep processed_table in sync with uncorrected_table after modifications
            })
          }
        })
      })
      
      ##########################################################################
      ##########################################################################
      ### PCA Plot
      output$pca_combined <- renderPlot({
        print("Combined PCA Plot: Colored by Replicate, Shaped by Batch")
        
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
        id_column <- tdata[, which(sapply(tdata, function(col) all(is.character(col) | is.factor(col)))), drop = FALSE]
        quant_columns <- tdata[, which(sapply(tdata, is.numeric)), drop = FALSE]
        
        # Debugging: Print dimensions and sample data of id_column and quant_columns
        print(paste("id_column dimensions:", dim(id_column)))
        print(paste("quant_columns dimensions:", dim(quant_columns)))
        print("Sample of id_column:")
        print(head(id_column))
        print("Sample of quant_columns:")
        print(head(quant_columns))
        
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
          labs(title = "PCA Plot: Colored by Group, Shaped by Batch",
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
        quant_columns <- tdata[, which(sapply(tdata, is.numeric)), drop = FALSE]
        
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
        
        # Plot the correlation matrix
        gplots::heatmap.2(correlation_matrix,
                          main = "Pairwise correlations between samples",
                          symm = TRUE, scale = "none", col = gplots::redblue, breaks = seq(-1, 1, 0.01), trace = "none",
                          cex.main = 1.5)  # Equivalent to setting font size of the title)
      })
      
      
      
      
      ##########################################################################
      #################### Updated Code for Batch Effect Detection #############
      
      # Hide the batch_correction_button and batch_correction_method by default
      shinyjs::hide("batch_correction_button")
      shinyjs::hide("batch_correction_method")
      
      observeEvent(input$batch_detection_button, {
        tdata <- uncorrected_table()
        
        print("Detecting batch effects using BEclear...")
        
        # Store total number of features before processing
        total_features <- nrow(tdata)
        
        # Prepare the data
        texp_design <- pexp_design()
        
        # Select only quantitative columns and remove columns with all NAs
        tdata <- tdata[, grep("quant", sapply(tdata, class))]
        tdata <- tdata[, colSums(!is.na(tdata)) > 0]
        texp_design <- texp_design[, colnames(tdata)]
        
        # Store number of features after removing NAs
        features_without_na <- nrow(tdata[complete.cases(tdata), ])
        
        # Filter out rows with missing values
        tdata <- (tdata[complete.cases(tdata), ])
        
        batch_labels <- batch_info()
        
        # Ensure batch_labels has at least two levels
        if (length(unique(batch_labels)) < 2) {
          sendSweetAlert(session,
                         title = "Batch Effect Detection",
                         text = "Batch effect detection requires at least two distinct batch levels. Your data has only one batch. Please check your batch assignments.",
                         type = "warning")
          return()
        }
        
        # Create a samples data frame required for BEclear
        sample_ids <- colnames(tdata)  # Assuming column names of tdata are the sample IDs
        samples <- data.frame(sample_id = sample_ids, batch_id = batch_labels, pvaluesTreshold = 0.05)
        colnames(samples) <- c("sample_id", "batch_id")
        
        # Use BEclear to calculate batch effects
        batch_effect_results <- tryCatch({
          BEclear::calcBatchEffects(
            data = tdata, 
            samples = samples,
            adjusted = TRUE, 
            method = "fdr"
          )
        }, error = function(e) {
          sendSweetAlert(session,
                         title = "Batch Effect Detection Error",
                         text = paste("An error occurred during batch effect detection:", e$message),
                         type = "error")
          return(NULL)
        })
        
        if (is.null(batch_effect_results)) return() # Exit if error occurred
        
        # Extract median differences and p-values from the results
        mdifs <- batch_effect_results$med
        pvals <- batch_effect_results$pval
        summary <- calcSummary(medians = mdifs, pvalues = pvals, pvaluesTreshold = 0.05)
        
        # Determine the number of features with significant batch effects (p < 0.05)
        significant_batches <- nrow(summary)
        
        # Prepare the message with detailed information using HTML <br/> for line breaks
        message <- paste(
          "<div style='text-align: left;'>",  # Start of left-aligned div
          "Total features: <strong>", total_features, "</strong><br/>",
          "Features without missing values: <strong>", features_without_na, "</strong><br/>",
          "Features affected by batch effects (p < 0.05): <strong>", significant_batches, "</strong>",
          "<br/><br/>Would you like to run batch effect correction?",
          "</div>"  # End of left-aligned div
        )
        
        # Ask user whether they want to run batch correction along with the results
        confirmSweetAlert(
          session = session,
          inputId = "confirm_batch_correction",
          title = "Batch Effect Detection Finish!",
          text = HTML(message),  # Use HTML() to render the message
          type = "question",
          btn_labels = c("No", "Yes"),
          html = TRUE  # Enable HTML rendering
        )
      })
      
      # Observe the user's response from the confirmation dialog
      observeEvent(input$confirm_batch_correction, {
        if (input$confirm_batch_correction) {
          # User chooses Yes -> Show batch correction button and method dropdown
          shinyjs::show("batch_correction_button")
          shinyjs::show("batch_correction_method")
          #shinyjs::hide("batch_detection_button")
        } else {
          # User chooses No -> Keep batch correction button and method dropdown hidden
          shinyjs::hide("batch_correction_button")
          shinyjs::hide("batch_correction_method")
        }
      })
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      ##########################################################################
      ##########################################################################
      # Batch effect correction
      observeEvent(input$batch_correction_button, {
        print("Detecting and correcting batch effects...")
        
        # Use the updated uncorrected table which reflects any changes made in data treatment before batch correction
        tdata <- uncorrected_table()
        
        # Print initial dimensions of uncorrected_table
        print(paste("Initial uncorrected_table dimensions:", nrow(tdata), ncol(tdata)))
        
        # Ensure enough data is available for batch effect detection
        if (is.null(tdata) || ncol(tdata) < 2 || nrow(tdata) < 10) { 
          sendSweetAlert(session,
                         title = "Batch Effect Detection Warning",
                         text = "Data matrix too small to perform batch effect detection.",
                         type = "warning")
          return()
        }
        
        # Select only quantitative columns and remove columns with all NAs
        tdata <- tdata[, grep("quant", sapply(tdata, class)), drop = FALSE]
        print(paste("tdata after selecting quant columns:", nrow(tdata), ncol(tdata)))
        
        tdata <- tdata[, colSums(!is.na(tdata)) > 0, drop = FALSE]
        print(paste("tdata after removing columns with all NAs:", nrow(tdata), ncol(tdata)))
        
        tdata <- tdata[complete.cases(tdata), ]
        print(paste("tdata after removing rows with NAs:", nrow(tdata), ncol(tdata)))
        
        # Remove constant columns to avoid errors in batch correction
        constant_columns <- apply(tdata, 2, function(col) var(col, na.rm = TRUE) == 0)
        tdata <- tdata[, !constant_columns]
        print(paste("tdata after removing constant columns:", nrow(tdata), ncol(tdata)))
        
        # Ensure there is still enough data after removing columns
        if (ncol(tdata) < 2) {
          sendSweetAlert(session,
                         title = "Batch Effect Detection Warning",
                         text = "Not enough columns with variance for batch effect detection after removing constant columns.",
                         type = "warning")
          return()
        }
        
        # Get the current experimental design to align replicate and batch information
        texp_design <- pexp_design()
        
        # Ensure replicate_info() and batch_info() match the data after processing
        # Find the common indices based on column names that are retained after processing
        valid_indices <- match(colnames(tdata), colnames(texp_design))
        replicate_info_filtered <- replicate_info()[valid_indices]
        batch_info_filtered <- batch_info()[valid_indices]
        
        # Check if the valid_indices has NAs which mean the alignment was not successful
        if (any(is.na(valid_indices))) {
          sendSweetAlert(session,
                         title = "Batch Effect Detection Error",
                         text = "Mismatch between data and experimental design. Ensure alignment of data columns.",
                         type = "error")
          return()
        }
        
        # Proceed with batch effect detection and correction
        batch_labels <- batch_info_filtered
        
        # Ensure batch_labels has at least two levels
        if (length(unique(batch_labels)) < 2) {
          sendSweetAlert(session,
                         title = "Batch Effect Detection",
                         text = "Batch effect detection requires at least two distinct batch levels. Your data has only one batch. Please check your batch assignments.",
                         type = "warning")
          return()
        }
        
        # Check which method to use for batch correction
        method <- input$batch_correction_method
        
        if (method == "limma") {
          # Use limma to calculate and correct batch effects
          batch_effect_corrected <- tryCatch({
            removeBatchEffect(tdata, batch = as.factor(batch_labels))
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
            sva::ComBat(dat = tdata, batch = as.factor(batch_labels), mod = NULL, par.prior = TRUE, prior.plots = FALSE)
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
        
        # Print dimensions after batch effect correction
        print(paste("batch_effect_corrected dimensions:", nrow(batch_effect_corrected), ncol(batch_effect_corrected)))
        
        # Separate ID column and Quant columns
        id_column <- uncorrected_table()[, grep("id", sapply(uncorrected_table(), class)), drop = FALSE]
        quant_columns_corrected <- batch_effect_corrected
        
        # Ensure the id_column matches the number of rows in quant_columns_corrected after subsetting
        id_column <- id_column[rownames(quant_columns_corrected), , drop = FALSE]
        print(paste("id_column dimensions after subsetting:", nrow(id_column), ncol(id_column)))
        print(paste("quant_columns_corrected dimensions after subsetting:", nrow(quant_columns_corrected), ncol(quant_columns_corrected)))
        
        # Update processed_table with corrected batch data and ensure to keep ID and quant columns
        processed_table(data.frame(id_column, quant_columns_corrected))
        
        # Print the updated dimensions of processed_table
        print(paste("Updated processed_table dimensions after batch correction:", nrow(processed_table()), ncol(processed_table())))
        
        # Show an alert after correction
        sendSweetAlert(session,
                       title = "Batch Effect Correction",
                       text = paste("Batch effects have been successfully corrected using", method, "."),
                       type = "success")
      })
      
      
      ##########################################################################
      ##########################################################################
      # Summary of main properties of data table
      output$ptable_summary <- renderText({
        
        # Get the processed table, which reflects any changes made in data treatment
        tdata <- processed_table()
        
        # Ensure processed_table is available and valid
        if (is.null(tdata) || ncol(tdata) < 2 || nrow(tdata) < 10) { 
          shiny::validate(
            need(FALSE, "Data matrix too small for generating summary")
          )
          return()
        }
        
        # Identify "id" and "quant" columns
        id_column <- tdata[, which(sapply(tdata, function(col) all(is.character(col) | is.factor(col)))), drop = FALSE]
        quant_columns <- tdata[, which(sapply(tdata, is.numeric)), drop = FALSE]
        
        # Debugging: Print dimensions and sample data of id_column and quant_columns
        print(paste("id_column dimensions:", dim(id_column)))
        print(paste("quant_columns dimensions:", dim(quant_columns)))
        print("Sample of id_column:")
        print(head(id_column))
        print("Sample of quant_columns:")
        print(head(quant_columns))
        print("Show final_exp_design:")
        print(pexp_design)
        
        # Ensure there is still enough data after filtering quantitative columns
        if (ncol(quant_columns) < 2 || nrow(quant_columns) < 10) {
          shiny::validate(
            need(FALSE, "Not enough quantitative columns or rows for summary generation")
          )
          return()
        }
        
        # Prepare the summary text
        paste(
          "<p><b>Size:</b> The current data table contains ", ncol(quant_columns),
          " distinct samples across ",
          length(unique(pexp_design()[1, ])), " conditions, and comprises ",
          nrow(quant_columns), " features.<br/><b>Missingness:</b> The proportion of missing values is ",
          round(sum(is.na(quant_columns)) / length(as.matrix(quant_columns)), 3),
          ", with the number of missing values ranging from ",
          min(colSums(is.na(quant_columns))), " to ", max(colSums(is.na(quant_columns))),
          " per sample.<br/><b>Range:</b> The dynamic range is from ",
          round(min(quant_columns, na.rm = TRUE), 2), " to ", round(max(quant_columns, na.rm = TRUE), 2),
          ".<br/><b>Summarization:</b> The ID column contains ",
          ifelse(sum(duplicated(id_column)) > 0, "non-unique IDs, and thus needs summarization.",
                 "unique IDs, so summarization is not required."), "</p>"
        )
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
      
      observeEvent(input$batch_effect, sendSweetAlert(session,
                                                      title = "Batch effect detection/correction",
                                                      text = HTML("<p align='justify'><i> For batch effect detection, BEclear package was used.</p>"),
                                                      type = "info", html = T
      ))
      
      return(list(
        next_tab = next_tab,
        processed_table = processed_table,
        uncorrected_table = uncorrected_table,
        result_table = result_table,
        pexp_design = pexp_design
      ))
      
    }
  )
}
