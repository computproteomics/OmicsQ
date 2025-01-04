#################### UI ##################
sendRetrieveUI <- function(id, prefix="") {
    ns <- NS(id)  # Namespace function to avoid ID conflicts in Shiny modules
    tagList(
        # Section for analyzing table with external apps
        fluidRow(column(7, h3("Analyze the table with the different apps")),
                 column(1, actionBttn(ns("h_apps"),
                                      icon = icon("info-circle"),  # Info button for app analysis help
                                      style = "pill", 
                                      color = "royal", size = "xs")
                 )),
        # Paired experimental design toggle switch
        fluidRow(column(10, switchInput(ns("paired"), "Paired experimental design", value = F))
        ),
        
        # UI for sending and retrieving results from different apps
        fluidRow(
            # PolySTest section for statistical testing
            (column(width = 4, id = ns("app_c1"),
                    h4("Statistical testing"),
                    actionButton(ns("send_PolySTest"), "Send to PolySTest"),  # Send button for PolySTest
                    span(textOutput(ns("connection_PolySTest")), style = "color:#33DD33;"),  # Display connection status
                    textInput(ns("url_PolySTest"), label = "URL", value = "http://computproteomics.bmb.sdu.dk/app_direct/PolySTest/"),  # PolySTest URL input
                    disabled(actionButton(ns("retrieve_PolySTest"), "Retrieve results from PolySTest"))  # Retrieve button, initially disabled
            )),
            # VSClust section for clustering
            (column(width = 4, id = ns("app_c2"),
                    h4("Clustering"),
                    actionButton(ns("send_VSClust"), "Send to VSClust"),  # Send button for VSClust
                    span(textOutput(ns("connection_VSClust")), style = "color:#33DD33;"),  # Display connection status
                    textInput(ns("url_VSClust"), label = "URL", value = "http://computproteomics.bmb.sdu.dk/app_direct/VSClust/"),  # VSClust URL input
                    disabled(actionButton(ns("retrieve_VSClust"), "Retrieve results from VSClust")),  # Retrieve button, initially disabled
                    style = 'border-left: 1px solid'    
            )),
            # ComplexBrowser section for investigating protein complexes
            (column(width = 4, id = ns("app_c3"),
                    h4("Investigate protein complex behavior"),
                    actionButton(ns("send_ComplexBrowser"), "Send to ComplexBrowser"),  # Send button for ComplexBrowser
                    span(textOutput(ns("connection_ComplexBrowser")), style = "color:#33DD33;"),  # Display connection status
                    textInput(ns("url_ComplexBrowser"), label = "URL", value = "http://computproteomics.bmb.sdu.dk/app_direct/ComplexBrowser/"),  # ComplexBrowser URL input
                    hidden(actionButton(ns("retrieve_ComplexBrowser"), "Retrieve results from ComplexBrowser")),  # Retrieve button, hidden by default
                    style = 'border-left: 1px solid'    
            ))
        ),
        # Download button for processed table
        br(),
        hidden(textInput(ns("app_log"), "app_log", value = NULL)),  # Hidden field to store log messages
        hidden(textInput(ns("VSClust_results"), "VSClust_results", value = NULL)),  # Hidden field to store log messages
        hidden(textInput(ns("PolySTest_results"), "PolySTest_results", value = NULL)),  # Hidden field to store log messages
        hr(),
        actionButton(ns("send_stringdb"), "Send (filtered) features to stringDB"),br(),br(),
        downloadBttn(ns("downloadTable"), label = "Download table"), 
        # Display the processed table
        fluidRow(
            DTOutput(ns('rtable'))  # Display processed table in a DataTable
        ),
        
    )
}



############## Server #######
sendRetrieveServer <- function(id, preProcessing, log_operations) {
    moduleServer(
        id,
        function(input, output, session) {
            # Reactive values to store logs and data
            log_VSClust <- reactiveVal(NULL)
            log_ComplexBrowser <- reactiveVal(NULL)
            log_PolySTest <- reactiveVal(NULL)
            result_table <- reactiveVal(NULL)  # Store the final table with results
            processed_table <- reactiveVal(NULL)  # Store the processed table from PreProcessing
            pexp_design <- reactiveVal(NULL)  # Store experimental design information
            other_cols <- reactiveVal(NULL) # Store additional columns (not quant or id)
            
            # Update and display the logs
            output$log_output <- renderText({
                input$app_log  # This will display the current log text
            })
            
            # overwrite app logs when new logs with client messagers
            # NOT WORKING:
            observeEvent(
                input$app_log, isolate({
                    print((input$app_log))
                    if (length(input$app_log) > 1) {
                        print(grepl("PolySTest", input$app_log))
                        if(grepl("VSClust", input$app_log)) {
                            log_VSClust(input$app_log)
                        } else if(grepl("PolyfSTest", input$app_log)) {
                            log_PolySTest(input$app_log)
                            print(log_PolySTest())
                        } else if(grepl("ComplexBrowser", input$app_log)) {
                            log_ComplexBrowser(input$app_log)
                        }
                    }
                })
                
            )
            
            # Ensure processed_table is always updated with changes from PreProcessing
            observe({
                tout <- preProcessing$processed_table()  # Update processed_table reactively
                # Remove classes id and quant to avoid error in toJSON
                if (!is.null(tout)) {
                    class(tout[,1]) <- "character"
                    for (i in 2:ncol(tout)) {
                        class(tout[,i]) <- "numeric"
                    }
                }
                
                processed_table(tout)
                other_cols(preProcessing$other_cols())
                pexp_design(preProcessing$pexp_design())  # Update experimental design reactively
                isolate(
                    result_table(processed_table())
                )
            })
            
            ## Show the processed table in a DataTable with advanced filter & sorting
            output$rtable <- DT::renderDT({
                # If result_table is not NULL, bind your additional columns
                if (!is.null(result_table())) {
                    
                    # Combine main and additional columns
                    full_data <- data.frame(result_table(), other_cols())
                    
                    # Create the DataTable
                    DT::datatable(
                        data = full_data,
                        rownames = FALSE,
                        
                        # Place a filter row at the top (or "bottom")
                        filter = "top",  
                        
                        # Use DT extensions for extra features like exporting or column reordering
                        extensions = c("Buttons", "ColReorder"), 
                        
                        # A few common table options
                        options = list(
                            # Let users change how many rows to display
                            pageLength = 10,
                            lengthMenu = c(5, 10, 20, 50, 100),  
                            
                            # Use the “Bfrtip” layout:
                            # B = Buttons, f = filter, r = processing info, t = table, i = info, p = pagination
                            dom = "frtip",
                            
                            # Define which buttons you want (e.g. copy, CSV, Excel, PDF, Print)
                            buttons = c("copy", "csv", "excel", "pdf", "print"),
                            
                            # Allow column reordering
                            colReorder = TRUE,
                            
                            # Horizontal scrolling if needed
                            scrollX = TRUE
                        )
                    ) %>%
                        # Example of conditional formatting:
                        formatStyle(
                            grep("^PolySTest", colnames(full_data)),
                            backgroundColor = 'lightblue'
                        ) %>%
                        formatStyle(
                            grep("^VSClust", colnames(full_data)),
                            backgroundColor = 'lightgreen'
                        ) %>% 
                        formatStyle(
                            colnames(data.frame(other_cols())),
                            backgroundColor = 'lightcoral'
                        )
                }
            })
            
            # log filters
            observeEvent(input$rtable_search, {
                # Whenever the global search changes
                req(input$rtable_search)  # Ensure it's not NULL
                tlog <- log_operations()
                tlog[["datatable_global_search"]] <- input$rtable_search
                log_operations(tlog)
            })
            
            observeEvent(input$rtable_search_columns, {
                print("search columns")
                # Whenever the column-specific filters change
                req(input$rtable_search_columns)  # Ensure it's not NULL
                tlog <- log_operations()
                # This will be a character vector corresponding to each column’s filter box
                # e.g. c("value for 1st col filter", "value for 2nd col", ...)
                tlog[["datatable_column_filters"]] <- input$rtable_search_columns
                log_operations(tlog)
            })
            
            
            # send selected id features in rtable to stringdb
            observeEvent(input$send_stringdb, {
                print("Sending features to STRINGDB")
                cat("Sending features to STRINGDB\n")
                selected_rows <- input$rtable_rows_all
                if (length(selected_rows) == 0) {
                    sendSweetAlert(session,
                                   title = "Submission to StringDB",
                                   text = paste("No features"),
                                   type = "error")
                    return(NULL)
                } else if (length(selected_rows) > 1000) {
                    sendSweetAlert(session,
                                   title = "Submission to StringDB",
                                   text = paste("Too many features selected. Please select less than 1000 features"),
                                   type = "error")
                    return(NULL)
                }
                
                # Extract the feature IDs from the first column, for example
                feature_ids <- result_table()[selected_rows, 1]
                
                # Encode them as newline (%0D%0A) separated
                id_block <- paste(feature_ids, collapse = "%0d")
                
                # Build the query. Typically you also want to specify species, etc.
                # Adjust as needed (e.g., species_text, limit, etc.)
                stringdb_url <- paste0(
                    "https://string-db.org/cgi/network?",
                    "identifiers=", id_block,
#                    "&species_text=Homo+sapiens",
                    "&show_query_node_labels=1"
                )
                
                # Open the URL in the user's browser
                js$send_message(
                    url = stringdb_url,  # Send to VSClust URL
                    dat = NULL, tool = "STRINGDB"
                )
            })
            
            
            
            # ## Show the processed table in a DataTable
            # output$rtable <- DT::renderDT({
            #     # Display the processed_table or result_table if available
            #     if (!is.null(result_table())) {
            #     DT::datatable(data.frame(result_table(), other_cols())) %>%
            #         formatStyle(
            #             grep("^PolySTest", colnames(data.frame(result_table()))),
            #             backgroundColor = 'lightblue'
            #         ) %>%
            #         formatStyle(
            #             grep("^VSClust", colnames(data.frame(result_table()))),
            #             backgroundColor = 'lightgreen'
            #         ) %>% 
            #         formatStyle(
            #             colnames(data.frame(other_cols())),
            #             backgroundColor = 'lightcoral'
            #         )
            #     }
            # })
            
            ##### Download Table Logic
            output$downloadTable <- downloadHandler(
                filename = function() {
                    # Validate that there is data before proceeding
                    if (is.null(result_table())) {
                        return(NULL)  # If no data, do not generate filename
                    }
                    paste("OmicsQResults", Sys.Date(), ".csv", sep = "")  # Name the file as a CSV
                }, 
                content = function(file) {
                    # Write the data to CSV: either processed or result table
                    write.csv(data.frame(result_table(), other_cols()), file, row.names = FALSE)
                }
            )
            
            ###################################################
            ## VSClust: Send data to VSClust app
            observeEvent(input$send_VSClust, isolate({
                print("Sending data to VSClust")
                # Extract processed data and prepare it for sending
                outdat <- processed_table()
                final_exp_design <- pexp_design()  # Get experimental design
                NumCond <- length(unique(final_exp_design[1, ]))  # Number of conditions
                NumReps <- table(final_exp_design[1, ])[1]  # Number of replicates per condition
                
                # Prepare the message to send to VSClust
                VSClustMessage <- toJSON(list(
                    numrep = NumReps, numcond = NumCond, grouped = F,
                    paired = input$paired, modsandprots = F,
                    expr_matrix = as.list(as.data.frame(outdat))  # Send data matrix
                ))
                
                # Update the log message
                log_VSClust("Opening VSClust and data upload ...")  # Log the action
                
                js$send_message(
                    url = input$url_VSClust,  # Send to VSClust URL
                    dat = VSClustMessage, tool = "VSClust"
                )
                enable("retrieve_VSClust")  # Enable the retrieve button
            }))
            
            # Log connection status for VSClust
            output$connection_VSClust <- renderText({
                print("Checking connection status for VSClust")
                toutput <- log_VSClust()  # Display the log for VSClust
                toutput  # Return the log output
            })
            
            # Sending message to retrieve results from VSClust
            observeEvent(input$retrieve_VSClust, isolate({
                print("Retrieving VSClust results")
                log_VSClust("Requesting VSClust results")  # Log the retrieval
                js$retrieve_results(
                    url = input$url_VSClust, dat = "Retrieve results", tool = "VSClust",
                    date = date()  # Retrieve results from VSClust
                )
            }))
            
            # Handle results from VSClust and merge them into result_table
            observeEvent(input$VSClust_results, isolate({
                print("Adding VSClust results")
                if (is.list(input$VSClust_results)) {
                    tdata <- NULL
                    # Convert JSON response to table
                    for (n in names(input$VSClust_results[[1]])) {
                        tdata <- cbind(tdata, as.numeric(input$VSClust_results[[1]][[n]]))
                    }
                    tdata <- data.frame(tdata)
                    colnames(tdata) <- names(input$VSClust_results[[1]])  # Assign column names
                    if (!any(colnames(result_table()) == "isClusterMember")) { # check whether VSClust was already run 
                        log_VSClust("Added VSClust results to table")    
                    } else {
                        log_VSClust("VSClust: overwriting already retrieved results")
                        result_table(result_table()[, !grepl("^VSClust", colnames(result_table()))])
                    }
                    colnames(tdata) <- paste("VSClust", colnames(tdata), sep = "_")  # Add prefix to column names
                    tdata[,"VSClust_isClusterMember"] <- as.logical(tdata[,"VSClust_isClusterMember"])  # Convert to logical
                    result_table(data.frame(result_table(), tdata))  # Combine results with processed tablec
                    # Update the log with processed results
                    tlog <- log_operations()
                    tlog[["VSClust version"]] <- input$VSClust_results$version
                    log_operations(tlog)
                }
            }))
            
            ################################################### 
            ## PolySTest: Send data to PolySTest app
            observeEvent(input$send_PolySTest, isolate({
                print("Sending data to PolySTest")
                # Extract processed data and prepare it for PolySTest
                outdat <- processed_table()
                final_exp_design <- pexp_design()  # Get experimental design
                NumCond <- length(unique(final_exp_design[1, ]))  # Number of conditions
                NumReps <- table(final_exp_design[1, ])[1]  # Number of replicates per condition
                
                # Prepare the message to send to PolySTest
                PolySTestMessage <- toJSON(list(
                    numrep = NumReps, numcond = NumCond, grouped = F,
                    paired = input$paired, firstquantcol = 2,
                    expr_matrix = as.list(as.data.frame(outdat))  # Send data matrix
                ))
                
                # Update the log
                log_PolySTest("Opening PolySTest and data upload ...")  # Log the action
                
                js$send_message(
                    url = input$url_PolySTest,  # Send to PolySTest URL
                    dat = PolySTestMessage, tool = "PolySTest"
                )
                enable("retrieve_PolySTest")  # Enable the retrieve button
            }))
            
            # Log connection status for PolySTest
            output$connection_PolySTest <- renderText({
                print("Checking connection status for PolySTest")
                toutput <- log_PolySTest()  # Display the log for PolySTest
                toutput  # Return the log output
            })
            
            # Retrieve results from PolySTest
            observeEvent(input$retrieve_PolySTest, isolate({
                print("Retrieving PolySTest results")
                log_PolySTest("Requesting PolySTest results")  # Log the retrieval
                js$retrieve_results(
                    url = input$url_PolySTest, dat = "Retrieve results", tool = "PolySTest"
                )
            }))
            
            # Handle results from PolySTest and merge them into result_table
            observeEvent(input$PolySTest_results, isolate({
                enable("send_stringdb")
                print("Adding PolySTest results")
                if (is.list(input$PolySTest_results)) {
                    tdata <- NULL
                    # Convert JSON response to table
                    for (n in names(input$PolySTest_results[[1]])) {
                        tdata <- cbind(tdata, as.numeric(input$PolySTest_results[[1]][[n]]))
                    }
                    colnames(tdata) <- names(input$PolySTest_results[[1]])  # Assign column names
                    
                    if (!any(grep("^PolySTest", colnames(result_table())))) { # check whether PolySTest was already run 
                        log_PolySTest("Added PolySTest results to table")    
                    } else {
                        log_PolySTest("PolySTest: overwriting already retrieved results")
                        result_table(result_table()[, !grepl("^PolySTest", colnames(result_table()))])
                    }
                    colnames(tdata) <- paste("PolySTest", colnames(tdata), sep = "_")  # Add prefix to column names
                    result_table(data.frame(result_table(), tdata))  # Combine results with processed table
                    # Update the log with processed results
                    tlog <- log_operations()
                    tlog[["PolySTest version"]] <- input$PolySTest_results$version
                    log_operations(tlog)
                    
                }
            }))
            
            
            ###################################################
            ## ComplexBrowser: Send data to ComplexBrowser app
            observeEvent(input$send_ComplexBrowser, isolate({
                print("Sending data to ComplexBrowser")
                # Extract processed data and prepare it for ComplexBrowser
                outdat <- processed_table()
                final_exp_design <- pexp_design()  # Get experimental design
                NumCond <- length(unique(final_exp_design[1, ]))  # Number of conditions
                NumReps <- table(final_exp_design[1, ])[1]  # Number of replicates per condition
                
                # Prepare the message to send to ComplexBrowser
                ComplexBrowserMessage <- toJSON(list(
                    numrep = NumReps, numcond = NumCond,
                    grouped = T, paired = input$paired, withstats = F,
                    expr_matrix = as.list(as.data.frame(outdat))  # Send data matrix
                ))
                log_ComplexBrowser("Opening ComplexBrowser and data upload ...")  # Log the action
                js$send_message(
                    url = input$url_ComplexBrowser, dat = ComplexBrowserMessage,
                    tool = "ComplexBrowser"
                )
            }))
            
            # Log connection status for ComplexBrowser
            output$connection_ComplexBrowser <- renderText({
                toutput <- log_ComplexBrowser()  # Display the log for ComplexBrowser
                toutput  # Return the log output
            })
            
            ############### Help messages
            observeEvent(input$h_apps, sendSweetAlert(session,
                                                      title = "Call apps for further analysis",
                                                      text = HTML("<p align='justify'>You can submit your data set to the apps
              <a href='http://computproteomics.bmb.sdu.dk/Apps/PolySTest'>PolySTest</a>,
              <a href='http://computproteomics.bmb.sdu.dk/Apps/VSClust'>VSClust</a>, and
              <a href='http://computproteomics.bmb.sdu.dk/Apps/ComplexBrowser'>ComplexBrowser</a>. The
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
                                                      type = "info", html = T
            ))
        }
    )
}
