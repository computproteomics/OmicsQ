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
                    actionButton(ns("send_polystest"), "Send to PolySTest"),  # Send button for PolySTest
                    span(textOutput(ns("connection_polystest")), style = "color:#33DD33;"),  # Display connection status
                    textInput(ns("url_polystest"), label = "URL", value = "http://computproteomics.bmb.sdu.dk/app_direct/PolySTest/"),  # PolySTest URL input
                    disabled(actionButton(ns("retrieve_polystest"), "Retrieve results from PolySTest"))  # Retrieve button, initially disabled
            )),
            # VSClust section for clustering
            (column(width = 4, id = ns("app_c2"),
                    h4("Clustering"),
                    actionButton(ns("send_vsclust"), "Send to VSClust"),  # Send button for VSClust
                    span(textOutput(ns("connection_vsclust")), style = "color:#33DD33;"),  # Display connection status
                    textInput(ns("url_vsclust"), label = "URL", value = "http://computproteomics.bmb.sdu.dk/app_direct/VSClust/"),  # VSClust URL input
                    disabled(actionButton(ns("retrieve_vsclust"), "Retrieve results from VSClust")),  # Retrieve button, initially disabled
                    style = 'border-left: 1px solid'    
            )),
            # ComplexBrowser section for investigating protein complexes
            (column(width = 4, id = ns("app_c3"),
                    h4("Investigate protein complex behavior"),
                    actionButton(ns("send_complexbrowser"), "Send to ComplexBrowser"),  # Send button for ComplexBrowser
                    span(textOutput(ns("connection_complexbrowser")), style = "color:#33DD33;"),  # Display connection status
                    textInput(ns("url_complexbrowser"), label = "URL", value = "http://computproteomics.bmb.sdu.dk/app_direct/ComplexBrowser/"),  # ComplexBrowser URL input
                    hidden(actionButton(ns("retrieve_complexbrowser"), "Retrieve results from ComplexBrowser")),  # Retrieve button, hidden by default
                    style = 'border-left: 1px solid'    
            ))
        ),
        # Download button for processed table
        br(),
        fluidRow(hidden(column(width = 4, id = ns("download_apps"), )), downloadBttn(ns("downloadTable"), label = "Download table")),
        br(),
        hidden(textInput(ns("app_log"), "app_log", value = NULL)),  # Hidden field to store log messages
        br(),
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
            log_vsclust <- reactiveVal(NULL)
            log_complexbrowser <- reactiveVal(NULL)
            log_polystest <- reactiveVal(NULL)
            result_table <- reactiveVal(NULL)  # Store the final table with results
            processed_table <- reactiveVal(NULL)  # Store the processed table from PreProcessing
            pexp_design <- reactiveVal(NULL)  # Store experimental design information
            other_cols <- reactiveVal(NULL) # Store additional columns (not quant or id)
            
            # Update and display the logs
            output$log_output <- renderText({
                input$app_log  # This will display the current log text
            })
            
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
            
            ## Show the processed table in a DataTable
            output$rtable <- DT::renderDT({
                # Display the processed_table or result_table if available
                datatable(data.frame(result_table(), other_cols()))  # Show result_table if it exists
            })
            
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
            observeEvent(input$send_vsclust, isolate({
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
                updateTextInput(session, "app_log", value = paste(input$app_log, "Opening VSClust and data upload ...", sep = "\n"))  # Log the action
                
                js$send_message(
                    url = input$url_vsclust,  # Send to VSClust URL
                    dat = VSClustMessage, tool = "VSClust"
                )
                enable("retrieve_vsclust")  # Enable the retrieve button
            }))
            
            # Log connection status for VSClust
            output$connection_vsclust <- renderText({
                toutput <- log_vsclust()  # Display the log for VSClust
                if (input$app_log != "" & !is.null(input$app_log)) {
                    if (grepl("vsclust", tolower(input$app_log))) {
                        toutput <- input$app_log
                        log_vsclust(toutput)  # Update VSClust log
                        updateTextInput(session, "app_log", value = "")
                    }
                }
                toutput  # Return the log output
            })
            
            # Sending message to retrieve results from VSClust
            observeEvent(input$retrieve_vsclust, isolate({
                updateTextInput(session, "app_log", value = "Getting VSClust results")  # Log the retrieval
                js$retrieve_results(
                    url = input$url_vsclust, dat = "Retrieve results", tool = "VSClust",
                    date = date()  # Retrieve results from VSClust
                )
            }))
            
            # Handle results from VSClust and merge them into result_table
            observeEvent(input$vsclust_results, isolate({
                if (is.list(input$vsclust_results)) {
                    tdata <- NULL
                    # Convert JSON response to table
                    for (n in names(input$vsclust_results[[1]])) {
                        tdata <- cbind(tdata, as.numeric(input$vsclust_results[[1]][[n]]))
                    }
                    colnames(tdata) <- names(input$vsclust_results[[1]])  # Assign column names
                    if (!any(colnames(result_table()) == "isClusterMember")) { # check whether VSClust was already run 
                        result_table(data.frame(result_table(), tdata))  # Combine results with processed table
                        updateTextInput(session, "app_log", value = paste(input$app_log, "Processed VSClust results", sep = "\n"))    
                    } else {
                        updateTextInput(session, "app_log", value = paste(input$app_log, "VSClust results already retrieved", sep = "\n"))
                    }
                    # Update the log with processed results
                    
                }
            }))
            
            ################################################### 
            ## PolySTest: Send data to PolySTest app
            observeEvent(input$send_polystest, isolate({
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
                updateTextInput(session, "app_log", value = paste(input$app_log, "Opening PolySTest and data upload ...", sep = "\n"))  # Log the action
                
                js$send_message(
                    url = input$url_polystest,  # Send to PolySTest URL
                    dat = PolySTestMessage, tool = "PolySTest"
                )
                enable("retrieve_polystest")  # Enable the retrieve button
            }))
            
            # Log connection status for PolySTest
            output$connection_polystest <- renderText({
                toutput <- log_polystest()  # Display the log for PolySTest
                if (!is.list(input$app_log) & input$app_log != "" & !is.null(input$app_log)) {
                    if (grepl("polystest", tolower(input$app_log))) {
                        toutput <- input$app_log
                        log_polystest(toutput)  # Update PolySTest log
                        updateTextInput(session, "app_log", value = "")
                    }
                }
                toutput  # Return the log output
            })
            
            # Retrieve results from PolySTest
            observeEvent(input$retrieve_polystest, isolate({
                updateTextInput(session, "app_log", value = "Getting PolySTest results")  # Log the retrieval
                js$retrieve_results(
                    url = input$url_polystest, dat = "Retrieve results", tool = "PolySTest"
                )
            }))
            
            # Handle results from PolySTest and merge them into result_table
            observeEvent(input$polystest_results, isolate({
                if (is.list(input$polystest_results)) {
                    tdata <- NULL
                    # Convert JSON response to table
                    for (n in names(input$polystest_results[[1]])) {
                        tdata <- cbind(tdata, as.numeric(input$polystest_results[[1]][[n]]))
                    }
                    colnames(tdata) <- names(input$polystest_results[[1]])  # Assign column names
                    
                    if (!any(grep("PolySTest", colnames(result_table())))) { # check whether PolySTest was already run 
                        result_table(data.frame(result_table(), tdata))  # Combine results with processed table
                        updateTextInput(session, "app_log", value = paste(input$app_log, "Processed PolySTest results", sep = "\n"))    
                    } else {
                        updateTextInput(session, "app_log", value = paste(input$app_log, "PolySTest results already retrieved", sep = "\n"))
                    }
                }
            }))
            
            
            ###################################################
            ## ComplexBrowser: Send data to ComplexBrowser app
            observeEvent(input$send_complexbrowser, isolate({
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
                updateTextInput(session, "app_log", value = paste(input$app_log, "Opening ComplexBrowser and data upload ...", sep = "\n"))  # Log the action
                js$send_message(
                    url = input$url_complexbrowser, dat = ComplexBrowserMessage,
                    tool = "ComplexBrowser"
                )
            }))
            
            # Log connection status for ComplexBrowser
            output$connection_complexbrowser <- renderText({
                toutput <- log_complexbrowser()  # Display the log for ComplexBrowser
                if (input$app_log != "" & !is.null(input$app_log)) {
                    if (grepl("complexbrowser", tolower(input$app_log))) {
                        toutput <- input$app_log
                        log_complexbrowser(toutput)  # Update ComplexBrowser log
                        updateTextInput(session, "app_log", value = "")
                    }
                }
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
