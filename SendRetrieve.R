#################### UI ##################
sendRetrieveUI <- function(id, prefix="") {
  ns <- NS(id)
  tagList(
    fluidRow(column(7,h3("Analyze the table with the different apps")),
             column(1,actionBttn(ns("h_apps"),
                                 icon=icon("info-circle"),
                                 style="pill", 
                                 color = "royal", size = "xs")
             )),
    fluidRow(column(10, switchInput(ns("paired"), "Paired experimental design",value=F))
    ),
    
    
    fluidRow(
      hidden(column(width=4, id=ns("app_c1"),
                    h4("Statistical testing"),
                    actionButton(ns("send_polystest"), "Send to PolySTest"),
                    span(textOutput(ns("connection_polystest")), style="color:#33DD33;"),
                    #textInput("url_polystest",label="URL",value="http://localhost:3838/Apps/PolySTest/"),
                    textInput(ns("url_polystest"),label="URL",value="http://computproteomics.bmb.sdu.dk/app_direct/PolySTest/"),
                    disabled(actionButton(ns("retrieve_polystest"), "Retrieve results from PolySTest"))
      )),
      hidden(column(width=4, id=ns("app_c2"),
                    h4("Clustering"),
                    actionButton(ns("send_vsclust"), "Send to VSClust"),
                    span(textOutput(ns("connection_vsclust")), style="color:#33DD33;"),
                    #textInput("url_vsclust",label="URL",value="http://localhost:3838/Apps/vsclust/inst/shiny/"),
                    textInput(ns("url_vsclust"),label="URL",value="http://computproteomics.bmb.sdu.dk/app_direct/VSClust/"),
                    disabled(actionButton(ns("retrieve_vsclust"), "Retrieve results from VSClust")),
                    style = 'border-left: 1px solid'    
      )
      ),
      hidden(column(width=4, id=ns("app_c3"),
                    h4("Investigate protein complex behavior"),
                    actionButton(ns("send_complexbrowser"), "Send to ComplexBrowser"),
                    span(textOutput(ns("connection_complexbrowser")), style="color:#33DD33;"),
                    textInput(ns("url_complexbrowser"),label="URL",value="http://computproteomics.bmb.sdu.dk/app_direct/ComplexBrowser/"),
                    hidden(actionButton(ns("retrieve_complexbrowser"), "Retrieve results from ComplexBrowser")),
                    style = 'border-left: 1px solid'    
      ))
    ),
    br(),
    fluidRow(hidden(column(width=4, id=ns("download_apps"), )),downloadBttn(ns("downloadTable"),label = "Download table")),
    br(),
    hidden(textInput(ns("app_log"), "app_log", value=NULL)),
    br(),
    fluidRow(
      DTOutput(ns('rtable'))
    )
  )
}


############## Server #######
sendRetrieveServer <- function(id, preProcessing, log_operations) {
  moduleServer(
    id,
    function(input, output, session) {
      log_vsclust <- reactiveVal(NULL)
      log_complexbrowser <- reactiveVal(NULL)
      log_polystest <- reactiveVal(NULL)
      result_table <- reactiveVal(NULL)
      processed_table <- reactiveVal(NULL)
      pexp_design <- reactiveVal(NULL)
      
      observeEvent(preProcessing$next_tab(), {
        if (!is.null(preProcessing$next_tab())) {
          processed_table(preProcessing$processed_table())
          result_table(preProcessing$result_table())
          pexp_design(preProcessing$pexp_design())
          
          shinyjs::show("app_c1")
          shinyjs::show("app_c2")
          shinyjs::show("app_c3")
          
        }
      })
      
      
      ##### table
      output$downloadTable <- downloadHandler(filename = function() {
        validate(need(NULL, "No data"))
        paste("Results", Sys.Date(), ".csv", sep = "")
      }, content = function(file) {
        if (is.null(result_table())) {
          write.csv(processed_table())
        } else {
          write.csv(result_table())
        }
      })
      
      ## Show table
      output$rtable <- DT::renderDT({
        if (is.null(result_table())) {
          data.table(processed_table())
        } else {
          data.table(result_table())
        }
      })
      
      
      ## VSClust Sent data to VSClust
      observeEvent(input$send_vsclust, isolate({
        # make table in right format
        tdata <- processed_table()
        outdat <- as.matrix(tdata[, grep("quant", sapply(tdata, class))])
        outdat <- cbind(tdata[, grep("id", sapply(tdata, class))], outdat)
        final_exp_design <- pexp_design()
        NumCond <- length(unique(final_exp_design[1, ]))
        NumReps <- table(final_exp_design[1, ])[1]
        # print(outdat)
        VSClustMessage <- toJSON(list(
          numrep = NumReps, numcond = NumCond, grouped = F,
          paired = input$paired, modsandprots = F,
          expr_matrix = as.list(as.data.frame(outdat))
        ))
        updateTextInput(session, "app_log",
                        value = "Opening VSClust and data upload ..."
        )
        js$send_message(
          url = input$url_vsclust,
          dat = VSClustMessage, tool = "VSClust"
        )
        enable("retrieve_vsclust")
      }))
      
      # Log for VSClust
      output$connection_vsclust <- renderText({
        toutput <- log_vsclust()
        # print(input$app_log)
        if (input$app_log != "" & !is.null(input$app_log)) {
          if (grepl("vsclust", tolower(input$app_log))) {
            toutput <- input$app_log
            # print(toutput)
            log_vsclust(toutput)
            updateTextInput(session, "app_log", value = "")
          }
        }
        toutput
      })
      
      # Sending message to retrieve results
      observeEvent(input$retrieve_vsclust, isolate({
        updateTextInput(session, "app_log", value = "Getting VSClust results")
        js$retrieve_results(
          url = input$url_vsclust, dat = "Retrieve results", tool = "VSClust",
          date = date()
        )
      }))
      
      
      # Merging PolySTest results into result r_table
      observeEvent(input$vsclust_results, isolate({
        print("Processing VSClust results")
        if (is.list(input$vsclust_results)) {
          print("data table received")
          tdata <- NULL
          for (n in names(input$vsclust_results[[1]])) {
            tdata <-
              cbind(tdata, as.numeric(input$vsclust_results[[1]][[n]]))
          }
          colnames(tdata) <- names(input$vsclust_results[[1]])
          # print(head(tdata)) print(summary(input$polystest_results[[1]]))
          # print(dim(as.data.frame(input$polystest_results[[1]])))
          if (is.null(result_table)) {
            result_table(cbind(processed_table(), tdata))
          } else {
            result_table(cbind(result_table(), tdata))
          }
          updateTextInput(session, "app_log", value = "Processed VSClust results")
        }
      }))
      
      
      ## PolySTest Sent data to PolySTest
      observeEvent(input$send_polystest, isolate({
        # make table in right format
        tdata <- processed_table()
        outdat <- as.matrix(tdata[, grep("quant", sapply(tdata, class))])
        outdat <- cbind(tdata[, grep("id", sapply(tdata, class))], outdat)
        final_exp_design <- pexp_design()
        NumCond <- length(unique(final_exp_design[1, ]))
        NumReps <- table(final_exp_design[1, ])[1]
        # print(outdat)
        PolySTestMessage <- toJSON(list(
          numrep = NumReps, numcond = NumCond, grouped = F,
          paired = input$paired, firstquantcol = 2,
          expr_matrix = as.list(as.data.frame(outdat))
        ))
        updateTextInput(session, "app_log",
                        value = "Opening PolySTest and data upload ..."
        )
        js$send_message(
          url = input$url_polystest,
          dat = PolySTestMessage, tool = "PolySTest"
        )
        enable("retrieve_polystest")
      }))
      
      # Log for PolySTest
      output$connection_polystest <- renderText({
        toutput <- log_polystest()
        # print(input$app_log)
        if (!is.list(input$app_log) & input$app_log != "" & !is.null(input$app_log)) {
          if (grepl("polystest", tolower(input$app_log))) {
            toutput <- input$app_log
            # print(toutput)
            log_polystest(toutput)
            updateTextInput(session, "app_log", value = "")
          }
        }
        toutput
      })
      
      # Sending message to retrieve results
      observeEvent(input$retrieve_polystest, isolate({
        updateTextInput(session, "app_log", value = "Getting PolySTest results")
        js$retrieve_results(
          url = input$url_polystest, dat = "Retrieve results",
          tool = "PolySTest"
        )
      }))
      
      # Merging PolySTest results into result r_table
      observeEvent(input$polystest_results, isolate({
        print("Processing PolySTest results")
        if (is.list(input$polystest_results)) {
          print("data table received")
          # jsonmessage <- fromJSON(input$polystest_results)
          # print(head(jsonmessage[['expr_matrix']]))
          tdata <- NULL
          for (n in names(input$polystest_results[[1]])) {
            tdata <- cbind(
              tdata,
              as.numeric(input$polystest_results[[1]][[n]])
            )
          }
          colnames(tdata) <- names(input$polystest_results[[1]])
          # print(head(tdata)) print(summary(input$polystest_results[[1]]))
          # print(dim(as.data.frame(input$polystest_results[[1]])))
          if (is.null(result_table())) {
            result_table(cbind(processed_table(), tdata))
          } else {
            result_table(cbind(result_table(), tdata))
          }
          updateTextInput(session, "app_log", value = "Processed PolySTest results")
        }
      }))
      
      ## ComplexBrowser Sending data to ComplexBrowser
      observeEvent(input$send_complexbrowser, isolate({
        # make table in right format
        tdata <- processed_table()
        outdat <- as.matrix(tdata[, grep("quant", sapply(tdata, class))])
        outdat <- cbind(tdata[, grep("id", sapply(tdata, class))], outdat)
        final_exp_design <- pexp_design()
        NumCond <- length(unique(final_exp_design[1, ]))
        NumReps <- table(final_exp_design[1, ])[1]
        # print(outdat) TODO allow PolySTest input for including statistics
        ComplexBrowserMessage <- toJSON(list(
          numrep = NumReps, numcond = NumCond,
          grouped = F, paired = input$paired, withstats = F,
          expr_matrix = as.list(as.data.frame(outdat))
        ))
        updateTextInput(session, "app_log",
                        value = "Opening ComplexBrowser and data upload ..."
        )
        js$send_message(
          url = input$url_complexbrowser, dat = ComplexBrowserMessage,
          tool = "ComplexBrowser"
        )
      }))
      
      # Log for ComplexBrowser
      output$connection_complexbrowser <- renderText({
        toutput <- log_complexbrowser()
        # print(input$app_log)
        if (input$app_log != "" & !is.null(input$app_log)) {
          if (grepl("complexbrowser", tolower(input$app_log))) {
            toutput <- input$app_log
            # print(toutput)
            log_complexbrowser(toutput)
            updateTextInput(session, "app_log", value = "")
          }
        }
        toutput
      })
      
      ############### Help messages
      observeEvent(input$h_apps, sendSweetAlert(session,
                                                title = "Call apps for further analysis",
                                                text = HTML("<p align='justify'>You can submit your data set to the apps
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
                                                type = "info", html = T
      ))
      
      
    }
  )
}

