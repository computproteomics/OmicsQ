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
    fluidRow(
      column(width=4,
             h3("Automatic selection of experimental groups"),
             p("Find most suitable settings. You  can manually edit the experimental design in the table below.
                      Replicates with equal number and of the same sample type will be summarized to one replicate."),
             actionBttn(ns("h_exp_design"),
                        icon=icon("info-circle"),
                        style="pill", 
                        color = "royal", size = "xs"),
             hidden(
               sliderInput(ns("dist_thresh"), "Threshold for string distance", value=0, min=0, max=1)
             ),  
             hidden(
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
             )
      ),
      hidden(column(width=4,id=ns("ed_c2"),
                    h4("Assign sample types manually"),
                    pickerInput(ns("ed_sel_samples"), "Select columns for setting sample type", 
                                choices=NULL,  multiple=T, 
                                options = list(
                                  `live-search` = TRUE,
                                  `actions-box` = TRUE)),
                    sliderInput(ns("ed_number"), "Set to this sample type",min=1,max=1,value=1,step=1),
                    downloadBttn(ns("downloadeTable"),label = "Download table"),
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
          updateSelectInput(session, "dist_type", selected = "jaccard")
          updateSelectInput(session, "dist_thresh", selected = 0)
          updateSelectInput(session, "dist_type", selected = "jw")
          updatePickerInput(session, "ed_sel_samples", choices = cnames)
          updateSliderInput(session, "ed_number", max = length(cnames))
          shinyjs::show("dist_thresh")
          shinyjs::show("dist_type")
          shinyjs::show("ed_c3")
          shinyjs::show("ed_c2")
          
        }
      })
      
      
      # update threshold
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
            #print(th_vals)
            median_dist <- median(th_vals[th_vals != 0], na.rm = T)
            updateSliderInput(session, "dist_thresh", value=median_dist,
                              min=round(min(th_vals), digits=3), 
                              max=round(max(th_vals), digits=3), 
                              step = round(diff(range(th_vals)/100), digits=3)
            )
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
            expd_d <- expd_dist(colnames(tdesign),
                                method = input$dist_type,
                                p = 0.2
            ) # p=0.1 prioritizes the start of the strings
            median_dist <- input$dist_thresh
            # print(median_dist)
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
      
      
      # Manually change design
      observeEvent(input$ed_sel_samples, isolate({
        ted <- exp_design()
        input$ed_sel_samples
        if (length(input$ed_sel_samples > 0)) {
          ted[1, input$ed_sel_samples] <- input$ed_number
          idx <- (ted[1, ] == input$ed_number)
          if (length(idx) > 0) {
            ted[2, idx] <- 1:sum(idx)
          }
          exp_design(ted)
        }
      }))
      
      # Table for editing design
      output$etable <- DT::renderDT({
        if (!is.null(exp_design())) {
          print("edtable")
          show_table <- t(exp_design())
          show_table[is.na(show_table)] <- 1
          datatable(show_table, editable = T) %>%
            formatStyle("Group",
                        target = "row", backgroundColor =
                          styleEqual(unique(show_table[
                            ,
                            "Group"
                          ]), rainbow(length(unique(show_table[, "Group"])), alpha = 0.7))
            )
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
      
      
      ## Send further to next tab
      observeEvent(input$proceed_to_process, {
        print("send to processing")
        # reordering sample names for easier treatment
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
      observeEvent(input$h_proceed_expdesign, sendSweetAlert(session,
                                                             title = "Ready to proceed?",
                                                             text = HTML("<p align='justify'>In order to change to define the
    experimental design, you need to have selected
            an <i>ID</i> column and multiple numeric <i>quant</i> columns.</p>"),
                                                             type = "info", html = T
      ))
      
      observeEvent(input$h_exp_design, sendSweetAlert(session,
                                                      title = "Estimate design",
                                                      text = HTML("<p align='justify'><i>General: </i>We estimate the
    experimental design from the similarity
              between column names. Try the different string distances
              below and play with the threshold
              to find the optimal setting. <br/>
              You can further <i>modify</i> the experimental design by
              double clicking on the respective entry in
              the table.<br/>
              <i>Explanation of table: </i>Group denote the experimental
              condition, i.e. the group of sample
              of the same type like drug, disease or time points.
              Different conditions are given by different numbers.
              Replicates correspons to biological or technical samples
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

