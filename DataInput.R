#################### UI ##################
dataInputUI <- function(id, prefix="") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             h3("File input"),
             fluidRow(column(6,fileInput(ns("pfile"), label = "Data table"),
                             actionLink(ns("run_example"), "Run example file"),
                             p("Note that this work is still under development. For feedback and bugs,
                                        please write the author: veits@bmb.sdu.dk")),
                      column(6,actionBttn(ns("h_pfile"),
                                          icon=icon("info-circle"),
                                          style="pill",
                                          color = "royal", size = "xs"))
             )),
      hidden(column(3,id=ns("in_c1"),
                    h4("File specific settings"),
                    fluidRow(column(10,h5("(modify if necessary)")),
                             column(2, actionBttn(ns("h_csv_input"),
                                                  icon=icon("info-circle"),
                                                  style="pill",
                                                  color = "royal", size = "xs")
                             )),
                    uiOutput(ns("file_options")),
                    style = 'border-left: 1px solid'
      )),
      hidden(column(3,id=ns("in_c2"),
                    h4("Select and adjust"),
                    fluidRow(column(10,p("Select id and data columns:"),
                                    pickerInput(ns("sel_icol"), "Select ID column",
                                                choices=NULL,  multiple=F,
                                                options = list(
                                                  `live-search` = TRUE,
                                                  `actions-box` = TRUE)),
                                    pickerInput(ns("sel_qcols"), "Select quantitative columns",
                                                choices=NULL,  multiple=T,
                                                options = list(
                                                  `live-search` = TRUE,
                                                  `actions-box` = TRUE)),
                    ),
                    column(2, actionBttn(ns("h_sel_id_col"),
                                         icon=icon("info-circle"),
                                         style="pill",
                                         color = "royal", size = "xs")
                    )),
                    hr(),
                    fluidRow(column(10,p("Simple manipulations and corrections:"),
                                    actionButton(ns("remove_zeroes"), label="Zeroes to missing values"),
                                    actionButton(ns("remove_char"), label="Non-numeric to missing values"),
                    ),column(2, actionBttn(ns("h_remove_zeroes"),
                                           icon=icon("info-circle"),
                                           style="pill",
                                           color = "royal", size = "xs")
                    )),
                    style = 'border-left: 1px solid'
      )),
      hidden(column(3,id=ns("in_c3"),
                    h4("Proceed to experimental design"),
                    fluidRow(column(10,textOutput(ns("txt_proceed_expdesign"),
                    ),column(2, actionBttn(ns("h_proceed_expdesign"),
                                           icon=icon("info-circle"),
                                           style="pill",
                                           color = "royal", size = "xs")
                    )),
                    disabled(actionButton(ns("proceed_to_expdesign"), "Proceed")),
                    style = 'border-left: 1px solid'
                    ))
      )
    ),
    hr(),
    fluidRow(
      DTOutput(ns('ptable'))
    )
  )

}

###################### Server #####################
dataInputServer <- function(id, parent, log_operations) {
  moduleServer(
    id,
    function(input, output, session) {
      ##### READING DATA
      ##### ######################################################### reading
      ##### input data
      indata <- reactiveVal(NULL)
      next_tab <- reactiveVal(NULL)
      exp_design <- reactiveVal(NULL)

      observe({
        tdata <- NULL
        input$in_sheet
        input$in_delimiter
        input$in_dec
        input$in_skip
        input$in_header

        # Read file
        print("Reading file")
        in_file <- input$pfile
        isolate({
          if (is.null(in_file)) {
            return(NULL)
          }
          tlog <- log_operations()
          tlog[["file_name"]] <- in_file$name

          if (tools::file_ext(in_file$datapath) %in% c("xls", "xlsx", "XLS", "XLSX")) {
            # Set Options for file input
            print(input$in_sheet)
            currsheet <- ifelse(is.null(input$in_sheet), 1, input$in_sheet)

            tdata <- try({
              sheets <- excel_sheets(in_file$datapath)
              read_excel(in_file$datapath, sheet = currsheet)
            })

            shinyjs::show(id = "in_c1")
            output$file_options <- renderUI({
              input$in_sheet
              selectInput(session$ns("in_sheet"), "Which table sheet?", choices = sheets, selected = currsheet)
            })

            # file options
            tlog[["file_type"]] <- "excel"
            tlog[["file_options"]] <- c("sheet" = currsheet)
          } else {
            # file options
            currdel <- ifelse(is.null(input$in_delimiter), "auto", input$in_delimiter)
            currdec <- ifelse(is.null(input$in_dec), ".", input$in_dec)
            currskip <- ifelse(is.null(input$in_skip), 0, input$in_skip)
            currheader <- ifelse(is.null(input$in_header), T, input$in_header)

            tdata <- (try(fread(in_file$datapath,
                                sep = currdel, skip = currskip,
                                header = currheader, dec = currdec, fill = TRUE
            )))
            shinyjs::show(id = "in_c1")
            output$file_options <- renderUI({
              tagList(selectInput(session$ns("in_delimiter"), label = "delimiter", choices = c(
                auto = "auto",
                comma = ",", semicolon = ";", tabulator = "\t", colon = ":", bar = "|",
                space = " "
              ), selected = currdel, multiple = FALSE, ), selectInput("in_dec",
                                                                      label = "decimal separator", choices = c(comma = ",", point = "."),
                                                                      selected = currdec
              ), numericInput(session$ns("in_skip"),
                              label = "remove lines at beginning?",
                              min = 0, max = 100, step = 1, value = currskip
              ), checkboxInput(session$ns("in_header"),
                               label = "Does file have a header?", value = currheader
              ))
            })
            tlog[["file_type"]] <- "csv"
            tlog[["file_options"]] <- c(
              "delimiter" = currdel, "decimal" = currdec,
              "skip" = currskip, "header" = currheader
            )
          }

          log_operations(tlog)

          shinyjs::show(id = "in_c2")
          shinyjs::show(id = "in_c3")
          updatePickerInput(session, "sel_icol", choices = names(tdata))
          updatePickerInput(session, "sel_qcols", choices = names(tdata))

          ## feedbeck via moda
          log_upload <- as.character(geterrmessage())
          if (inherits(tdata, "try-error")) {
            print("error reading file")
            showModal(modalDialog(
              title = "file upload", log_upload, size = "s",
              easyClose = T
            ))
          } else {
            # set id column
            tdata <- data.frame(tdata)
            class(tdata[, 1]) <- "id"
            indata(tdata)
          }
        })
      })

      ### Read example file and push through
      observeEvent(input$run_example, ({
        tdata <- read.csv("Myo_Res.csv")
        class(tdata[, 1]) <- "id"
        for (i in 2:19) class(tdata[, i]) <- "quant"
        indata(tdata)
        shinyjs::show(id = "in_c3")
        shinyjs::enable(id="proceed_to_expdesign")
        js$run_button(button = session$ns("proceed_to_expdesign"), number = 1)
        tdata2 <- read.csv("Myo.csv", row.names = 1)
        # result_table(cbind(tdata, tdata))
        print("example file read")
        tlog <- log_operations()
        tlog[["file_name"]] <- "Example file"
        tlog[["file_type"]] <- "csv"
        tlog[["file_options"]] <- c(
          "delimiter" = "auto", "decimal" = ".",
          "skip" = 0, "header" = TRUE
        )
        tlog[["file_idcol"]] <- names(tdata)[1]
        tlog[["file_qcols"]] <- names(tdata)[2:19]
        log_operations(tlog)
      }))

      #### Input data data table
      output$ptable <- DT::renderDT({
        print("dttable")
        show_table <- indata()
        if (!is.null(show_table)) {
          ## create header and footer of table

          header.style <- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #008080;}"
          # pull header names from the table
          header.names <- c("Columns", colnames(show_table))
          header.classes <- c("Type", sapply(show_table, class))
          header.counts <- c("Duplicated values", sapply(show_table, function(x) {
            sum(duplicated(x,
                           incomparables = NA
            ))
          }))
          # The container parameter allows us to design the header of the
          # table using CSS
          column_cols <- c(
            numeric = "#808000", character = "#008080", id = "#800080",
            quant = "#33AA33", factor = "#008080", logical = "#008080"
          )
          datfile_container <- withTags(table(
            style(type = "text/css", header.style),
            thead(
              tr(lapply(header.names, th, style = "text-align: center; border-right-width: 1px;
                 border-right-style: solid; border-right-color: white; border-bottom-width: 1px;
                 border-bottom-style: solid; border-bottom-color: white")),
              tr(lapply(header.classes, function(x) {
                th(x, style = paste0(
                  "text-align: center; border-right-width: 1px;
                                 border-right-style: solid; background-color: ",
                  column_cols[x], "; border-right-color: white; border-bottom-width: 1px;
                                border-bottom-style: solid; border-bottaom-color: white"
                ))
              })),
              tr(lapply(header.counts, function(x) {
                th(x, style = paste0(
                  "text-align: center; border-right-width: 1px;
                                 border-right-style: solid; background-color: ",
                  ifelse(x > 0, "#800000", "#008000"), "; border-right-color: white; border-bottom-width: 1px;
                                border-bottom-style: solid; border-bottom-color: white"
                ))
              }))
            )
          ))
          datatable(show_table, container = datfile_container, options = list(scrollX = TRUE))
        } else {
          NULL
        }
      })

      ## select column with ids
      observeEvent(input$sel_icol, {
        isolate({
          get_cols <- make.names(input$sel_icol)[1]
          if (!is.null(get_cols)) {
            print("select id columns")
            tdata <- data.frame(indata())
            # remove id class from all
            for (col in colnames(tdata)) {
              if (class(tdata[, col]) == "id") {
                tdata[, col] <- unclass(tdata[, col])
              }
            }
            class(tdata[, get_cols]) <- "id"
            indata(tdata)
            tlog <- log_operations()
            tlog[["file_idcol"]] <- get_cols
            log_operations(tlog)
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
              if (class(tdata[, col]) == "quant") {
                tdata[, col] <- unclass(tdata[, col])
              }
            }
            for (col in get_cols) {
              if (class(tdata[, col]) == "numeric" || class(tdata[, col]) ==
                  "integer") {
                class(tdata[, col]) <- "quant"
                tlog <- log_operations()
                tlog[["file_qcols"]] <- get_cols
                log_operations(tlog)
              }
            }

            # Control button
            if (sum(sapply(tdata, class) == "quant") > 0) {
              enable("proceed_to_expdesign")
              output$proceed_to_expdesign <- renderText("Ready to go")
            } else {
              disable("proceed_to_expdesign")
              output$proceed_to_expdesign <- renderText("You need to select at least
          one column with quantified features.
          This column needs to the \"numeric\".")
            }

            indata(tdata)
          }
        })
      })

      ## Manipulate input table
      observeEvent(input$remove_zeroes, {
        isolate({
          get_cols <- make.names(input$sel_qcols)
          print(input$sel_col)
          if (!is.null(get_cols)) {
            print("removing zeroes")
            tdata <- data.frame(indata())
            for (col in get_cols) {
              tdata[, col] <- replace(tdata[, col], tdata[,col] == 0, NA)
            }
            indata(tdata)
            tlog <- log_operations()
            tlog[["preprocess_remove_zeroes"]] <- TRUE
            log_operations(tlog)
          }
        })
      })

      ## Make characters NA
      observeEvent(input$remove_char, {
        isolate({
          get_cols <- make.names(input$sel_qcols)
          if (!is.null(get_cols)) {
            print("removing chars")
            tclasses <- sapply(indata(), class)
            tdata <- data.frame(indata())
            for (col in get_cols) {
              tdata[, col] <- as.numeric(tdata[, col])
              class(tdata[, col]) <- tclasses[col]
            }
            tlog <- log_operations()
            tlog[["preprocess_remove_char"]] <- TRUE
            log_operations(tlog)
            indata(tdata)
          }
        })
      })

      ## Send further to next tab
      observeEvent(input$proceed_to_expdesign, isolate({
        print("proceed to expdesign")
        updateTabsetPanel(parent, "mainpage", selected = "exp_design")
        cnames <- colnames(indata())[sapply(indata(), class) == "quant"]
        ted <- rbind(rep(NA, length(cnames)), NA)
        colnames(ted) <- cnames
        rownames(ted) <- c("Group", "Replicate")
        print(ted)
        exp_design(ted)
        
        if (!is.null(next_tab())) {
          next_tab(paste0(next_tab(), "_new"))
        } else {
          next_tab("ready")
        }

      })
      )

      ############### Help messages
      observeEvent(input$h_pfile, sendSweetAlert(session,
                                                 title = "File types", text = HTML("<p align='justify'>OmicsQ can read
    <i>Excel</i> files, and tables in <i>textual format</i> like csv, tsv and others.<br/>
    The underlying function tries to automatically determine delimiters
    and digit separators.</p>"),
                                                 type = "info", html = T
      ))

      observeEvent(input$h_csv_input, sendSweetAlert(session,
                                                     title = "Options of textual input file",
                                                     text = HTML("<p align='justify'><i>Delimiter:</i> Specify the character
    that separates the values. Change only if 'auto' does not
    provide the correct table.<br/>
              <i>Decimal separator:</i> Specify the character to
              denote decimals.<br/>
              <i>Remove lines at beginning:</i> Sometimes, a textual format
              has a header spanning more than one line. You have the option to ignore
              a number of lines at the start of the files. This information
              will be lost.<br/>
              <i>Does file have a header: </i>In case that the file does not
              have a first row with information about the data columns, deselect this option.</p>"),
                                                     type = "info", html = T
      ))

      observeEvent(input$h_sel_id_col, sendSweetAlert(session,
                                                      title = "Select relevant columns",
                                                      text = HTML("<p align='justify'>Select the columns you want to make the ID
    column and the quantitative columns. You can search for multiple colums.
              This is particularly useful when your quantitative columns have
              similar columns names.<br/><i>ID column:</i> Select column with
              the main features. These can be e.g. gene ids, protein ids, or
              peptide sequences.
              They do not need to be unique as we offer summarization in the
              following analysis. The main analysis will take place on a
              unique set of IDs.<br/>
              <i>Quantitative columns: </i> These are the columns with values
              we will use in the analyses. They usually are quantifications
              of the features
              in the ID column (e.g. protein abundances or gene expressions).<br>
              ID and quant columns are marked in the table below according
              to your selection. You need to select them
              sto proceed to the next analysis step.</p>"),
                                                      type = "info", html = T
      ))

      observeEvent(input$h_remove_zeroes, sendSweetAlert(session,
                                                         title = "Simple data manipulation",
                                                         text = HTML("<p align='justify'><i>Zeroes to missing values:
    </i>Missed measurements are often given by
              zeroes. As the actual value of the features is not known,
              we make them missing values
              (NA or not available in R)<br/>
              <i>Non-numeric to missing values: </i>Data manipulations
              with software like Excel can lead to values like
              '#DIV/0!'. This buttons converts any non-numeric value
              into a missing values. Only purely numeric <i>quant</i> columns
              are accepted to continue the analysis.</p>"),
                                                         type = "info", html = T
      ))


      ### return info for using in expDesign
      return(list(
        next_tab = next_tab,
        exp_design = exp_design,
        indata = indata
      ))


    }
  )
}

