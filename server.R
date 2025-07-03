app_version <- readLines("VERSION")


#### Overall server for OmicsQ ###########
server <- function(input, output, session) {
  ## main data sets
  log_operations <- reactiveVal(list(omicsQ_version = app_version))
  
  ##### READING DATA
  dataInput <- dataInputServer(id="dataInput", parent=session, log_operations)
  
  ##### EXPERIMENTAL DESIGN
  expDesign <- expDesignServer(id="expDesign", parent=session, dataInput, log_operations)
  
  ##### PRE-PROCESSING
  preProcessing <- preProcessingServer("preProcessing", parent=session, expDesign, log_operations)
  
  ##### SEND TO APPS
  sendRetrieve <- sendRetrieveServer("sendRetrieve", preProcessing, log_operations)
  
  ###### Logging all operations ######
  observeEvent(input$h_log,{
    showModal(modalDialog(
      title = span(h3(strong("Summary of data transformations and used upload options"), style = 'font-size:16px;color:#6cbabf;')),
      renderPrint(log_operations()),
      downloadButton("download_log", "Download log as json file", class = "btn btn-primary")
      # br(),
      # p("To upload a log file and set all parameters to the values stored in the log, please use the button below."),
      # fileInput("upload_log", label = "Upload log file (json format)", 
      #           accept = c(".json"), 
      #           buttonLabel = "Browse...", 
      #           multiple = FALSE, 
      #           width = "100%")
      
    ))
    
  })
  
  ###### Download log file as json ######
  output$download_log <- downloadHandler(
    filename = function() {
      paste("OmicsQ_log_", Sys.Date(), ".json", sep = "")
    },
    content = function(file) {
      # Convert the log_operations to JSON format
      json_data <- jsonlite::toJSON(log_operations(), pretty = TRUE, auto_unbox = TRUE)
      writeLines(json_data, file)
    }
  )
  
  
  ###### When uploading log file, go back to reading file and set all parameters ######
  

  ###### Access to tutorial ######
  observeEvent(input$h_tutorial,{
      # Open tutorial in new tab
      runjs("window.open('tutorial/Tutorial.html', '_blank')")
  })
  
  
    
  ###### General info about app ######
  observeEvent(input$h_about,{
      showModal(modalDialog(
          title = span(
              h3(strong("OmicsQ: Quantitative analysis of Omics data")),br(),
              p(               strong("Features: "),
              span("This web application facilitates the processing of quantitative data from Omics type experiments. 
      It is furthermore an entrypoint for using the following tools:"),
              br(),
                  a("PolySTest", href = "https://computproteomics.bmb.sdu.dk/app_direct/PolySTest/", style = 'color:#6cbabf;'),
                  span(" for statistical testing"),
                  br(),
                  a("VSClust", href = "https://computproteomics.bmb.sdu.dk/app_direct/VSClust/", style = 'color:#6cbabf;'),
                  span(" for clustering, and"),
                  br(),
                  a("ComplexBrowser", href = "https://computproteomics.bmb.sdu.dk/app_direct/ComplexBrowser/", style = 'color:#6cbabf;'),
                  span(" for exploration of the behavior of protein complexes."),
                  br(),
              strong("Version: "), app_version,br(),
                  style = 'font-size:16px; color:#6c2a3f;'
              ),
              p(
                  strong("Source code:"),
                  a("https://github.com/computproteomics/OmicsQ", href = "https://github.com/computproteomics/OmicsQ", style = 'color:#6cbabf;'),
                  style = 'font-size:16px; color:#6c2a3f;'
              ),
              p(
                  strong("What does the app do?"),
                  style = 'font-size:16px; color:#6c2a3f;'
              ),
              tags$img(height = "1000px", src = "OmicsQWorkflow.svg"),
              br(),
              style = 'font-size:16px; color:#6c2a3f;'
          ),
          size = "xl",
          easyClose = TRUE
      ))
      
    
  })
  
  observeEvent(input$custom_bookmark, {
      session$doBookmark()
  })
  
  
  
}
