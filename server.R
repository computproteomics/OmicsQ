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
      renderPrint(log_operations())
    ))
    
  })
  
  ###### General info about app ######
  observeEvent(input$h_about,{
      showModal(modalDialog(
          title = span(
              h3(strong("OmicsQ: Quantitative analysis of Omics data")),
              p(  strong("Version: "), app_version,br(),
                  strong("Features: "),
                  "This web application facilitates the processing of quantitative data from Omics type experiments. 
      It is furthermore an entrypoint for using the following tools:",
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
  
  
}
