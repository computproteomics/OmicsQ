#### Overall server for OmicsQ ###########
server <- function(input, output, session) {
  ## main data sets
  log_operations <- reactiveVal(list())
  
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
      title = span(h3(strong("OmicsQ: Quantitative analysis of Omics data"),
                      p("This app aims to facilitate the processing of quantitative data from Omics type experiments. 
It is furthermore an entrypoint for using Apps like PolySTest for statistical testing, VSClust for clustering and
ComplexBrowser for the investigation of the behavior of protein complexes."),br(), 
                  p(strong("More info: "), a("https://computproteomics.bmb.sdu.dk")), 
                p(strong("Source code:"), a("https://bitbucket.org/veitveit/omicsq")),
                      style = 'font-size:16px;color:#6cbabf;')),
    ))
    
  })
  
  
}
