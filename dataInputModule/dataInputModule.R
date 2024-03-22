
source("./dataInputModule/util_functions.R")

#original ui
if(F){
  dataInputModuleUI = function(id){
  tabItem(tabName = "input")
}
}

dataInputModuleUI = function(id){
}

dataInputModuleServer = function(id, google_sheet){
  moduleServer(id, function(input, output, session){
    
    #read from multiple files and combine each file
    input_file <- reactive({
      validate(need(input$files != "", "select files..."))
      
      if (is.null(input$files)) {
        return(NULL)
      } else {
        
        path_list <- as.list(input$files$datapath)
        tbl_list <- lapply(input$files$datapath, read.csv, header=TRUE, sep=",")
        
        df <- data.frame(do.call(rbind, tbl_list))
      }
    })
    
    
    #format the input files
    summarised_input_files = reactive({
      summarise_input_files(input_file())
    })
  
    #get invalid files
    invalid_files = reactive({
      get_invalid_files(input_file())
    })
    
    
    # save files
    observeEvent(input$files,{ 
      saveData(sheet = google_sheet,
      summarised_input_files())
      })
  })
}


