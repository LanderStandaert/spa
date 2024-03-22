#this loads the PHV function from util_functions (makes me wonder if i should just make 1 general util file?)
source("./phvModule/util_functions.R")


healthOverviewModuleUI = function(id, var_ploeg, var_month, var_year){
  # Fourth tab content
  tabItem(tabName = "health_database",
          fluidRow(
            box(
              width = 12,
              div(style="display: inline-block;vertical-align:top; min-width: 100px;",
                  selectInput(NS(id,"ploeg_health"), "Ploeg selectie",var_ploeg, selected = var_ploeg[[1]])),
              div(style="display: inline-block;vertical-align:top; min-width: 200px;",
                  selectInput(NS(id,"month_health"), "Maand selectie", var_month, selected = var_month[2])),
              div(style="display: inline-block;vertical-align:top; min-width: 200px;",
                  selectInput(NS(id,"year_health"), "Jaar selectie", var_year, selected = var_year[2])),
              DTOutput(NS(id,"health_database"))
            )
          )
  )
}

healthOverviewModuleServer = function(id, google_sheet_hvp){
  moduleServer(id, function(input, output, session){
    
    global_phv = phv_clean(google_sheet_hvp)
    
    # Combine the selected variables into a new data frame----
    selectedData <- reactive({
      
      #filters from input
      maand = match(input$month_health, month.name)
      selectie = input$ploeg_health
      jaar = input$year_health
      
      #sort by date
      global_phv = arrange(global_phv, timestamp)
      
      #apply filters
      global_phv = global_phv %>% 
        group_by(naam = tolower(naam)) %>% 
        mutate(groei_cm = lengte - lag(lengte))%>% 
        filter(month(timestamp) == maand)%>% 
        filter(year(timestamp) == jaar) %>% 
        filter(ploeg == selectie)
      
      return(global_phv)
    })
    
    output$health_database = renderDT({
      
      #deploy scheme
      datatable(selectedData(), 
                options = 
                  list(lengthMenu = c(20,40))
      )
    })
    
  })
}
  