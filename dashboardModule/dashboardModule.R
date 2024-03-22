source("./dashboardModule/util_functions.R")

#original dashboard Module
if(F){
  
  dashboardModuleUI = function(id, ploeg){
    # Second tab content
    tabItem(tabName = "dashboard",
            #tabbox dashboard----
            fluidRow(
              
              tabBox(
                id = "tabset1",
                width = 12,
                
                #panel 1 ----
                tabPanel("Heart rates",
                         width = 12,
                         div(style="display: inline-block;vertical-align:top; min-width: 60px;",
                             dateRangeInput(NS(id,"inDateRange"), "Week selectie")),
                         div(style="display: inline-block;vertical-align:top; min-width: 20px;",
                             selectInput(NS(id,"ploeg_hrdb"), "Ploeg", choices = ploeg)),
                         DTOutput(NS(id,"dashboard_table")),
                         tags$head(
                           tags$style(
                             ".input-daterange input {
                      min-height: 34px;
                      }"
                           )
                         ),
                         img(src='hartslagzones.png', width = 240, height = 120)
                ),
                
                #panel 2 ----
                tabPanel("Time info",
                         width = 12,
                         DTOutput("timeinfo_table")),
                
                #panel 3 ----
                tabPanel("Missing Players",
                         width = 12,
                         DTOutput("missing_player_table")
                )
              )
              
            )
    )
  } 
}

#login dashboard module
dashboardModuleUI = function(id, ploeg){
  # Second tab content
          #tabbox dashboard----
          fluidRow(
            
            tabBox(
              id = "tabset1",
              width = 12,
              
              #panel 1 ----
              tabPanel("Heart rates",
                       width = 12,
                       div(style="display: inline-block;vertical-align:top; min-width: 60px;",
                           dateRangeInput(NS(id,"inDateRange"), "Week selectie")),
                       div(style="display: inline-block;vertical-align:top; min-width: 20px;",
                           selectInput(NS(id,"ploeg_hrdb"), "Ploeg", choices = ploeg)),
                       DTOutput(NS(id,"dashboard_table")),
                       tags$head(
                         tags$style(
                           ".input-daterange input {
                      min-height: 34px;
                      }"
                         )
                       ),
                       img(src='hartslagzones.png', width = 240, height = 120)
              ),
              
              #panel 2 ----
              tabPanel("Time info",
                       width = 12,
                       DTOutput(NS(id,"timeinfo_table"))),
              
              #panel 3 ----
              tabPanel("Missing Players",
                       width = 12,
                       DTOutput(NS(id,"missing_player_table"))),
              #panel 4 ----
              tabPanel("diagnostics",
                       box(
                         DTOutput(NS(id,"diagnostic_max_heart_rate_table")), title = "welke polar accounts zijn incorrect gekoppeld", width = 3, solidHeader = TRUE, status = "primary"
                       )
              )
          )
        )
}

dashboardModuleServer = function(id, google_sheet_heart_rates, google_sheet_MAX_heart_rates){
  moduleServer(id, function(input, output, session){
    
    #call data
    heart_rate = google_sheet_heart_rates
    max_heart_rate = google_sheet_MAX_heart_rates
    
    # prepare data ----
    global_heart_rate = reactive({
      
      #clean and select necessary
      heart_rate = heart_rate %>% 
        group_by(Naam, Dag) %>% 
        summarise(mean_heart_rate = round(mean(mean_heart_rate)))
      
      #tolower
      heart_rate$Naam = tolower(heart_rate$Naam)
      heart_rate$Naam = str_squish(heart_rate$Naam)
      max_heart_rate$Naam = tolower(max_heart_rate$name)
      
      #merge data and arrange dates
      global_heart_rate = merge(heart_rate, max_heart_rate, by= "Naam")
      global_heart_rate = arrange(global_heart_rate, Dag)
      
    })
    
    # DataTable Render ----
    output$dashboard_table <- renderDT({
      
      #call data
      global_heart_rate = global_heart_rate()
      
      #FILTER ON DATE----
      date_start <- as.Date(input$inDateRange[1])
      date_end <- as.Date(input$inDateRange[2])
      
      #filter on ploeg
      selectie = input$ploeg_hrdb
      
      #temporary disabled----
      if (T) {
        
        #if end is selected by user
        if (date_end > (date_start + 3) ) {
          
          #define date range
          begin = date_start
          end = date_end
          
          #apply filters
          global_heart_rate = global_heart_rate %>% 
            filter(Dag>=begin) %>% 
            filter(Dag<= end) %>% 
            filter(ploeg == selectie) %>% 
            mutate(Dag_complete = paste(wday(Dag,T,F),Dag)) %>% 
            arrange(Dag)
          
          #output plot
          datatable(
            pivot_wider(
              global_heart_rate, 
              id_cols = c('Naam',"ploeg","max_heart_rate"), 
              names_from = 'Dag_complete', 
              values_from = c('mean_heart_rate')
            ), 
            extensions = 'Buttons',
            options = 
              list(lengthMenu = c(20,40), 
                   rowCallback = JS(rowCallback_heart_rate_db),
                   dom = 'Bfrtip', buttons = I('colvis'))
          )
          
        }else{
          
          #define date range
          begin = date_start
          end = date_start + 3
          
          global_heart_rate = global_heart_rate %>% 
            filter(Dag>=begin) %>% 
            filter(Dag<= end) %>% 
            filter(ploeg == selectie) %>% 
            arrange(Dag)
          
          #output plot
          datatable(
            pivot_wider(
              global_heart_rate, 
              id_cols = c('Naam',"ploeg","max_heart_rate"), 
              names_from = 'Dag', 
              values_from = c('mean_heart_rate')
            ),
            extensions = 'Buttons',
            options = 
              list(lengthMenu = c(20,40), 
                   rowCallback = JS(rowCallback_heart_rate_db),
                   dom = 'Bfrtip', buttons = I('colvis'))
          )
        }
      }
    })
    
    
    # Date range input----
    #observe({
    #  date_start <- as.Date(input$inDateRange[1])
    #  date_end <- as.Date(input$inDateRange[2])
    #  
    #  #if end is selected by user
    #  if (date_end > (date_start +3) ) {
    #    updateDateRangeInput(session, "inDateRange",
    #                         label = paste("Date range label"),
    #                         start = date_start,
    #                         end = date_end
    #    )
    #    
    #  }else{
    #    updateDateRangeInput(session, "inDateRange",
    #                         label = paste("Date range label"),
    #                         start = date_start,
    #                         end = date_start+3
    #    )
    #  }
    #})
    
    #####################################
    #      TIME TABLE DATABASE          #
    #####################################
    
    #prepare data----
    timetable_df = reactive({
      
      #filters
      selectie = input$ploeg_hrdb
      date_start <- as.Date(input$inDateRange[1])
      date_end <- as.Date(input$inDateRange[2])
      
      #clean and select necessary
      heart_rate = heart_rate %>% 
        group_by(Naam, Dag) %>% 
        summarise(Duur = round((Duur)))
      
      #tolower
      heart_rate$Naam = tolower(heart_rate$Naam)
      heart_rate$Naam = str_squish(heart_rate$Naam)
      max_heart_rate$Naam = tolower(max_heart_rate$name)
      
      #merge data and arrange dates
      global_heart_rate = merge(heart_rate, max_heart_rate, by= "Naam")
      global_heart_rate = arrange(global_heart_rate, Dag) %>%
        select(Naam, Dag, Duur, ploeg)
      
      #apply selectie
      global_heart_rate_filtered = global_heart_rate %>% 
        filter(ploeg == selectie) %>% 
        filter(Dag>=date_start) %>% 
        filter(Dag<= date_end) %>%
        mutate(Dag_complete = paste(wday(Dag,T,F),Dag))
      
      #pivot
      pivot_wider(
        global_heart_rate_filtered, 
        id_cols = c('Naam'), 
        names_from = 'Dag_complete', 
        values_from = c('Duur')
      )
    })
    
    #render DT----
    output$timeinfo_table = renderDT({
      datatable(timetable_df())
    })
    
    
    
    #####################################
    #      MISSING PLAYERS          #
    #####################################
    
    # prepare  data----
    missing_players = reactive({
      hr_db = heart_rate
      lookup = max_heart_rate[,1:3]
      
      log_names = unique(str_squish(tolower(hr_db$Naam)))
      lookup_names = unique(str_squish(tolower(lookup$name)))
      
      no_results = data.frame(missing_players = na.omit(setdiff(lookup_names, log_names)))
      
      lookup %>%
        filter(str_squish(tolower(name)) %in% no_results$missing_players)
    })
    
    # apply filters ----
    missing_players_week = reactive({
      
      #FILTER ON DATE----
      date_start <- as.Date(input$inDateRange[1])
      date_end <- as.Date(input$inDateRange[2])
      
      #filter on ploeg
      selectie = input$ploeg_hrdb
      
      #get data from week
      global_heart_rate = global_heart_rate() %>% 
        filter(Dag>= date_start) %>% 
        filter(Dag<= date_end) %>% 
        filter(ploeg == selectie) %>% 
        summarise(Naam = unique(Naam))
      
      #get total from selectie
      total_heart_rate = global_heart_rate() %>%
        filter(ploeg == selectie) %>%
        count(Naam)
      
      #get total missing players
      
      total_missing_playes = missing_players() %>% 
        filter(ploeg == selectie) %>% 
        count(name) %>%
        mutate(n = n-1)%>%
        rename(Naam = name)
      
      #rbind selectie en total misisng
      total_heart_rate = rbind(total_heart_rate, total_missing_playes)
      
      #display missing
      missing_players = setdiff(total_heart_rate %>% select(Naam),global_heart_rate)
      overview_missing = total_heart_rate %>%
        filter(Naam %in% missing_players$Naam)%>%
        rename(total_inputs = n)
      
    })
    
    # DataTable Render ---
    output$missing_player_table= renderDT({
      datatable(missing_players_week())
    })
    
    #####################################
    #      DIAGNOSTICS                  #
    #####################################
    
    #HEART RATE
      #welke mensen staan in heart rate maar zijn niet gekoppeld aan global
      
      uncoupled_players = reactive({
        #clean and select necessary
        heart_rate = heart_rate %>% 
          group_by(Naam, Dag) %>% 
          summarise(mean_heart_rate = round(mean(mean_heart_rate)))
        
        #tolower
        heart_rate$Naam = tolower(heart_rate$Naam)
        heart_rate$Naam = str_squish(heart_rate$Naam)
        max_heart_rate$Naam = tolower(max_heart_rate$name)
        
        #merge data and arrange dates
        global_heart_rate = merge(heart_rate, max_heart_rate, by= "Naam")
        
        #welke mensen staan in heart rate maar zijn niet gekoppeld aan global
        
        #alle mensen global
        global_names = unique(global_heart_rate$Naam)
        
        #alle mensen in polar
        polar_names = unique(heart_rate$Naam)
        
        #difference
        return(tibble(names = setdiff(polar_names,global_names)))
      })
    
      # DataTable Render ---
      output$diagnostic_max_heart_rate_table= renderDT({
        datatable(uncoupled_players())
      })

  })
}