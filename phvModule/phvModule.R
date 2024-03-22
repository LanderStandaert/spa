source("./phvModule/util_functions.R")

phvModuleUI = function(id, ploeg, season){
  # Third tab content

          fluidRow(
            tabBox(
              id = "tabset2",
              width = 12,
              
              tabPanel("In-depth overview",
                       width = 12,
                       div(style="display: inline-block;vertical-align:top; min-width: 100px;",
                           selectInput(NS(id,"ploeg_aphv"), "Ploeg selectie",ploeg, selected = ploeg[[1]])),
                       div(style="display: inline-block;vertical-align:top; min-width: 200px;",
                           selectInput(NS(id,"season_aphv"), "Season selectie", season, selected = season[[length(var_season)]])),
                       DTOutput(NS(id,"APHV"))
              ),
              
              tabPanel("general overview",
                       width = 12,
                       DTOutput(NS(id,"global_aphv"))),
              
              tabPanel("diagnositcs",
                         box(
                           DTOutput(NS(id,"diagnostic_phv_table")), title = "welke PHV gegevens staan onder de foute naam", width = 3, solidHeader = TRUE, status = "primary"
                         )
                       )
              )
          )

}


phvModuleServer = function(id, google_sheet_hvp, google_sheet_player_info){
  moduleServer(id, function(input, output, session){
    
  # prepare phv data ----
  phv_clean = phv_clean(google_sheet_hvp)
    
  # prepare aphv data ----
  aphv_clean = reactive({
    
    #call data
    player_info = readRDS("SHEET_ID_PLAYER_INFO")
    phv = phv_clean
    
    #fuzzy join while ignore uppercases
    phv$naam = tolower(phv$naam)
    player_info$naam = tolower(paste(player_info$Voornaam, player_info$Naam))
    
      player_info = player_info %>% 
        drop_na(naam)
      global_phv = stringdist_inner_join(phv,player_info, by="naam") #uses a fuzzy inner join cuz im not trusting these kids
      
    #get current age
    global_phv$current_age = time_length((global_phv$timestamp - global_phv$geboortedatum),"year")
    
    #variables
    leg_length = global_phv$lengte - global_phv$zithoogte
    sitting_height = global_phv$zithoogte
    height = global_phv$lengte
    weight = global_phv$gewicht
    age = global_phv$current_age
    division = global_phv$ploeg
    
    #formulas
    aphv  = -9.376 + (0.0002708 * (leg_length * sitting_height)) + (-0.001663 * (age * leg_length)) + (0.007216 * (age * sitting_height)) + (0.02292 * ((weight/height) * 100))
    peak_age = global_phv$current_age-aphv
    
    #create summary df
    aphv_df = data.frame(global_phv$naam.x, peak_age, aphv,  division, global_phv$timestamp)
    names(aphv_df) = c("naam","peak_age","aphv","ploeg","timestamp")
    
    #get mean avhp per player
    aphv_df
  })
  
  # render APHV ----
  output$APHV = renderDT({
    
    #load data
    aphv_df = aphv_clean()
    
    #get filter
    selectie = input$ploeg_aphv
    season_input = as.integer(word(input$season_aphv, -1))
    
    
    #set filters
    aphv_df = aphv_df %>% 
      group_by(naam,ploeg) %>% 
      filter(ploeg == selectie) %>% 
      filter(between(as_date(timestamp), ymd(paste(season_input-1,"/06/01")),ymd(paste(season_input,"/05/30")))) %>% 
      select(-timestamp) %>% 
      summarise(peak_age = round(mean(peak_age),2),months_until_peak = round(last(-aphv*12)))
    
    #deploy scheme
    datatable(aphv_df, 
              options = 
                list(rowCallback = JS(rowCallback_aphv),
                     lengthMenu = c(20,40))
    )
  })
  
  # global aphv ----
  output$global_aphv = renderDT({
    
    #load data
    aphv_df = aphv_clean()
    
    aphv_df = aphv_df %>%
      group_by(naam) %>%
      arrange(timestamp, .by_group = T) %>% 
      summarise(months_until_peak = round(last(aphv*12)), ploeg = last(ploeg)) %>% 
      filter(abs(months_until_peak)<=3)
    
    #deploy scheme
    datatable(aphv_df, 
              options = 
                list(rowCallback = JS(rowCallback_aphv),
                     lengthMenu = c(20,40))
    )
  })
  

  #####################################
  #      DIAGNOSTICS                  #
  #####################################
  
  uncoupled_players = reactive({
    #call data
    player_info = read_sheet(SHEET_ID_PLAYER_INFO, sheet = "2023", range= "A:C",col_types = "ccD")
    phv = phv_clean 
    
    #fuzzy join while ignore uppercases
    phv$naam = tolower(phv$naam)
    player_info$naam = tolower(paste(player_info$Voornaam, player_info$Naam))
    
    player_info = player_info %>% 
      drop_na(naam)
    
    global_phv = stringdist_inner_join(phv,player_info, by="naam") #uses a fuzzy inner join cuz im not trusting these kids
    global_phv = global_phv %>%
      select(-c(Naam, Voornaam, lengte, zithoogte, gewicht))
    
    #welke mensen staan in phv maar zijn niet gekoppeld aan global
    
    #alle mensen global
    global_names = unique(global_phv$naam.x)
    
    #alle mensen in phv
    phv_names = unique(phv$naam)
    
    #difference
    return(tibble(names = setdiff(phv_names,global_names))) 
  })
  
  # DataTable Render ---
  output$diagnostic_phv_table= renderDT({
    datatable(uncoupled_players())
  })
  

})
}






