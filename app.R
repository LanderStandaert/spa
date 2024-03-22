renv::isolate()

library(fresh)
library(shinydashboard)
library(waiter)

library(googlesheets4)
library(rsconnect)
library(gargle)

library(DT)
library(dplyr, warn.conflicts = TRUE)
library(tidyr)
library(stringr)
library(ggplot2)
library(googledrive)
library(lubridate)
library(ggrepel)
library(textshape)
library(scales)
library(fmsb)
library(fuzzyjoin)

# Google sheets authentification -----
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "landerstandaert8@gmail.com")
gs4_auth(token = drive_token())


source("./loginModule/util_functions.R")
source("./performanceModule/util_functions.R")

source("./clubSpecific/Lede.R")
source("./dashboardModule/dashboardModule.R")
source("./dataInputModule/dataInputModule.R")
source("./performanceModule/performanceModule.R")
source("./phvModule/phvModule.R")
source("./healthOverviewModule/healthOverviewModule.R")

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#354F6B"
  )
)

app <- function() {
  
  ui = 
    dashboardPage(
      dashboardHeader(title = tags$a(tags$img(src='logo4.png', width = 200, height = 35)),
                      tags$li(
                        class = "dropdown",
                        style = "padding: 8px;",
                        shinyauthr::logoutUI("logout")
                      )), #200 -60
      
      dashboardSidebar(
        sidebarMenu(
          menuItem("dashboard", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Input", tabName = "input", icon = icon("keyboard"),
                   tabPanel("Upload",
                            fileInput(NS("input_id", "files"),
                                      "Upload",
                                      multiple = TRUE,
                                      accept = c(".xls", ".csv" , ".xlsx")
                            )
                   ),
                   tabPanel("Invalid files",
                            tableOutput(NS("input_id", "invalid_files"))
                   )
          ),
          menuItem("Performance", tabName = "performance_tab", icon = icon("bullseye")),
          menuItem("PHV", tabName = "aphv", icon = icon("notes-medical")),
          menuItem("Health Database", tabName = "health_database", icon = icon("th"))
        )
      ),
      dashboardBody(
        use_theme(mytheme),
        useWaitress(),
        
        shinyauthr::loginUI(
          "login"
          #, 
          #additional_ui = tagList(
          #  tags$p("test the different outputs from the sample logins below
          # as well as an invalid login attempt.", class = "text-center"),
          #  HTML(knitr::kable(user_base[,1:2], format = "html", table.attr = "style='width:100%;'"))
          #)
        ),
        
        tabItems(
          tabItem("dashboard", uiOutput("dashboardRender")),
          tabItem("performance_tab", uiOutput("performanceRender")),
          tabItem("input", uiOutput("dataInputRender")),
          tabItem("aphv",uiOutput("phvRender")),
          tabItem("health_database",uiOutput("healthDatabaseRender"))
        )
      )
    )
  
  server <- function(input, output, session) {
    
    # call the waitress
    waitress <- Waitress$
      new(theme = "overlay-percent")$
      start() # start
    
    
    # call login module supplying data frame, user and password cols and reactive trigger
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = user_base,
      user_col = user,
      pwd_col = password_hash,
      sodium_hashed = TRUE,
      log_out = reactive(logout_init())
    )
    
    # call the logout module with reactive trigger to hide/show
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
    
    user_info <- reactive({
      credentials()$info
    })
    
    
    #DASHBOARD ----
    waitress$inc(20)
    if(T){
      output$dashboardRender <- renderUI({
        req(credentials()$user_auth)
        dashboardModuleUI("dashboard_id",get_ploeg(user_info()$permissions))
      })
      dashboardModuleServer("dashboard_id",SHEET_ID_HR_DB, SHEET_ID_MAX_HR)
    }
    
    #DATA INPUT ----
    waitress$inc(20)
    if(T){
      output$dataInputRender <- renderUI({
        req(credentials()$user_auth)
        dataInputModuleUI("input_id")
      })
      dataInputModuleServer("input_id",SHEET_ID_HR_DB)
    }
    
    #PERFORMANCE ----
    waitress$inc(20)
    if(T){
      output$performanceRender <- renderUI({
        req(credentials()$user_auth)
        performanceModuleUI("performance_id",get_ploeg(user_info()$permissions))
      })
      performanceModuleServer("performance_id", SHEET_PERFORMANCE)
    }
    
    #PHV ----
    waitress$inc(20)
    if(T){
      output$phvRender <- renderUI({
        req(credentials()$user_auth)
        phvModuleUI("aphv_id",get_ploeg(user_info()$permissions), var_season)
      })
      phvModuleServer("aphv_id",SHEET_ID_HVP, SHEET_ID_PLAYER_INFO)
    }
    
    #HEALTH DATABASE
    waitress$inc(20)
    if(T){
      output$healthDatabaseRender <- renderUI({
        req(credentials()$user_auth)
        healthOverviewModuleUI("health_database_id",get_ploeg(user_info()$permissions), var_month, var_year)
      })
      healthOverviewModuleServer("health_database_id",SHEET_ID_HVP)
    }
    
    
    # hide when it's done
    waitress$close() 
    
  }
  shinyApp(ui, server)  
}

app()
