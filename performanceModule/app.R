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

source("./performanceModule.R")
source("./util_functions.R")

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
        tabItems(
          tabItem("performance_tab", uiOutput("performanceRender")
        )
      )
    )
    )
  
  server <- function(input, output, session) {
    if(T){
      output$performanceRender <- renderUI({
        performanceModuleUI("performance_id",c("U-10g",
                                               "U-10w",
                                               "U-11g",
                                               "U-11w",
                                               "U-11z",
                                               "U-12g",
                                               "U-12w",
                                               "U-13g",
                                               "U-13w",
                                               "U-13z",
                                               "U-14",
                                               "U-15w",
                                               "U-15g",
                                               "U-16",
                                               "U-17w",
                                               "U-17g",
                                               "Postformatie"))
      })
      performanceModuleServer("performance_id", SHEET_PERFORMANCE)
    }
    
  }
  shinyApp(ui, server)  
}

app()
