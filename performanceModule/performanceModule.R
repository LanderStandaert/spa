
performanceModuleUI = function(id, ploeg){
    tabBox(
      id = "tabset3",
      width = 12,
      tabPanel("team overview",
               fluidRow(
                 column(width = 3, plotOutput(NS(id, "spider_plot_ploeg")), solidHeader = TRUE),
                 column(width = 3, plotOutput(NS(id, "spider_plot_age")), solidHeader = TRUE),
                 column(width = 6,
                        fluidRow(
                          column(width = 8, DTOutput(NS(id, "ranking"))),
                          column(width = 2, selectInput(NS(id,"ploeg_performance"), "Ploeg", choices = ploeg)),
                          column(width = 2, selectInput(NS(id, 'select_player'), 'Speler', choices = NULL))
                        )
                 )
               )
               ,
               fluidRow(
                 box(
                   plotOutput(NS(id,"test_10_m")), title = "Test 10m", width = 3, solidHeader = TRUE, status = "primary"
                 ),
                 box(
                   plotOutput(NS(id,"test_30_m")), title = "Test 30m", width = 3, solidHeader = TRUE, status = "primary"
                 ),
                 box(
                   plotOutput(NS(id, "test_sit_reach")), title = "Test sit and reach", width = 3, solidHeader = TRUE, status = "info"
                 ),
                 box(
                   plotOutput(NS(id, "test_sprong")), title = "Test Sprong", width = 3, solidHeader = TRUE, status = "info"
                 )
               )
               ,
               
               fluidRow(
                 box(
                   plotOutput(NS(id, "test_t")), title = "Test T", width = 3, solidHeader = TRUE, status = "primary"
                 ),
                 box(
                   plotOutput(NS(id, "test_5_10_5")), title = "Test 5-10-5", width = 3, solidHeader = TRUE, status = "primary"
                 ),
                 box(
                   plotOutput(NS(id, "test_dribbel_links")), title = "Test dribbel links", width = 3, solidHeader = TRUE, status = "primary"
                 ),
                 box(
                   plotOutput(NS(id, "test_dribbel_rechts")), title = "Test dribbel rechts", width = 3, solidHeader = TRUE, status = "primary"
                 )
               )
      ),
      tabPanel("Yoyo",
               fluidRow(
                 column(width = 4, plotOutput(NS(id, "yoyo_global")), solidHeader = TRUE),
                 column(width = 4, plotOutput(NS(id, "yoyo_time")), solidHeader = TRUE),
                 )
               ,
               fluidRow(
                 column(width = 4, plotOutput(NS(id, "yoyo_specific")), solidHeader = TRUE),
                 column(width = 2, selectInput(NS(id,"ploeg_yoyo"), "Ploeg", choices = ploeg))
               )
      )
    )
}

performanceModuleServer = function(id, SHEET_PERFORMANCE){
  moduleServer(id, function(input, output, session){
      #read data 
      
      #df = read_xlsx("fysieke_testen.xlsx", col_types = c("text","text","text","text",rep("numeric",11)), skip = 1, trim_ws = T)
      df = SHEET_PERFORMANCE
      #df = readRDS("test.rds")
      
      ##prepare data
      step_1 = step_1_function(df)
      step_1_longer = step_1_longer_function(step_1)

      #reactive part
      step_4 <- reactive({
        req(input$select_player)
        highlight_name = input$select_player
        
        step_4 = step_1_longer %>% 
          mutate(
            min_sd = mean(test_score) - sd(test_score),
            max_sd = mean(test_score) + sd(test_score),
            cut_group = case_when(
              names == highlight_name ~ 3,
              test_score < min_sd ~ 0,
              test_score > max_sd ~ 2,
              TRUE ~ 1
            )
          )
        
        return(step_4)
      })
      
      
      #update players based on select ploeg
      ploeg_update = reactive({
        req(input$ploeg_performance)
        filter(step_1, ploeg == input$ploeg_performance)
      })
      
      observeEvent(ploeg_update(),{
        players = ploeg_update()$name
        updateSelectInput(session, inputId = 'select_player', choices = c(players))
      })
      
      #create ranking
      output$ranking = renderDT({
        data = step_1 %>% 
          filter(ploeg == input$ploeg_performance)
        data = create_ranking(data)
        datatable(data, options = list(pageLength = 5))
      })  
      
      
        #spider data 
        spider_data_ploeg = reactive({
          req(input$ploeg_performance)
          create_spider_data_ploeg(step_4() %>% ungroup(),input$ploeg_performance)
        })
        
        spider_data_age = reactive({
          req(input$select_player)
          
          age <- step_1 %>% 
            filter(name == input$select_player) %>% 
            select(current_age) %>% 
            as.numeric()
          
          create_spider_data_age(step_4() %>% ungroup(),age)
        })
        
        #spider plots
        output$spider_plot_ploeg = renderPlot({
          req(input$select_player)
          
          isolate({
            render_spider_plot(spider_data_ploeg(), input$select_player, "Compared to ploeg:")
          })
          
        })
        
        output$spider_plot_age = renderPlot({
          req(input$select_player)
          
          isolate({
            render_spider_plot(spider_data_age(), input$select_player, "Compared to age:")
          })
        })
      
 
      
      
      #test plots
      plot_names <- c("test_t", "test_5_10_5", "test_dribbel_links", "test_dribbel_rechts",
                      "test_10_m", "test_30_m", "test_sit_reach", "test_sprong")
      
      lapply(plot_names, function(plot_name) {
        output[[plot_name]] <- renderPlot({
          comparative_plot(step_4(), plot_name, 
                           plot_name %in% c("test_t", "test_5_10_5", "test_dribbel_links", "test_dribbel_rechts", "test_10_m","test_30_m"), #TRUE (so lower is better)
                           input$ploeg_performance)
        })
      })
      
      
      
      
      #yoyo tests
      yoyo_data = reactive({
        
        #Assuming yoyo_sheets is a list of sheet numbers or names
        yoyo_data_list <- lapply(c(2:3), read_and_convert_yoyo)
        yoyo_df <- do.call(rbind, yoyo_data_list)
        
        #resutls
        yoyo_df = yoyo_df %>% 
          mutate(ploeg = factor(ploeg)) %>% 
          mutate(name = factor(name)) %>% 
          mutate(datum = factor(datum))%>%
          mutate(test = factor(test))
      })
      
      output$yoyo_global = renderPlot({
        #create visuals
        create_yoyo_visual(yoyo_data(),2023)+
          ggtitle("Global yoyo results of 2023")
        
      })
      
      output$yoyo_time = renderPlot({
        create_yoyo_time_visual(yoyo_data(), "el2")+
          ggtitle("evolution of yoyo results over time")
      })
      
      yoyo_specific = reactive({
        yoyo_data() %>%
          filter(ploeg == input$ploeg_yoyo)%>%
          filter(name == "el2")
      })
      
      output$yoyo_specific = renderPlot({
        yoyo_specific()%>%
          mutate(datum = str_c(datum, "-",str_sub(test, 6,6)))%>%
          ggplot(aes(y = datum, x = distance, fill = ploeg)) +
          ggridges::geom_density_ridges(
            alpha = 0.7,  # Decrease the fill alpha to make the plot less transparent
            size = 0.7,   # Adjust the size of the lines to 0.7
            quantile_lines = TRUE,
            quantiles = 2
          ) +
          #scale_y_discrete(expand = c(0.001, 0.001)) +
          facet_wrap(~ ploeg, scales = "free") +
          labs(title = "Density Ridges Plot of Yo-Yo Test",
               x = "Distance (meters)",
               y = "Datum")  # Add axis labels
        
      })
      
  })
}