library(googlesheets4)
library(tidyverse)

#pallette
my_palette <- c("#FF6F61",  # Red/Orange
                     "#91C369",  # Green
                     "#A07C52",  # Brown
                     "#1E90FF"   # Blue
)

#get different sheets
metadata_sheet = sheet_properties(SHEET_PERFORMANCE)
yoyo_sheets = metadata_sheet$name[str_detect(metadata_sheet$name, "yoyo")]
performance_sheets =metadata_sheet$name[!str_detect(metadata_sheet$name, "yoyo")]

#merge the names
performance_1 = read_sheet(SHEET_PERFORMANCE, sheet = performance_sheets[1], skip = 1, trim_ws = T, col_types = "ccccddddddddddd")
player_info  = read_sheet("https://docs.google.com/spreadsheets/d/1oR0wdr6y79_RRyhfM1WXtyb-M4bJlR59HkEVVoAvo-k/edit#gid=0", col_types = "ccT", range = "A:C")
fuzzyjoin::fuzzy_left_join(x = performance_1, y = player_info, match_fun = c())


#yoyo overview

  #functions
  convert_yoyo = function(yoyo_df){
    #convert yoyo
    yoyo_convertor = readRDS("yoyo_converter.rds")
    
    #create yoyo df
    yoyo_df = yoyo_df %>% 
      pivot_longer(cols = !c("datum", "achternaam", "voornaam", "ploeg"), values_to = "value", names_to = "test") %>% 
      filter(!is.na(value)) %>% 
      mutate(test_level = str_sub(test, 8,10))
    
    #merge to convert
    yoyo_basetable = merge(yoyo_convertor, yoyo_df, by.x = c("name","shuttel"), by.y = c("test_level","value")) %>% 
      select(-c(shuttel))
    
    return(yoyo_basetable)
  }
  read_and_convert_yoyo <- function(sheet_num) {
    sheet_data <- read_sheet(SHEET_PERFORMANCE, sheet = yoyo_sheets[sheet_num], skip = 1, trim_ws = TRUE, col_types = "dccccccccccc")
    converted_data <- convert_yoyo(sheet_data)
    return(converted_data)
  }
  
  sheet_data %>% 
    pivot_longer(cols = !c("datum", "achternaam", "voornaam", "ploeg"), values_to = "value", names_to = "test") %>% 
    filter(!is.na(value)) %>% 
    mutate(test_level = str_sub(test, 8,10))
  
  create_yoyo_visual = function(yoyo_df, year){
    #filter on year
    yoyo_df = yoyo_df %>% filter(datum == year)
    
    # Custom theme
    my_theme <- theme_minimal() +
      theme(axis.text.x = element_blank())  # Rotate x-axis labels for better readability
    
    # Plotting code
    ggplot(yoyo_df, aes(x = ploeg, y = distance, fill = name)) +
      geom_boxplot(alpha = 0.7, size = 0.5, outlier.size = 0.5) +
      facet_wrap(~ ploeg, scales = "free") +
      scale_fill_manual(values = my_palette) +  # Use the custom color palette
      labs(title = "Yoyo test",
           x = "Team",
           y = "Distance (meters)") +  # Add axis labels
      my_theme  # Apply the custom theme
  }
  create_yoyo_time_visual = function(yoyo_df, test_name){
    # Custom theme
    my_theme <- theme_minimal()
    
    # Plotting code
    ggplot(yoyo_df %>% filter(name == test_name), aes(y = datum, x = distance, fill = ploeg)) +
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
           y = "Datum") +  # Add axis labels
      my_theme  # Apply the custom theme
  }
  
  #Assuming yoyo_sheets is a list of sheet numbers or names
  yoyo_data_list <- lapply(1:length(yoyo_sheets), read_and_convert_yoyo)
  yoyo_df <- do.call(rbind, yoyo_data_list)
  
  
  #resutls
  yoyo_df = yoyo_df %>% 
    mutate(ploeg = factor(ploeg)) %>% 
    mutate(name = factor(name)) %>% 
    mutate(datum = factor(datum))%>%
    mutate(test = factor(test))
  
  #create visuals
  create_yoyo_visual(yoyo_df,2023)
  create_yoyo_time_visual(yoyo_df, "el2")
  
  yoyo_df%>%
    filter(ploeg == "U-16")%>%
    filter(name == "el2")%>%
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
  


