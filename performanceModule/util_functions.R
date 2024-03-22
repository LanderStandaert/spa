
##########
# GENERAL#
##########

#define columns for rescaling
higher_columns = c("test_sit_reach","test_sprong","test_yoyo_el1","test_yoyo_el2")
lower_columns = c("test_10_m","test_30_m","test_t","test_5_10_5","test_dribbel_links","test_dribbel_rechts","test_coordination")


##########
# TRANS  #
##########
step_1_function = function(data){
  data %>% 
    filter(!is.na(achternaam)) %>% 
    mutate(name = paste(voornaam, achternaam)) %>% 
    select(-c(achternaam, voornaam)) %>% 
    mutate(current_age = 14)            # THIS NEEDS UPDATES
}

step_1_longer_function = function(data){
  data %>%
    pivot_longer(!c("name", "ploeg", "current_age", "datum"), names_to = "test_name", values_to = "test_score") %>%
    rename(names = name) %>% 
    group_by(test_name, ploeg) %>%
    mutate(missing_value = is.na(test_score)) %>% 
    mutate(test_score = replace_na(test_score, mean(test_score, na.rm = T))) 
  
}


##########
# COMP   #
##########

#function for comparative plots ----
comparative_plot = function(data, test_name, reverse = F, ploeg){
  
  #step 5: plot
  p = ggplot(data %>% filter(test_name == {{test_name}}) %>% filter(!missing_value) %>% filter(ploeg == {{ploeg}}), aes(x = names, y = test_score, label = names)) +
    geom_point(aes(colour = factor(cut_group), size = factor(cut_group))) +
    geom_hline(aes(yintercept= min_sd), linetype="dashed")+
    geom_hline(aes(yintercept= max_sd), linetype="dashed")+
    theme_minimal() +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank())+
    xlab("player names")+
    ylab("result")+
    geom_text_repel(aes(names, test_score, label = names))+
    scale_size_manual(values = c("0" = 3,"1" = 3, "2" = 3, "3" = 6, "4" = 3))  # Adjust the size values as needed
  
  #decide reverse
  if (!reverse) {
    p +
      scale_colour_manual(values = c("red","lightblue","green","orange"))
  }else{
    p +
      scale_y_reverse()+
      scale_colour_manual(values = c("green","lightblue","red","orange"))
  }
}


##########
# SPIDER #
##########

#define categories to summarise----
summarise_performance = function(rescaled_df){
  rescaled_df %>% 
    rowwise() %>% 
    mutate(lenigheid = test_sit_reach) %>% 
    mutate(control = mean(c(test_dribbel_links , test_dribbel_rechts), na.rm = T)) %>% 
    mutate(coordinatie = mean(c(test_t, test_5_10_5, test_coordination), na.rm = T)) %>% 
    mutate(sprongkracht = test_sprong) %>% 
    mutate(sprints = mean(c(test_10_m, test_30_m), na.rm =T)) %>% 
    mutate(stamina = mean(c(test_yoyo_el1 , test_yoyo_el2), na.rm = T)) %>% 
    select(names, ploeg,current_age, lenigheid, control, coordinatie, sprongkracht, sprints, stamina)
}

create_spider_data_ploeg = function(data, filter_value){
  data = data %>% 
    select(ploeg, names, test_name, test_score, current_age) %>% 
    pivot_wider(names_from = test_name, values_from = test_score) %>% 
    filter(ploeg == filter_value) %>% 
    group_by(ploeg) %>% 
    mutate(across(all_of(higher_columns), rescale)) %>% 
    mutate(across(all_of(lower_columns), function(x){1-x})) %>% 
    mutate(across(all_of(lower_columns), rescale)) %>% 
    summarise_performance() %>% 
    ungroup() %>% 
    column_to_rownames("names") %>% 
    select(-c(ploeg, current_age))
}

create_spider_data_age = function(data, filter_value){
  data = data %>% 
    select(ploeg, names, test_name, test_score, current_age) %>% 
    pivot_wider(names_from = test_name, values_from = test_score) %>% 
    filter(current_age == filter_value) %>% 
    group_by(current_age) %>% 
    mutate(across(all_of(higher_columns), rescale)) %>% 
    mutate(across(all_of(lower_columns), function(x){1-x})) %>% 
    mutate(across(all_of(lower_columns), rescale)) %>% 
    summarise_performance() %>% 
    ungroup() %>% 
    column_to_rownames("names") %>% 
    select(-c(ploeg, current_age))
}

#render_spider_plot(create_spider_data_ploeg(data, "U-10g"), "none","ploeg")
#render_spider_plot(create_spider_data_age(data, 14), "none","age")



render_spider_plot = function(analysis_df, selected_player,  title_category){
  col_min = rep(0,ncol(analysis_df))
  col_max = rep(1,ncol(analysis_df))
  col_mean = apply(analysis_df, 2, mean, na.rm = T)
  col_summary <- t(data.frame(Max = col_max, Min = col_min, Average = col_mean))
  
  final_df2 <- as.data.frame(rbind(col_summary, analysis_df))
  
  #select player
  #player_id = which(rownames(final_df2) == selected_player()$player_names)
  player_id = which(rownames(final_df2) == selected_player)
  
  #beautify
  create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                          vlabels = colnames(data), vlcex = 0.7,
                                          caxislabels = NULL, title = NULL, ...){
    radarchart(
      data, axistype = 1,
      # Customize the polygon
      pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
      # Customize the grid
      cglcol = "grey", cglty = 1, cglwd = 0.8,
      # Customize the axis
      axislabcol = "grey", 
      # Variable labels
      vlcex = vlcex, vlabels = vlabels,
      caxislabels = caxislabels, title = title, ...
    )
  }
  
  radarchart(
    final_df2[c(1:3, player_id), ],
    axistype = 1,
    pfcol = c("#99999980",NA),
    pcol= c(NA,2), plty = 1, plwd = 2,
    title = paste(title_category, row.names(final_df2)[player_id]),
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    axislabcol = "black",
    vlcex = 0.7,
    vlabels = colnames(final_df2),
    caxislabels = c(0,0.25,0.5,0.75,1)
  )
}


##########
# RANKING #
##########

create_ranking = function(data){
  #ranking 
  ranked_columns_lower = lapply(lower_columns, function(col) {
    rank_col <- replace_na(rank(-data[[col]], na.last = "keep"), 0)
  })
  
  ranked_columns_higher = lapply(higher_columns, function(col) {
    rank_col <- replace_na(rank(data[[col]], na.last = "keep"), 0)
  })
  
  #renaming
  names(ranked_columns_lower) = lower_columns
  names(ranked_columns_higher) = higher_columns
  ranked_columns = do.call(c, list(ranked_columns_lower, ranked_columns_higher))
  
  # Calculate composite ranks
  lenigheid <- rank(ranked_columns[["test_sit_reach"]], ties.method = "min")
  
  control <- rank(
    ranked_columns[["test_dribbel_links"]] +
      ranked_columns[["test_dribbel_rechts"]],
    ties.method = "min"
  )
  
  coordinatie <- rank(
    ranked_columns[["test_t"]] +
      ranked_columns[["test_5_10_5"]] +
      ranked_columns[["test_coordination"]],
    ties.method = "min"
  )
  
  sprongkracht <- rank(ranked_columns[["test_sprong"]], ties.method = "min")
  
  sprints <- rank(
    ranked_columns[["test_10_m"]] +
      ranked_columns[["test_30_m"]],
    ties.method = "min"
  )
  
  stamina <- rank(
    ranked_columns[["test_yoyo_el1"]] +
      ranked_columns[["test_yoyo_el2"]],
    ties.method = "min"
  )
  
  total_score = lenigheid + control + coordinatie + sprongkracht + sprints + stamina
  
  # Create the final data frame
  rank_test_df <- data.frame(
    name = data$name,
    lenigheid,
    control,
    coordinatie,
    sprongkracht,
    sprints,
    stamina,
    total_score
  )
}

##########
# YOYO #
##########

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
  sheet_data <- read_sheet("https://docs.google.com/spreadsheets/d/1jUQ5bJKca7Q8WP-VM8321aiQldpYg0a-8reHVaQrvnc/edit#gid=0", sheet = sheet_num, skip = 1, trim_ws = TRUE, col_types = "dccccccccccc")
  converted_data <- convert_yoyo(sheet_data)
  return(converted_data)
}


create_yoyo_visual = function(yoyo_df, year){
  
  #pallette
  my_palette <- c("#FF6F61",  # Red/Orange
                  "#91C369",  # Green
                  "#A07C52",  # Brown
                  "#1E90FF"   # Blue
  )
  
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
