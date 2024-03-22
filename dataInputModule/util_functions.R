summarise_input_files = function(data){
  data %>% 
    select(Dag,Naam, Duur, Gemiddelde.hartslag..hsm.,Maximale.hartslag..hsm.,Afstand..km.) %>% 
    group_by(Dag, Naam) %>% 
    filter(!is.na(Gemiddelde.hartslag..hsm.)) %>% 
    mutate(minutes = hour(hms(Duur))*60 + minute(hms(Duur))) %>%
    summarise(mean_heart_rate = round(sum(minutes*Gemiddelde.hartslag..hsm.)/sum(minutes)),
              max_heart_rate = round(sum(minutes*Maximale.hartslag..hsm.)/sum(minutes)),
              mean_distance = round(sum(Afstand..km.)),
              Duur = (sum(minutes)))%>%
    replace(is.na(.), 0)
}

get_invalid_files = function(data){
  if (sum(is.na(data$Gemiddelde.hartslag..hsm.))>0) {
    data %>% 
      filter(is.na(Gemiddelde.hartslag..hsm.)) %>% 
      summarise(invalid_files = unique(Naam))
  }
}


saveData <- function(sheet, new_data) {
  
  # Format date
  new_data$Dag <- lubridate::dmy(new_data$Dag)
  
  # Load original sheet
  base <- read_sheet(sheet)
  
  # Find unique data
  unique_data <- dplyr::anti_join(new_data, base)
  
  # Save unique data
  if (nrow(unique_data) > 0) {
    sheet_append(sheet, unique_data)
  } else {
    warning("No unique data to save.")
  }
}