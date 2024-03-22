#colour DT tabel: APHV----
rowCallback_aphv <- c(
  "function(row, dat, displayNum, index){",
  "    for(var j=4; j<dat.length; j++){",
  "      var x = dat[j];",
  "      var color = x == null ? 'white' : (x) >= 10 ? 'lightblue' : (x) < -3 ? 'lightgreen' : 
                               (x) >= 5 ? 'orange': (x) < 5 ? 'red' :'white';",
  "      $('td:eq('+j+')', row)", 
  "        .css('background-color', color);",
  "  }",
  "}"
)

# prepare phv data ----
phv_clean = function(google_sheet_hvp){
  #read PHV data from sheet in function argument
  phv = readRDS("SHEET_ID_HVP") %>% select(-buffer)
  #cleaning
  names(phv) = c("timestamp","ploeg","naam","lengte","zithoogte","gewicht")
  
  #remove NA
  phv = na.omit(phv)
  
  return(phv)
}

