#colour DT tabel: Heart Rate overview----
rowCallback_heart_rate_db <- c(
  "function(row, dat, displayNum, index){",
  "    for(var j=3; j<dat.length; j++){",
  "      var x = dat[j];",
  "      var color = x == null ? 'white' : (x/dat[3]) <= 0.6 ? 'lightgray' : (x/dat[3]) <= 0.7 ? 'lightblue' : 
                               (x/dat[3]) <= 0.8 ? 'lightgreen': (x/dat[3]) <= 0.9 ? 'yellow' :'red';",
  "      $('td:eq('+j+')', row)", 
  "        .css('background-color', color);",
  "  }",
  "}"
)

#colour DT tabel: Mean heart rate ----
rowCallback_mean_heart_rate <- c(
  "function(row, dat, displayNum, index){",
  "    for(var j=2; j<dat.length; j++){",
  "      var x = dat[j];",
  "      var color = x == null ? 'white' : (x/dat[2]) <= 0.6 ? 'lightgray' : (x/dat[2]) <= 0.7 ? 'lightblue' : 
                               (x/dat[2]) <= 0.8 ? 'lightgreen': (x/dat[2]) <= 0.9 ? 'yellow' :'red';",
  "      $('td:eq('+j+')', row)", 
  "        .css('background-color', color);",
  "  }",
  "}"
)
