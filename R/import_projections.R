import_projections <- function(source = "rotogrinders", site = "draftkings") {

  projection_import <- read.table("https://rotogrinders.com/projected-stats/nba-player.csv?site=draftkings", 
                              sep = ",", 
                              stringsAsFactors = FALSE)
  
  names(projection_import) <- c("player_name", "salary", "tm", "pos", "opp", "ceiling", "floor", "proj")
  
  return(projection_import)
  
  }