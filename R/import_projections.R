import_projections <- function(source = "rotogrinders", 
                               site = "draftkings", 
                               sport = "nba") {
  
  import_loc <- paste0("https://rotogrinders.com/projected-stats/", sport, "-player.csv?site=", site)

  projection_import <- read.table(import_loc, 
                              sep = ",", 
                              stringsAsFactors = FALSE)
  
  names(projection_import) <- c("player_name", "salary", "tm", "pos", "opp", "ceiling", "floor", "proj")
  
  projection_import <- dplyr::filter(projection_import, salary > 0)
  
  return(projection_import)
  
  }