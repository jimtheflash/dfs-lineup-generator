#' Import projections
#' 
#' @param source The site where projections are gathered.  Currently defaults to "rotogrinders" but is not used.
#' @param site Betting site. Default is "draftkings"
#' @param sport Sport for which lineups are generated. Default = "nba"
#' 
#' @return imported projections
#' 
#' @export
import_projections <- function(source = "rotogrinders", 
                               site = "draftkings", 
                               sport = "nba") {
  
  import_loc <- paste0("https://rotogrinders.com/projected-stats/", sport, "-player.csv?site=", site)

  projection_import <- try({read.table(import_loc, 
                                         sep = ",", 
                                         stringsAsFactors = FALSE)}, silent = TRUE)
  
  if (class(projection_import) == 'try-error'){
    stop("There were no projections to be downloaded from ", import_loc)
  }
  
  names(projection_import) <- c("player_name", "salary", "tm", "pos", "opp", "ceiling", "floor", "proj")
  
  projection_import <- dplyr::filter(projection_import, salary > 0)
  return(projection_import)
  
}