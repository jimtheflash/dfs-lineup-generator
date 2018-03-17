#' Function to split out be position
#' 
#' @param projections Projection data
#' @param entry_list Entry list with position names as some of the headers
#' @param multiple_positions vector of position names that use multiple positions
#' 
#' @export
split_by_position <- function(projections, entries, multiple_positions = c("util", "flex", "g", "f")) {
  
  position_list_raw <- names(entries) %>% tolower()
  position_list <- unique(position_list_raw[!grepl("fee|name|id$", position_list_raw)])
  
  player_by_position <- list()
  player_by_position[1:length(position_list)] <- 0
  names(player_by_position) <- position_list
  
  for(i in position_list) {
    
    if(i %in% multiple_positions) {
      next
    }
    
    player_by_position[[i]] <- projections %>%
      dplyr::filter(pos1 == i | pos2 == i) %>%
      dplyr::arrange(pts_rank) %>%
      dplyr::mutate(player_position_rank = row_number())
  }
  
  special_pos <- names(player_by_position)[names(player_by_position) %in% multiple_positions]
  
  for (j in special_pos) {
    if (j == "g") {
      player_by_position$g <- rbind(player_by_position$pg, player_by_position$sg) %>%
        dplyr::select(-player_position_rank) %>%
        dplyr::group_by(player_name) %>%
        dplyr::filter(row_number() == 1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(pts_rank) %>%
        dplyr::mutate(player_position_rank = row_number()) 
      next
    }
    
    if (j == "f") {
      player_by_position$f <- rbind(player_by_position$pf, player_by_position$sf) %>%
        dplyr::select(-player_position_rank) %>%
        dplyr::group_by(player_name) %>%
        dplyr::filter(row_number() == 1) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(pts_rank) %>%
        dplyr::mutate(player_position_rank = row_number())
      next
    }
    
    if (j == "util") {
      player_by_position$util <- projections %>%
        dplyr::arrange(pts_rank) %>%
        dplyr::mutate(player_position_rank = row_number())
      next
    }
  }
  
  output <- list(players_by_position = player_by_position,
                 final_positions = position_list_raw[!grepl("fee|name|id$", position_list_raw)])
  
  return(output)

  
}