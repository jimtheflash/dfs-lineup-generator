#' Function to split out be position
#' 
#' @param projections Projection data
#' @param entry_list Entry list with position names as headers
#' 
#' @export
split_by_position <- function(projections, entries) {
  
  browser()
  
  position_list <- entries %>%
    dplyr::select(-ends_with("Fee"), -ends_with("ID"), -ends_with("Name")) %>%
    names() %>%
    tolower()  
  
  for(i in position_list) {
    player_by_position[[i]] <- projections %>%
      dplyr::filter(pos1 == i | pos2 == i) %>%
      dplyr::arrange(pts_rank) %>%
      dplyr::mutate(player_position_rank = row_number())
  }
  
  # manually add the guard, forward, utility positions
  player_by_position$g <- rbind(player_by_position$pg, player_by_position$sg) %>%
    dplyr::select(-player_position_rank) %>%
    dplyr::group_by(player_name) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(pts_rank) %>%
    dplyr::mutate(player_position_rank = row_number())
  
  player_by_position$f <- rbind(player_by_position$pf, player_by_position$sf) %>%
    dplyr::select(-player_position_rank) %>%
    dplyr::group_by(player_name) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(pts_rank) %>%
    dplyr::mutate(player_position_rank = row_number())
  
  player_by_position$util <- projections %>%
    dplyr::arrange(pts_rank) %>%
    dplyr::mutate(player_position_rank = row_number())
  
  return(player_by_position)
  
}