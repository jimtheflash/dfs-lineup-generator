#' Quick function to filter players by position
#' 
#' @param player_position_list The list of all applicable positions
#' @param player_pts_rank The lower-limit of points-ranking allowed (Default = 20)
#' @param player_value_rank The lower-limit of player value ranking allowed (Default = 5)
#' @param player_pos_rank The lower-limit of player's positional rank (Default = 3)
#' 
#' @description Filtering is an OR conditional, meaning that players are excluded only when they fail to meet ANY of the rank criteria. So, a player with points rank of 30, a value rank of 8, but a position rank of 2, will be included.
#' 
#' @export

filter_players_by_position <- function(player_position_list = players_by_position,
                                       player_pts_rank = 20,
                                       player_value_rank = 5,
                                       player_pos_rank = 3) {
  
  filtered_players <- lapply(player_position_list, 
                             function(x) {dplyr::filter(x, pts_rank <= player_pts_rank |
                                                  ppk_rank <= player_value_rank |
                                                  player_position_rank <= player_pos_rank)})
  
  return(filtered_players)
  
}