filter_players_by_position <- function(player_position_list = players_by_position,
                                       player_pts_rank = 20,
                                       player_value_rank = 5,
                                       player_pos_rank = 3) {
  
  filtered_players <- lapply(player_position_list, 
                             function(x) filter(x, pts_rank <= player_pts_rank |
                                                  ppk_rank <= player_value_rank |
                                                  player_position_rank <= player_pos_rank))
  
  return(filtered_players)
  
}