#' Function to augment unique lineups
#' 
#' @param unique_lineup_object Object containing unique lineups
#' @param outcome_object the outcome data
#' 
#' @description Adds projection information to unique lineups
#' 
#' @return data frame containing augmented unique lineups
#' 
#' @export
augment_unique_lineups <- function(unique_lineup_object, 
                                   outcome_object) {
  
  outcome_positions <- names(outcome_object)
  
  lu_outcome <- do.call(rbind, outcome_object) %>%
    dplyr::group_by(uid) %>%
    dplyr::filter(row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(uid, tm, game_id, outcome)
  
  projection_mat <- matrix(nrow = nrow(unique_lineup_object), ncol = ncol(unique_lineup_object))
  
  game_mat <- matrix(nrow = nrow(unique_lineup_object), ncol = ncol(unique_lineup_object))
  
  tm_mat <- matrix(nrow = nrow(unique_lineup_object), ncol = ncol(unique_lineup_object))
  
  for (i in 1:ncol(unique_lineup_object)) {
    uid_column <- data.frame(uid = unique_lineup_object[, i])
    uid_outcome <- dplyr::inner_join(uid_column, lu_outcome, by = "uid")
    projection_mat[, i] <- uid_outcome$outcome
    game_mat[, i] <- uid_outcome$game_id
    tm_mat[, i] <- uid_outcome$tm
  }
  
  projections <- rowSums(projection_mat)
  unique_games <- as.numeric(apply(game_mat, 1, function(x) length(unique(x))))
  max_players_per_team <- as.numeric(apply(tm_mat, 1, function(x) max(table(x))))
  
  team_counter <- matrix(ncol = length(unique(lu_outcome$tm))) %>% as.data.frame()
  names(team_counter) <- unique(lu_outcome$tm)
  
  output_df <- data.frame(unique_lineup_object, outcome = projections, games = unique_games, max_team_rep = max_players_per_team)
  
  return(output_df)
  
}