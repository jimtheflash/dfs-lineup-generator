split_by_position <- function(projections) {
  
  positions <- c("pg", "sg", "pf", "sf", "c")
  
  player_by_position <- list()
  
  for(i in positions) {
    player_by_position[[i]] <- projections %>%
      filter(pos1 == i | pos2 == i) %>%
      arrange(pts_rank) %>%
      mutate(player_position_rank = row_number())
  }
  
  # manually add the guard, forward, utility positions
  player_by_position$g <- rbind(player_by_position$pg, player_by_position$sg) %>%
    select(-player_position_rank) %>%
    group_by(player_name) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(pts_rank) %>%
    mutate(player_position_rank = row_number())
  
  player_by_position$f <- rbind(player_by_position$pf, player_by_position$sf) %>%
    select(-player_position_rank) %>%
    group_by(player_name) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    arrange(pts_rank) %>%
    mutate(player_position_rank = row_number())
  
  player_by_position$util <- projections %>%
    arrange(pts_rank) %>%
    mutate(player_position_rank = row_number())
  
  return(player_by_position)
  
}