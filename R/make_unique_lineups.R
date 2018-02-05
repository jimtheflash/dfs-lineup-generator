make_unique_lineups <- function(all_possible_lineups) {
  
  lineup_matrix <- all_possible_lineups %>%
    select(-ends_with("_salary")) %>%
    as.matrix()
  
  ordered_lineups <- t(apply(lineup_matrix, 1, sort))
  
  lineupid_vec <- apply(ordered_lineups, 1, function(x) paste(x, collapse = "_"))
  
  unique_lineups <- all_possible_lineups %>%
    mutate(lineup_id = lineupid_vec) %>%
    group_by(lineup_id) %>%
    filter(row_number() == 1) %>%
    ungroup()

}