# TODO: remove hard coding of positions

make_possible_lineups <- function(player_position_list, salary_cap = 50000, salary_min = 30000) {

  possible_lineups <- expand.grid(lapply(player_position_list, 
                                         function(x) unique(x$uid)), stringsAsFactors = FALSE) %>%
    dplyr::filter(pg != sg & pg != pf & pg != sf & pg != `c` & pg != `f` & pg != `g` & pg != util &
                    sg != pf & sg != sf & sg != `c` & sg != `f` & sg != `g` & sg != util &
                    pf != sf & pf != `c` & pf != `f` & pf != `g` & pf != util &
                    sf != `c` & sf != `f` & sf != `g` & sf != util &
                    `c` != `f` & `c` != `g` & `c` != util &
                    `f` != `g` & `f` != util &
                    `g` != util)
  
  # attach position salaries
  for (i in names(player_position_list)) {
    pos_df <- data.frame(uid = possible_lineups[[i]])
    lu <- dplyr::select(player_position_list[[i]] %>% ungroup(), uid, salary)
    slry <- dplyr::left_join(pos_df, lu, by = "uid")
    new_col <- paste0(i, "_salary")
    possible_lineups[[new_col]] <- as.numeric(slry$salary)
  }
  
  # create a salary filter for valid lineups
  salary_df <- select(possible_lineups, ends_with("_salary")) %>%
    as.data.frame()
  total_salary <- rowSums(salary_df, na.rm = TRUE)
  possible_lineups$allpos_total_salary <- total_salary
  
  possible_lineups <- possible_lineups %>%
    dplyr::filter(allpos_total_salary <= salary_cap & allpos_total_salary >= salary_min)
  
  attr(possible_lineups, "out.attrs") <- NULL
  
  return(possible_lineups)
  
}