setwd("C:/Users/Jim/Documents/git_repos/dfs_lineup_generator/R")
for (f in list.files(pattern = "*.R")) {
  source(f)
}

rm(list = setdiff(ls(), lsf.str()))

t1 <- Sys.time()
salaries <- import_salaries("C:/Users/Jim/Documents/dfs/dk/nba/inputs/DKSalaries_20180209_night.csv", 
                            from_entry = TRUE)
salary_lu <- make_salary_lu(salaries, from_entry = TRUE)
nba_projections <- import_projections()
augmented_projections <- augment_projections(nba_projections)
projections_plus_salaries <- add_salary_ids(projections = augmented_projections,
                                            salaries = salary_lu)
in_scope_projections <- scope_projections(projections = projections_plus_salaries)
players_by_position <- split_by_position(in_scope_projections)
filtered_players <- filter_players_by_position(player_position_list = players_by_position,
                                               player_pts_rank = 60,
                                               player_value_rank = 8,
                                               player_pos_rank = 5)
possible_lineups <- make_possible_lineups(filtered_players, salary_min = 40000)
unique_lineups <- make_unique_lineups(possible_lineups)
augmented_unique_lineups <- augment_unique_lineups(unique_lineup_object = unique_lineups,
                                                   outcome_object = filtered_players,
                                                   salary_object = salary_lu)

optimized_lineups <- optimize_lineups(unique_lineup_object = augmented_unique_lineups,
                                      n_lineups = 41,
                                      limit_search = 25000,
                                      max_exposure = .65)

entry_list <- import_entries("C:/Users/Jim/Documents/dfs/dk/nba/inputs/DKEntries_20180209_night.csv")
setwd("C:/Users/Jim/Documents/dfs/dk/nba")
export_lineups(lineups = optimized_lineups$lineups[c(1:40), ], 
               entries = entry_list,
               randomize_entries = FALSE)
t2 <- Sys.time()
t2-t1
