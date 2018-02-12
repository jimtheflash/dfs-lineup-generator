setwd("C:/Users/Jim/Documents/git_repos/dfs_lineup_generator/R")
for (f in list.files(pattern = "*.R")) {
  source(f)
}

rm(list = setdiff(ls(), lsf.str()))

t1 <- Sys.time()
salaries <- import_salaries("C:/Users/Jim/Documents/dfs/dk/nba/inputs/DKSalaries_20180211_early.csv", 
                            from_entry = TRUE)
salary_lu <- make_salary_lu(salaries, from_entry = TRUE)
nba_projections <- import_projections()
augmented_projections <- augment_projections(nba_projections, projection_to_use = "proj", value_projection = "proj")
fixed_names <- fix_nba_names(salary_lookup = salary_lu, projection_list = augmented_projections$augmented_projections)
projections_plus_salaries <- add_salary_ids(projections = fixed_names$fixed_projections,
                                            salaries = fixed_names$fixed_salaries)
in_scope_projections <- scope_projections(projections = projections_plus_salaries)
players_by_position <- split_by_position(in_scope_projections$in_scope)
filtered_players <- filter_players_by_position(player_position_list = players_by_position,
                                               player_pts_rank = 40,
                                               player_value_rank = 10,
                                               player_pos_rank = 5)
possible_lineups <- make_possible_lineups(filtered_players, salary_min = 41000)
unique_lineups <- make_unique_lineups(possible_lineups)
augmented_unique_lineups <- augment_unique_lineups(unique_lineup_object = unique_lineups,
                                                   outcome_object = filtered_players,
                                                   salary_object = fixed_names$fixed_salaries)

optimized_lineups <- optimize_lineups(unique_lineup_object = augmented_unique_lineups,
                                      n_lineups = 40,
                                      limit_search = 50000,
                                      max_exposure = .6)

entry_list <- import_entries("C:/Users/Jim/Documents/dfs/dk/nba/inputs/DKEntries_20180211_early.csv")
setwd("C:/Users/Jim/Documents/dfs/dk/nba")
export_lineups(lineups = optimized_lineups$lineups, 
               entries = entry_list,
               randomize_entries = FALSE)
t2 <- Sys.time()
t2-t1
