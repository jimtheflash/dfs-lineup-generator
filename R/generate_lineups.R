#' Generate Lineups from YAML
#' 
#' @param config_yaml The configuration yaml, which contains all the pertinent information
#' @param optional_file Default NULL. If provided, this is the file we'll use to get salary/entry information. Otherwise, we'll use the slate ID from the config_yaml and scrape DK for the data
#' 
#' @return NULL
#' 
#' @export
generate_lineups_from_yaml <- function(config_yaml, optional_file = NULL){
  
  # Start a timer
  t1 <- Sys.time()
  
  # Grab data from yaml
  message("Importing Config data...")
  config <- yaml::yaml.load_file(config_yaml)
  
  
  # Salary information
  message("Gathering salary information...")
  if (is.null(optional_file)){
    current_slate <- paste0('https://www.draftkings.com/bulklineup/getdraftablecsv?draftGroupId=',config$slate_id)
    salaries <- import_salaries(current_slate, from_entry = FALSE)
  } else {
    current_slate = optional_file
    salaries <- import_salaries(current_slate, from_entry = FALSE)
  }
  
  # Make salary lookup
  salary_lu <- make_salary_lu(salaries, from_entry = FALSE)
  
  # Projection Data
  #  NOTE - if other sites are used at some point in the future, this would be another thing to include in the config
  message("Importing projections...")
  nba_projections <- import_projections()
  augmented_projections <- augment_projections(nba_projections, 
                                               projection_to_use = config$projections_to_use$projection_to_use, 
                                               value_projection = config$projections_to_use$value_projection)
  
  # Fix the names
  fixed_names <- fix_nba_names(salary_lookup = salary_lu, projection_list = augmented_projections$augmented_projections)
  
  # Append the salary information
  projections_plus_salaries <- add_salary_ids(projections = fixed_names$fixed_projections,
                                              salaries = fixed_names$fixed_salaries)
  
  
  # Scoping
  in_scope_projections <- scope_projections(projections = projections_plus_salaries)
  players_by_position <- split_by_position(in_scope_projections$in_scope)
  
  # Filtering players
  filtered_players <- filter_players_by_position(player_position_list = players_by_position,
                                                 player_pts_rank = config$player_filters$player_pts_rank,
                                                 player_value_rank = config$player_filters$player_value_rank,
                                                 player_pos_rank = config$player_filters$player_pos_rank)
  
  # Making possible lineups
  possible_lineups <- make_possible_lineups(filtered_players, 
                                            salary_cap = config$lineup_config$salary_max,
                                            salary_min = config$lineup_config$salary_min)
  
  # Get the unique ones
  unique_lineups <- make_unique_lineups(possible_lineups)
  augmented_unique_lineups <- augment_unique_lineups(unique_lineup_object = unique_lineups,
                                                     outcome_object = filtered_players,
                                                     salary_object = fixed_names$fixed_salaries)
  
  optimized_lineups <- optimize_lineups(unique_lineup_object = augmented_unique_lineups,
                                        n_lineups = config$lineup_config$n_lineups,
                                        limit_search = config$lineup_config$limit_search,
                                        max_exposure = config$lineup_config$max_exposure)
  
  entry_list <- import_entries(current_slate)
  
  export_lineups(lineups = optimized_lineups$lineups, 
                 file_name = 'C:/Users/anthony/Desktop/WHATEVER.csv', 
                 entries = entry_list,
                 randomize_entries = FALSE)
  t2 <- Sys.time()
  message("The lineup optimizer took: ",  round(difftime(t2,t1,units="min"),2), " minutes.")
}
