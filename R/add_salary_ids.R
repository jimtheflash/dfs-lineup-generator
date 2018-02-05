add_salary_ids <- function(projections, salaries) {
  
  library(dplyr)
  
  players_plus_salary_id <- projections %>%
    left_join(select(salaries, "salary_id", "lower_clean_name"), 
              by = "lower_clean_name")
  
  return(players_plus_salary_id)
  
}