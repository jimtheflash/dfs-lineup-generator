scope_projections <- function(projections) {
  
  scoped_projections <- filter(projections, !is.na(salary_id))
  excluded_projections <- filter(projections, is.na(salary_id))
  
  return_list <- list(in_scope = scoped_projections,
                      out_of_scope = excluded_projections)
  
  return(return_list)
  
}