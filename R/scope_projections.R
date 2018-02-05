scope_projections <- function(projections) {
  
  scoped_projections <- filter(projections, !is.na(salary_id))
  
  return(scoped_projections)
  
}