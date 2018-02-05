make_salary_lu <- function(salary_import,
                           site = "draftkings", 
                           from_entry = FALSE) {
                           
  
  library(dplyr)
  
  if (from_entry == TRUE) {
    
    salary_import_clean <- salary_import
    names(salary_import_clean) <- gsub("[^[:alnum:]]", "", names(salary_import_clean)) %>% 
      tolower()
    
  } else {
    
    salary_import_clean <- salary_import[-c(1:8), ]
    new_names <- as.character(salary_import_clean[1, ])
    names(salary_import_clean) <- gsub("[^[:alnum:]]", "", new_names) %>% 
      tolower()
  }
  
  salary_lu <- suppressWarnings(
    data.frame(player_name = salary_import_clean$name,
               salary_id = as.numeric(salary_import_clean$id),
               salary = as.numeric(salary_import_clean$salary),
               stringsAsFactors = FALSE) %>%
      filter(player_name != "Name" & !is.na(player_name) & !is.na(salary_id)) %>%
      mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", player_name)))
  )
  
  return(salary_lu)
  
}