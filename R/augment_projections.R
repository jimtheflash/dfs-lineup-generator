augment_projections <- function(projection_data, projection_to_use = "ceiling", value_projection = "ceiling") {
  
  library(dplyr)
  
  player_augmented <- projection_data %>%
    mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", player_name)),
           outcome = projection_data[[projection_to_use]],
           sal_over_onethousand = salary / 1000) %>%
    mutate(ppk = outcome / sal_over_onethousand) %>%
    na.omit() %>%
    mutate(ppk_rank = rank(-ppk),
           pts_rank = rank(-outcome))
  
  player_augmented$uid <- 1:nrow(player_augmented)
  
  pos_split <- strsplit(player_augmented$pos, split = "/", fixed = TRUE)
  
  player_augmented$pos1 <- lapply(pos_split, "[", 1) %>% 
    unlist() %>% 
    tolower()
  
  player_augmented$pos2 <- lapply(pos_split, "[", 2) %>% 
    unlist() %>% 
    tolower()
  
  return(player_augmented)
 
}