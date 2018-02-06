augment_projections <- function(projection_data, 
                                projection_to_use = "ceiling", 
                                value_projection = "ceiling",
                                game_style = "classic") {
  
  library(dplyr)
  
  player_augmented <- projection_data %>%
    mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", player_name)),
           outcome = projection_data[[projection_to_use]]) %>%
    mutate(pts_rank = rank(-outcome))
  
  if (game_style == "classic") {
    player_augmented <- player_augmented %>%
      mutate(sal_over_onethousand = salary / 1000) %>%
      mutate(ppk = outcome / sal_over_onethousand) %>%
      mutate(ppk_rank = rank(-ppk))
    
    pos_split <- strsplit(player_augmented$pos, split = "/", fixed = TRUE)
    
    player_augmented$pos1 <- lapply(pos_split, "[", 1) %>% 
      unlist() %>% 
      tolower()
    
    player_augmented$pos2 <- lapply(pos_split, "[", 2) %>% 
      unlist() %>% 
      tolower()
  }
  
  player_augmented <- na.omit(player_augmented)
  player_augmented$uid <- 1:nrow(player_augmented)
  
  return(player_augmented)
 
}