
rm(list = ls())

t1 <- Sys.time() 
library(dplyr)
library(doParallel)
library(foreach)

salaries <- read.csv("C:/Users/Jim/Documents/dfs/dk/pga/DKSalaries_20180208_pga.csv", stringsAsFactors = FALSE)


projections <- read.csv("C:/Users/Jim/Documents/dfs/dk/pga/pga_proj.csv", stringsAsFactors = FALSE)


players_salaries_projections <- salaries %>%
  left_join(projections, by = "Name")

filtered_players <- players_salaries_projections %>%
  arrange(-pts) %>%
  mutate(pts_rank = 1:nrow(players_salaries_projections)) %>%
  arrange(-ppk) %>%
  mutate(ppk_rank = 1:nrow(players_salaries_projections)) %>%
  filter(pts_rank <= 79 | ppk_rank <= 79) %>%
  select(ID, sal, pts) %>%
  as.data.frame()


sim_function <- function() {
  library(dplyr)
  stor <- data.frame(ID = NA, sal = NA, pts = NA, lu_id = NA, stringsAsFactors = FALSE)
  
  while (length(unique(stor$lu_id)) <= 1000) {
    obj <- filtered_players[sample(1:nrow(filtered_players), 6, replace = FALSE), ]
    obj$lu_id <- paste(sort(obj$ID), collapse = "_")
    if (obj$lu_id[[1]] %in% unique(stor$lu_id)) {
      next
    }
    
    if (sum(obj$sal) > 50 | sum(obj$sal) < 35) {
      next
    }
    
    if (nrow(stor) == 1) {
      stor <- obj
    } else {
      stor <- rbind(stor, obj)
    }
  }
  
  grouped <- stor %>%
    group_by(lu_id) %>%
    summarise(total_sal = sum(sal),
              total_pts = sum(pts))
  
  return(grouped)
  
}

cl <- makeCluster(5)
registerDoParallel(cl)
salary_list <- c()

big_lineup_list <- foreach (i = 1:1000) %dopar% sim_function()

stopCluster(cl)

t2 <- Sys.time()
t2 - t1


unique_lineups <- do.call(rbind, big_lineup_list) %>%
  group_by(lu_id) %>%
  filter(row_number() == 1)
  

unique_lineup_matrix <- strsplit(unique_lineups$lu_id, "_") %>%
  unlist() %>%
  matrix(nrow = nrow(unique_lineups)) %>%
  cbind(unique_lineups$total_pts)

unique_lineup_matrix <- apply(unique_lineup_matrix, 2, as.numeric)
deduped_filter <- apply(unique_lineup_matrix, 1, function(x) length(unique(x)) == ncol(unique_lineup_matrix))
unique_lineup_matrix_deduped <- unique_lineup_matrix[deduped_filter, ]

unique_lineup_matrix_deduped <- unique_lineup_matrix_deduped[rev(order(unique_lineup_matrix_deduped[, 7])), ]


test <- head(unique_lineup_matrix_deduped, 20)
write.csv(test, "pga.csv", row.names = FALSE, quote = FALSE)

