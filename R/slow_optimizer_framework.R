rm(list = ls())
gc(reset = TRUE)

library(dplyr)

#### read in salaries ####
salary_import <- read.csv(file.choose(),
                          header = FALSE,
                          stringsAsFactors = FALSE,
                          na.strings = "")

salary_import_clean <- salary_import[-c(1:8), ]
new_names <- as.character(salary_import_clean[1, ])
names(salary_import_clean) <- gsub("[^[:alnum:]]", "", new_names) %>% 
  tolower()

salary_template_lu <- data.frame(player_name = salary_import_clean$name, salary_id = salary_import_clean$id,
                                 stringsAsFactors = FALSE) %>%
  filter(player_name != "Name" & !is.na(player_name) & !is.na(salary_id)) %>%
  mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", player_name))) %>%
  mutate(lower_clean_name = gsub("ottoporterjr", "ottoporter", lower_clean_name))


#### read in predictions from url ####
 
player_import <- read.table("https://rotogrinders.com/projected-stats/nba-player.csv?site=draftkings", 
                             sep = ",", 
                             stringsAsFactors = FALSE)
 
#### option for csv import ####
# player_import <- read.csv(file.choose(), header = FALSE, stringsAsFactors = FALSE)
#### fix column names ####
names(player_import) <- c("player_name", "salary", "tm", "pos", "opp", "ceiling", "floor", "proj")

#### compute ppk variables ####
player_computed <- player_import %>%
  mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", player_name)),
         sal_over_onethousand = salary / 1000,
         range = ceiling - floor) %>%
  filter(lower_clean_name %in% unique(salary_template_lu$lower_clean_name)) %>%
  mutate_at(vars(ceiling, floor, proj), funs(ppk = . / sal_over_onethousand)) %>%
  na.omit() %>%
  mutate(ppk_rank = rank(-ceiling_ppk),
         pts_rank = rank(-ceiling))

player_computed$uid <- 1:nrow(player_computed)

  
#### recode positions ####
pos_split <- strsplit(player_computed$pos, split = "/", fixed = TRUE)


player_computed$pos1 <- lapply(pos_split, "[", 1) %>% 
  unlist() %>% 
  tolower()

player_computed$pos2 <- lapply(pos_split, "[", 2) %>% 
  unlist() %>% 
  tolower()


#### split by position ####
positions <- unique(c(player_computed$pos1, 
                      player_computed$pos2)) %>%
  na.omit()

player_by_position <- list()

for(i in positions) {
  player_by_position[[i]] <- player_computed %>%
    filter(pos1 == i | pos2 == i) %>%
    arrange(pts_rank) %>%
    mutate(player_position_rank = row_number())
  rm(i)
  }

# manually add the guard, forward, utility positions
player_by_position$g <- rbind(player_by_position$pg, player_by_position$sg) %>%
  select(-player_position_rank) %>%
  group_by(player_name) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  arrange(pts_rank) %>%
  mutate(player_position_rank = row_number())

player_by_position$f <- rbind(player_by_position$pf, player_by_position$sf) %>%
  select(-player_position_rank) %>%
  group_by(player_name) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  arrange(pts_rank) %>%
  mutate(player_position_rank = row_number())

player_by_position$util <- player_computed %>%
  arrange(pts_rank) %>%
  mutate(player_position_rank = row_number())


#### filter by player_position_rank ####
# TODO: set a parameter, and make it variable by position
filtered_players <- lapply(player_by_position, 
                           function(x) filter(x, pts_rank <= 25 | ppk_rank <= 7 | player_position_rank <= 3))

unique(unlist(lapply(filtered_players, function(x) x$player_name)))

#### build lineups ####
# get all possible lineups without duplicate players
possible_lineups <- expand.grid(lapply(filtered_players, 
                                       function(x) unique(x$uid)), stringsAsFactors = FALSE) %>%
  filter(pg != sg & pg != pf & pg != sf & pg != `c` & pg != `f` & pg != `g` & pg != util &
           sg != pf & sg != sf & sg != `c` & sg != `f` & sg != `g` & sg != util &
           pf != sf & pf != `c` & pf != `f` & pf != `g` & pf != util &
           sf != `c` & sf != `f` & sf != `g` & sf != util &
           `c` != `f` & `c` != `g` & `c` != util &
           `f` != `g` & `f` != util &
           `g` != util)

# attach position salaries
for (i in names(filtered_players)) {
  pos_df <- data.frame(uid = possible_lineups[[i]])
  lu <- select(filtered_players[[i]] %>% ungroup(), uid, salary)
  slry <- left_join(pos_df, lu, by = "uid")
  new_col <- paste0(i, "_salary")
  possible_lineups[[new_col]] <- as.numeric(slry$salary)
  rm(pos_df, lu, slry, new_col)
}

# create a filter for valid lineups (i.e. under 50K)
salary_df <- select(possible_lineups, ends_with("_salary")) %>%
  as.data.frame()
total_salary <- rowSums(salary_df, na.rm = TRUE)
possible_lineups$allpos_total_salary <- total_salary

# TODO: parameterize the lower bound of salaries
filtered_lineups <- possible_lineups %>%
  filter(allpos_total_salary <= 50000) %>%
  filter(allpos_total_salary >= 40000)

# tidy up big objects
rm(possible_lineups, salary_df, total_salary)
gc()

# attach position information for estimated scores
for (i in names(filtered_players)) {
  pos_df <- data.frame(uid = filtered_lineups[[i]])
  lu <- select(filtered_players[[i]] %>% ungroup(), uid, player_name, ceiling, floor, proj)
  lu_plus_scores <- left_join(pos_df, lu, by = "uid")
  new_name <- paste0(i, "_player_name")
  new_ceiling <- paste0(i, "_ceiling")
  new_floor <- paste0(i, "_floor")
  new_proj <- paste0(i, "_proj")
  
  filtered_lineups[[new_name]] <- lu_plus_scores$player_name
  filtered_lineups[[new_ceiling]] <- lu_plus_scores$ceiling
  filtered_lineups[[new_floor]] <- lu_plus_scores$floor
  filtered_lineups[[new_proj]] <- lu_plus_scores$proj
  
  rm(pos_df, lu, lu_plus_scores, new_ceiling, new_floor, new_proj)
}


#### get unique lineups, i.e. lineups that have unique sums of ceilings, floors, and projs ####
ceilings <- select(filtered_lineups, ends_with("_ceiling"))
floors <- select(filtered_lineups, ends_with("_floor"))
projs <- select(filtered_lineups, ends_with("_proj"))

ceiling_total <- rowSums(ceilings)
floor_total <- rowSums(floors)
proj_total <- rowSums(projs)
unique_total <- ceiling_total + floor_total + proj_total

filtered_lineups$ceiling_total_points <- ceiling_total
filtered_lineups$floor_total_points <- floor_total
filtered_lineups$proj_total_points <- proj_total
filtered_lineups$unique_total_points <- unique_total

rm(ceilings, floors, projs, ceiling_total, floor_total, proj_total, unique_total)

unique_lineups <- filtered_lineups %>%
  group_by(unique_total_points) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(lineupid = row_number())

for (i in names(filtered_players)) {
  new_col <- paste0(i, "_salaryid")
  name_col <- paste0(i, "_player_name")
  temp_name <- data.frame(player_name = unique_lineups[[name_col]],
                          lower_clean_name = tolower(gsub("[^[:alnum:]]", "", unique_lineups[[name_col]])),
                          stringsAsFactors = FALSE)
  join_table <- left_join(temp_name, salary_template_lu, by = "lower_clean_name")
  unique_lineups[[new_col]] <- as.numeric(join_table$salary_id)
}


# filter to get only lineups with both > 20th percentile total points
final_lineups <- unique_lineups %>%
  arrange(-ceiling_total_points) %>%
  filter(row_number() <= 40)

# balance out the lineups so we're not over 60% exposed to any player
freq_table <- final_lineups[, names(final_lineups) %in% names(filtered_players)] %>% 
  as.matrix() %>% 
  table() %>% 
  as.data.frame(stringsAsFactors = FALSE)
names(freq_table) <- c("uid", "Freq")

t1 <- Sys.time()
while (max(freq_table$Freq) > (nrow(final_lineups) * .65) & 
       nrow(unique_lineups) > 1 & 
       nrow(final_lineups) == 40) {
  
  max_uid <- freq_table$uid[which.max(freq_table$Freq)] %>% 
    as.numeric()
  
  temp <- unique_lineups[, names(unique_lineups) %in% names(filtered_players)] %>%
    as.matrix()
  
  row_filt <- apply(temp, 1, function(x) max_uid %in% unique(x))
  
  lu_to_insert <- unique_lineups[!row_filt, ] %>%
    filter(!(lineupid %in% final_lineups$lineupid)) %>%
    arrange(-ceiling_total_points) %>%
    filter(row_number() == 1)
  
  if (nrow(lu_to_insert) == 0) {
    break
  } else {
    lu_to_remove <- 
      final_lineups[apply(final_lineups[, names(final_lineups) %in% names(filtered_players)], 1, function(x) max_uid %in% x), ] %>%
      filter(ceiling_total_points == min(ceiling_total_points))
    
    final_lineups <- final_lineups %>%
      filter(lineupid != lu_to_remove$lineupid) %>%
      rbind(lu_to_insert)
    
    unique_lineups <- unique_lineups %>%
      filter(lineupid != lu_to_remove$lineupid)
    
      freq_table <- final_lineups[, names(final_lineups) %in% names(filtered_players)] %>% 
      as.matrix() %>% 
      table() %>% 
      as.data.frame(stringsAsFactors = FALSE)
    names(freq_table) <- c("uid", "Freq")
    
    print(paste0(nrow(unique_lineups), " rows remaining"))
    
  }
  
}
t2 <- Sys.time()

t2 - t1

hist(final_lineups$ceiling_total_points)
summary(final_lineups$allpos_total_salary)
View(freq_table)
View(select(final_lineups, ends_with("_name")))

salaryids <- select(final_lineups, ends_with("_salaryid"))
names(salaryids) <- gsub("_salaryid", "", names(salaryids))
names(salaryids) <- toupper(names(salaryids))
salaryids <- salaryids %>%
  select(PG, SG, SF, PF, C, G, `F`, UTIL)

output <- salaryids[order(rnorm(nrow(salaryids))), ]
# output <- salaryids
View(output)
write.csv(output, file = "dk_lineups_night.csv", quote = FALSE, row.names = FALSE)

