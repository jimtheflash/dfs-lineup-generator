# salary_import <- read.csv(file.choose(),
#                           header = FALSE,
#                           stringsAsFactors = FALSE,
#                           na.strings = "")
# 
# salary_import_clean <- salary_import[-c(1:8), ]
# new_names <- as.character(salary_import_clean[1, ])
# names(salary_import_clean) <- gsub("[^[:alnum:]]", "", new_names) %>% 
#   tolower()
# 
# salary_lu <- salary_import_clean[, c(1:6)] %>%
#   filter(!is.na(id) & id != "ID") %>%
#   mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", name))) %>%
#   mutate(lower_clean_name = gsub("ottoporterjr", "ottoporter", lower_clean_name))
# 
player_import <- read.table("https://rotogrinders.com/projected-stats/nba-player.csv?site=draftkings",
                            sep = ",",
                            stringsAsFactors = FALSE)

salary_lu <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE) %>%
  mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", Name)))

#### option for csv import ####
# player_import <- read.csv(file.choose(), header = FALSE, stringsAsFactors = FALSE)
#### fix column names ####
names(player_import) <- c("player_name", "salary", "tm", "pos", "opp", "ceiling", "floor", "proj")

#### compute ppk variables ####
player_computed <- player_import %>%
  mutate(lower_clean_name = tolower(gsub("[^[:alnum:]]", "", player_name))) %>%
  mutate(pts_rank = rank(-ceiling)) %>%
  inner_join(salary_lu)

possible_combinations <- expand.grid(T1 = player_computed$lower_clean_name[player_computed$Roster.Position == "T1"],
                                     T2 = player_computed$lower_clean_name[player_computed$Roster.Position == "T2"],
                                     T3 = player_computed$lower_clean_name[player_computed$Roster.Position == "T3"],
                                     T4 = player_computed$lower_clean_name[player_computed$Roster.Position == "T4"],
                                     T5 = player_computed$lower_clean_name[player_computed$Roster.Position == "T5"],
                                     T6 = player_computed$lower_clean_name[player_computed$Roster.Position == "T6"],
                                     stringsAsFactors = FALSE)

for (i in names(possible_combinations)) {
  new_col_name <- paste0(i, "_ceiling")
  
  ceiling_lu <- data.frame(lower_clean_name = possible_combinations[[i]], stringsAsFactors = FALSE) %>%
    left_join(select(player_computed, lower_clean_name, ceiling))
  
  possible_combinations[[new_col_name]] <- ceiling_lu$ceiling
  
}

projections <- possible_combinations %>%
  select(ends_with("ceiling")) %>%
  mutate(total = rowSums(.)) %>%
  select(total) %>%
  cbind(possible_combinations) %>%
  arrange(-total) %>%
  filter(row_number() <= 20)

for (i in c("T1", "T2", "T3", "T4", "T5", "T6")) {
  new_col_name <- paste0(i, "_id")
  
  id_lu <- data.frame(lower_clean_name = projections[[i]], stringsAsFactors = FALSE) %>%
    left_join(select(salary_lu, lower_clean_name, ID))
  
  projections[[new_col_name]] <- id_lu$ID
}

output <- projections %>%
  select(ends_with("_id"))

names(output) <- gsub("_id", "", names(output))
write.csv(output, "pickem_output.csv", row.names = FALSE, quote = FALSE)
getwd()
