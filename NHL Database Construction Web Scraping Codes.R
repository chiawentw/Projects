library(xml2)
library(rvest)
library(tidyverse)
library(dplyr)
library(plyr)
library(stringr)
library(tidyr)
library(RCurl)
library(purrr)
library(dbplyr)
library(jsonlite)
# for division table
conference <- read_html("https://en.wikipedia.org/wiki/History_of_organizational_changes_in_the_NHL")
str(conference)
# select the table using CSS selector 
nodes_conference <- html_nodes(conference, "table")
# extract the table content using subsetting
con_90 <- html_table(nodes_conference)[[41]]
con_91 <- html_table(nodes_conference)[[43]]
con_92 <- html_table(nodes_conference)[[45]]
con_93 <- html_table(nodes_conference)[[47]]
con_95 <- html_table(nodes_conference)[[48]]
con_96 <- html_table(nodes_conference)[[49]]
con_97 <- html_table(nodes_conference)[[50]]
con_98 <- html_table(nodes_conference)[[52]]
con_99 <- html_table(nodes_conference)[[54]]
con_00 <- html_table(nodes_conference)[[56]]
con_06 <- html_table(nodes_conference)[[57]]
con_11 <- html_table(nodes_conference)[[58]]
con_13 <- html_table(nodes_conference)[[59]]
con_14 <- html_table(nodes_conference)[[60]]
con_17 <- html_table(nodes_conference)[[62]]
con_20 <- html_table(nodes_conference)[[63]]
con_21 <- html_table(nodes_conference)[[65]]
write.csv(con_90, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1990.csv", row.names = FALSE)
write.csv(con_91, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1991.csv", row.names = FALSE)
write.csv(con_92, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1992.csv", row.names = FALSE)
write.csv(con_93, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1993.csv", row.names = FALSE)
write.csv(con_95, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1995.csv", row.names = FALSE)
write.csv(con_96, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1996.csv", row.names = FALSE)
write.csv(con_97, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1997.csv", row.names = FALSE)
write.csv(con_98, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1998.csv", row.names = FALSE)
write.csv(con_99, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_1999.csv", row.names = FALSE)
write.csv(con_00, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_2000.csv", row.names = FALSE)
write.csv(con_06, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_2006.csv", row.names = FALSE)
write.csv(con_11, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_2011.csv", row.names = FALSE)
write.csv(con_13, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_2013.csv", row.names = FALSE)
write.csv(con_14, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_2014.csv", row.names = FALSE)
write.csv(con_17, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_2017.csv", row.names = FALSE)
write.csv(con_20, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_2020.csv", row.names = FALSE)
write.csv(con_21, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/conference_2021.csv", row.names = FALSE)

# for coach table
coach <- read_html("https://en.wikipedia.org/wiki/List_of_NHL_head_coaches")
nodes_coach <- html_nodes(coach, "table")
headcoach <- html_table(nodes_coach)[[2]]
headcoach1 <- headcoach[-1, -c(10, 17)]
colnames(headcoach1) <- c("served_team", "coach_name", "from_date", "team_games_coached", "team_wins", "team_losses", "team_ties", "team_OT/SO_losses", "team_points", "career_games_coached", "career_wins", "career_losses", "career_ties", "career_OT/SO_losses", "career_points", "professional_career")
write.csv(headcoach1, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/coach/headcoach1.csv", row.names = FALSE)
## for historical coach data from an interactive website
registered_coach <- read_html("https://records.nhl.com/registry/head-coach-regular-season")

### parse extracted string
registered_coach_json <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/coach/coach-franchise-records.json")
### change column names to avoid the leading "data." and "$" for the coach_id column
registered_coach_df <- as.data.frame(registered_coach_json)
colnames(registered_coach_df) <- sub("data.","",colnames(registered_coach_df))
colnames(registered_coach_df[, 3]) <- "coach_id"
### edit values in columns "endSeason" and "startSeason"
registered_coach_df$endSeason <- paste(substr(as.character(registered_coach_df$endSeason), 1, 4), "-", substr(as.character(registered_coach_df$endSeason), 5, 8), sep = "")
registered_coach_df$startSeason <- paste(substr(as.character(registered_coach_df$startSeason), 1, 4), "-", substr(as.character(registered_coach_df$startSeason), 5, 8), sep = "")
### drop rows that have not been active since the season of 1990
registered_coach_df_1 <- subset(registered_coach_df, as.numeric(substr(registered_coach_df$endSeason, 6, 9)) <= 1990)
registered_coach_df_2 <- registered_coach_df[setdiff(rownames(registered_coach_df), rownames(registered_coach_df_1)), ]
### export the file to csv
write.csv(registered_coach_df_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/coach/registered_coach.csv")


# for game table
##2022-23 regular
game_reg22 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg22.json")
game_reg22_venue <- as.data.frame(game_reg22$games$venue)
game_reg22_venue[, 11] <- c(1:1319)
colnames(game_reg22_venue)[11] <- "id_2"
game_reg22_home <- as.data.frame(game_reg22$games$home)
game_reg22_home[, 6] <- c(1:1319)
colnames(game_reg22_home)[6] <- "id_2"
game_reg22_away <- as.data.frame(game_reg22$games$away)
game_reg22_away[, 6] <- c(1:1319)
colnames(game_reg22_away)[6] <- "id_2"
game_reg22_game <- as.data.frame(game_reg22$games)
game_reg22_game <- game_reg22_game[, -c(9:12)]
game_reg22_game[, 10] <- c(1:1319)
colnames(game_reg22_game)[10] <- "id_2"
game_reg22_all <- merge(game_reg22_game, game_reg22_venue, by = "id_2")
game_reg22_all <- merge(game_reg22_all, game_reg22_home, by = "id_2")
game_reg22_all <- merge(game_reg22_all, game_reg22_away, by = "id_2")
game_reg22_all <- game_reg22_all%>% relocate(title, .before = status)
game_reg22_all <- game_reg22_all%>% relocate(state, .before = country)
game_reg22_all <- game_reg22_all%>% relocate(zip, .before = country)
game_reg22_all$playoff <- 0
colnames(game_reg22_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game",  "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "playoff")


##2022-23 postseason
game_pst22 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post22.json")
game_pst22_venue <- as.data.frame(game_pst22$games$venue)
game_pst22_venue[, 11] <- c(1:105)
colnames(game_pst22_venue)[11] <- "id_2"
game_pst22_home <- as.data.frame(game_pst22$games$home)
game_pst22_home[, 7] <- c(1:105)
colnames(game_pst22_home)[7] <- "id_2"
game_pst22_away <- as.data.frame(game_pst22$games$away)
game_pst22_away[, 7] <- c(1:105)
colnames(game_pst22_away)[7] <- "id_2"
game_pst22_game <- as.data.frame(game_pst22$games)
game_pst22_game <- game_pst22_game[, -c(10:13)]
game_pst22_game[, 10] <- c(1:105)
colnames(game_pst22_game)[10] <- "id_2"
game_pst22_all <- merge(game_pst22_game, game_pst22_venue, by = "id_2")
game_pst22_all <- merge(game_pst22_all, game_pst22_home, by = "id_2")
game_pst22_all <- merge(game_pst22_all, game_pst22_away, by = "id_2")
game_pst22_all$playoff <- 1
colnames(game_pst22_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "home_seed", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "away_seed", "playoff")

##combine 2022-23 regular and postseason
game22 <- rbind.fill(game_reg22_all, game_pst22_all)
##write to .csv
write.csv(game22, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game22.csv")

##data manipulation
game22_SQL <- game22
game22_SQL$home_short <- gsub("SJ", "SJS", game22_SQL$home_short)
game22_SQL$away_short <- gsub("SJ", "SJS", game22_SQL$away_short)
game22_SQL$home_short <- gsub("TB", "TBL", game22_SQL$home_short)
game22_SQL$away_short <- gsub("TB", "TBL", game22_SQL$away_short)
game22_SQL$home_short <- gsub("LA", "LAK", game22_SQL$home_short)
game22_SQL$away_short <- gsub("LA", "LAK", game22_SQL$away_short)
game22_SQL$home_short <- gsub("NJ", "NJD", game22_SQL$home_short)
game22_SQL$away_short <- gsub("NJ", "NJD", game22_SQL$away_short)
game22_SQL$home_short <- gsub("FLAK", "FLA", game22_SQL$home_short)
game22_SQL$away_short <- gsub("FLAK", "FLA", game22_SQL$away_short)
game22_SQL$winner <- ifelse(game22_SQL$home_points > game22_SQL$away_points, game22_SQL$home_short, ifelse(game22_SQL$home_points == game22_SQL$away_points, "tie", game22_SQL$away_short))
game22_SQL$winner_score <- ifelse(game22_SQL$winner == game22_SQL$home_short, game22_SQL$home_points, ifelse(game22_SQL$winner == "tie", game22_SQL$home_points, game22_SQL$away_points))
game22_SQL$rivalry <- ifelse(game22_SQL$winner == game22_SQL$home_short, game22_SQL$away_short, ifelse(game22_SQL$winner == "tie", "tie", game22_SQL$home_short))
game22_SQL$rivalry_score <- ifelse(game22_SQL$winner == game22_SQL$home_short, game22_SQL$away_points, ifelse(game22_SQL$winner == "tie", game22_SQL$home_points, game22_SQL$home_points))
game22_SQL_1 <- subset(game22_SQL, home_points != 0)
#game22_SQL_1$playoff[grepl("all-star", tolower(game22_SQL_1$game_title))] <- 0
game22_SQL_1 <- subset(game22_SQL_1, !grepl("all-star", tolower(game_title)))
game22_SQL_2 <- subset(game22_SQL_1, select = -c(1:5, 7:9, 11:22, 24:27, 29:30, 32:33))
game22_SQL_2 <- game22_SQL_2 %>% relocate(reference_game, .before = scheduled)
game22_SQL_2 <- game22_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game22_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game22_cleaned.csv")
game22_venue <- game22_SQL_1[, c(11:20)]
write.csv(game22_venue, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game22_venue.csv")

##2021-22 regular
game_reg21 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg21.json")
game_reg21_venue <- as.data.frame(game_reg21$games$venue)
game_reg21_venue[, 11] <- c(1:1417)
colnames(game_reg21_venue)[11] <- "id_2"
game_reg21_home <- as.data.frame(game_reg21$games$home)
game_reg21_home[, 6] <- c(1:1417)
colnames(game_reg21_home)[6] <- "id_2"
game_reg21_away <- as.data.frame(game_reg21$games$away)
game_reg21_away[, 6] <- c(1:1417)
colnames(game_reg21_away)[6] <- "id_2"
game_reg21_game <- as.data.frame(game_reg21$games)
game_reg21_game <- game_reg21_game[, -c(9:12)]
game_reg21_game[, 10] <- c(1:1417)
colnames(game_reg21_game)[10] <- "id_2"
game_reg21_all <- merge(game_reg21_game, game_reg21_venue, by = "id_2")
game_reg21_all <- merge(game_reg21_all, game_reg21_home, by = "id_2")
game_reg21_all <- merge(game_reg21_all, game_reg21_away, by = "id_2")
game_reg21_all <- game_reg21_all%>% relocate(title, .before = status)
game_reg21_all$playoff <- 0
colnames(game_reg21_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "playoff")


##2021-22 postseason
game_pst21 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post21.json")
game_pst21_venue <- as.data.frame(game_pst21$games$venue)
game_pst21_venue[, 11] <- c(1:105)
colnames(game_pst21_venue)[11] <- "id_2"
game_pst21_home <- as.data.frame(game_pst21$games$home)
game_pst21_home[, 6] <- c(1:105)
colnames(game_pst21_home)[6] <- "id_2"
game_pst21_away <- as.data.frame(game_pst21$games$away)
game_pst21_away[, 6] <- c(1:105)
colnames(game_pst21_away)[6] <- "id_2"
game_pst21_game <- as.data.frame(game_pst21$games)
game_pst21_game <- game_pst21_game[, -c(10:13)]
game_pst21_game[, 10] <- c(1:105)
colnames(game_pst21_game)[10] <- "id_2"
game_pst21_all <- merge(game_pst21_game, game_pst21_venue, by = "id_2")
game_pst21_all <- merge(game_pst21_all, game_pst21_home, by = "id_2")
game_pst21_all <- merge(game_pst21_all, game_pst21_away, by = "id_2")
game_pst21_all$playoff <- 1
colnames(game_pst21_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "playoff")

##combine 2021-22 regular and postseason
game21 <- rbind.fill(game_reg21_all, game_pst21_all)
##write to .csv
write.csv(game21, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game21.csv")

##data manipulation
game21_SQL <- game21
game21_SQL$home_short <- gsub("SJ", "SJS", game21_SQL$home_short)
game21_SQL$away_short <- gsub("SJ", "SJS", game21_SQL$away_short)
game21_SQL$home_short <- gsub("TB", "TBL", game21_SQL$home_short)
game21_SQL$away_short <- gsub("TB", "TBL", game21_SQL$away_short)
game21_SQL$home_short <- gsub("LA", "LAK", game21_SQL$home_short)
game21_SQL$away_short <- gsub("LA", "LAK", game21_SQL$away_short)
game21_SQL$home_short <- gsub("NJ", "NJD", game21_SQL$home_short)
game21_SQL$away_short <- gsub("NJ", "NJD", game21_SQL$away_short)
game21_SQL$home_short <- gsub("FLAK", "FLA", game21_SQL$home_short)
game21_SQL$away_short <- gsub("FLAK", "FLA", game21_SQL$away_short)
game21_SQL$winner <- ifelse(game21_SQL$home_points > game21_SQL$away_points, game21_SQL$home_short, ifelse(game21_SQL$home_points == game21_SQL$away_points, "tie", game21_SQL$away_short))
game21_SQL$winner_score <- ifelse(game21_SQL$winner == game21_SQL$home_short, game21_SQL$home_points, ifelse(game21_SQL$winner == "tie", game21_SQL$home_points, game21_SQL$away_points))
game21_SQL$rivalry <- ifelse(game21_SQL$winner == game21_SQL$home_short, game21_SQL$away_short, ifelse(game21_SQL$winner == "tie", "tie", game21_SQL$home_short))
game21_SQL$rivalry_score <- ifelse(game21_SQL$winner == game21_SQL$home_short, game21_SQL$away_points, ifelse(game21_SQL$winner == "tie", game21_SQL$home_points, game21_SQL$home_points))
game21_SQL_1 <- subset(game21_SQL, home_points != 0)
#game22_SQL_1$playoff[grepl("all-star", tolower(game22_SQL_1$game_title))] <- 0
game21_SQL_1 <- subset(game21_SQL_1, !grepl("all-star", tolower(game_title)))
game21_SQL_2 <- subset(game21_SQL_1, select = -c(1:5, 7:9, 11:22, 24:27, 29:30))
game21_SQL_2 <- game21_SQL_2 %>% relocate(reference_game, .before = scheduled)
game21_SQL_2 <- game21_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game21_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game21_cleaned.csv")


##2020-21 regular
game_reg20 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg20.json")
game_reg20_venue <- as.data.frame(game_reg20$games$venue)
game_reg20_venue[, 11] <- c(1:928)
colnames(game_reg20_venue)[11] <- "id_2"
game_reg20_home <- as.data.frame(game_reg20$games$home)
game_reg20_home[, 6] <- c(1:928)
colnames(game_reg20_home)[6] <- "id_2"
game_reg20_away <- as.data.frame(game_reg20$games$away)
game_reg20_away[, 6] <- c(1:928)
colnames(game_reg20_away)[6] <- "id_2"
game_reg20_game <- as.data.frame(game_reg20$games)
game_reg20_game <- game_reg20_game[, -c(9:12)]
game_reg20_game[, 9] <- c(1:928)
colnames(game_reg20_game)[9] <- "id_2"
game_reg20_all <- merge(game_reg20_game, game_reg20_venue, by = "id_2")
game_reg20_all <- merge(game_reg20_all, game_reg20_home, by = "id_2")
game_reg20_all <- merge(game_reg20_all, game_reg20_away, by = "id_2")
game_reg20_all$playoff <- 0
colnames(game_reg20_all) <- c("id_2", "game_id", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "playoff")

##2020-21 postseason
game_pst20 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post20.json")
game_pst20_venue <- as.data.frame(game_pst20$games$venue)
game_pst20_venue[, 11] <- c(1:105)
colnames(game_pst20_venue)[11] <- "id_2"
game_pst20_home <- as.data.frame(game_pst20$games$home)
game_pst20_home[, 7] <- c(1:105)
colnames(game_pst20_home)[7] <- "id_2"
game_pst20_away <- as.data.frame(game_pst20$games$away)
game_pst20_away[, 7] <- c(1:105)
colnames(game_pst20_away)[7] <- "id_2"
game_pst20_game <- as.data.frame(game_pst20$games)
game_pst20_game <- game_pst20_game[, -c(10:13)]
game_pst20_game[, 10] <- c(1:105)
colnames(game_pst20_game)[10] <- "id_2"
game_pst20_all <- merge(game_pst20_game, game_pst20_venue, by = "id_2")
game_pst20_all <- merge(game_pst20_all, game_pst20_home, by = "id_2")
game_pst20_all <- merge(game_pst20_all, game_pst20_away, by = "id_2")
game_pst20_all <- game_pst20_all%>% relocate(seed.x, .after = reference.y)
game_pst20_all <- game_pst20_all%>% relocate(seed.y, .after = reference)
game_pst20_all$playoff <- 1
colnames(game_pst20_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "seed_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "seed_away", "playoff")

##combine 2020-21 regular and postseason
game20 <- rbind.fill(game_reg20_all, game_pst20_all)
game20 <- game20 %>% relocate(game_title, .before = status)
##write to .csv
write.csv(game20, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game20.csv")

##data manipulation
game20_SQL <- game20
game20_SQL$home_short <- gsub("SJ", "SJS", game20_SQL$home_short)
game20_SQL$away_short <- gsub("SJ", "SJS", game20_SQL$away_short)
game20_SQL$home_short <- gsub("TB", "TBL", game20_SQL$home_short)
game20_SQL$away_short <- gsub("TB", "TBL", game20_SQL$away_short)
game20_SQL$home_short <- gsub("LA", "LAK", game20_SQL$home_short)
game20_SQL$away_short <- gsub("LA", "LAK", game20_SQL$away_short)
game20_SQL$home_short <- gsub("NJ", "NJD", game20_SQL$home_short)
game20_SQL$away_short <- gsub("NJ", "NJD", game20_SQL$away_short)
game20_SQL$home_short <- gsub("FLAK", "FLA", game20_SQL$home_short)
game20_SQL$away_short <- gsub("FLAK", "FLA", game20_SQL$away_short)
game20_SQL$winner <- ifelse(game20_SQL$home_points > game20_SQL$away_points, game20_SQL$home_short, ifelse(game20_SQL$home_points == game20_SQL$away_points, "tie", game20_SQL$away_short))
game20_SQL$winner_score <- ifelse(game20_SQL$winner == game20_SQL$home_short, game20_SQL$home_points, ifelse(game20_SQL$winner == "tie", game20_SQL$home_points, game20_SQL$away_points))
game20_SQL$rivalry <- ifelse(game20_SQL$winner == game20_SQL$home_short, game20_SQL$away_short, ifelse(game20_SQL$winner == "tie", "tie", game20_SQL$home_short))
game20_SQL$rivalry_score <- ifelse(game20_SQL$winner == game20_SQL$home_short, game20_SQL$away_points, ifelse(game20_SQL$winner == "tie", game20_SQL$home_points, game20_SQL$home_points))
game20_SQL_1 <- subset(game20_SQL, home_points != 0)
#game22_SQL_1$playoff[grepl("all-star", tolower(game22_SQL_1$game_title))] <- 0
game20_SQL_1 <- subset(game20_SQL_1, !grepl("all-star", tolower(game_title)))
game20_SQL_2 <- subset(game20_SQL_1, select = -c(1:5, 7:9, 11:22, 24:27, 29:30, 32:33))
game20_SQL_2 <- game20_SQL_2 %>% relocate(reference_game, .before = scheduled)
game20_SQL_2 <- game20_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game20_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game20_cleaned.csv")


##2019-20 regular
game_reg19 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg19.json")
game_reg19_venue <- as.data.frame(game_reg19$games$venue)
game_reg19_venue[, 11] <- c(1:1275)
colnames(game_reg19_venue)[11] <- "id_2"
game_reg19_home <- as.data.frame(game_reg19$games$home)
game_reg19_home[, 6] <- c(1:1275)
colnames(game_reg19_home)[6] <- "id_2"
game_reg19_away <- as.data.frame(game_reg19$games$away)
game_reg19_away[, 6] <- c(1:1275)
colnames(game_reg19_away)[6] <- "id_2"
game_reg19_game <- as.data.frame(game_reg19$games)
game_reg19_game <- game_reg19_game[, -c(9:12)]
game_reg19_game[, 10] <- c(1:1275)
colnames(game_reg19_game)[10] <- "id_2"
game_reg19_all <- merge(game_reg19_game, game_reg19_venue, by = "id_2")
game_reg19_all <- merge(game_reg19_all, game_reg19_home, by = "id_2")
game_reg19_all <- merge(game_reg19_all, game_reg19_away, by = "id_2")
game_reg19_all <- game_reg19_all %>% relocate(title, .before = status)
game_reg19_all <- game_reg19_all %>% relocate(zip, .before = country)
game_reg19_all$playoff <- 0
colnames(game_reg19_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "playoff")

##2019-20 postseason
game_pst19 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post19.json")
game_pst19_venue <- as.data.frame(game_pst19$games$venue)
game_pst19_venue[, 10] <- c(1:162)
colnames(game_pst19_venue)[10] <- "id_2"
game_pst19_home <- as.data.frame(game_pst19$games$home)
game_pst19_home[, 7] <- c(1:162)
colnames(game_pst19_home)[7] <- "id_2"
game_pst19_away <- as.data.frame(game_pst19$games$away)
game_pst19_away[, 7] <- c(1:162)
colnames(game_pst19_away)[7] <- "id_2"
game_pst19_game <- as.data.frame(game_pst19$games)
game_pst19_game <- game_pst19_game[, -c(10:13)]
game_pst19_game[, 10] <- c(1:162)
colnames(game_pst19_game)[10] <- "id_2"
game_pst19_all <- merge(game_pst19_game, game_pst19_venue, by = "id_2")
game_pst19_all <- merge(game_pst19_all, game_pst19_home, by = "id_2")
game_pst19_all <- merge(game_pst19_all, game_pst19_away, by = "id_2")
game_pst19_all <- game_pst19_all%>% relocate(seed.x, .after = reference.y)
game_pst19_all <- game_pst19_all%>% relocate(seed.y, .after = reference)
game_pst19_all$playoff <- 1
colnames(game_pst19_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "seed_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "seed_away", "playoff")

##combine 2019-20 regular and postseason
game19 <- rbind.fill(game_reg19_all, game_pst19_all)
##write to .csv
write.csv(game19, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game19.csv")

##data manipulation
game19_SQL <- game19
game19_SQL$home_short <- gsub("SJ", "SJS", game19_SQL$home_short)
game19_SQL$away_short <- gsub("SJ", "SJS", game19_SQL$away_short)
game19_SQL$home_short <- gsub("TB", "TBL", game19_SQL$home_short)
game19_SQL$away_short <- gsub("TB", "TBL", game19_SQL$away_short)
game19_SQL$home_short <- gsub("LA", "LAK", game19_SQL$home_short)
game19_SQL$away_short <- gsub("LA", "LAK", game19_SQL$away_short)
game19_SQL$home_short <- gsub("NJ", "NJD", game19_SQL$home_short)
game19_SQL$away_short <- gsub("NJ", "NJD", game19_SQL$away_short)
game19_SQL$home_short <- gsub("FLAK", "FLA", game19_SQL$home_short)
game19_SQL$away_short <- gsub("FLAK", "FLA", game19_SQL$away_short)
game19_SQL$winner <- ifelse(game19_SQL$home_points > game19_SQL$away_points, game19_SQL$home_short, ifelse(game19_SQL$home_points == game19_SQL$away_points, "tie", game19_SQL$away_short))
game19_SQL$winner_score <- ifelse(game19_SQL$winner == game19_SQL$home_short, game19_SQL$home_points, ifelse(game19_SQL$winner == "tie", game19_SQL$home_points, game19_SQL$away_points))
game19_SQL$rivalry <- ifelse(game19_SQL$winner == game19_SQL$home_short, game19_SQL$away_short, ifelse(game19_SQL$winner == "tie", "tie", game19_SQL$home_short))
game19_SQL$rivalry_score <- ifelse(game19_SQL$winner == game19_SQL$home_short, game19_SQL$away_points, ifelse(game19_SQL$winner == "tie", game19_SQL$home_points, game19_SQL$home_points))
game19_SQL_1 <- subset(game19_SQL, home_points != 0)
game19_SQL_1 <- subset(game19_SQL_1, !grepl("all-star", tolower(game_title)))
game19_SQL_2 <- subset(game19_SQL_1, select = -c(1:5, 7:9, 11:22, 24:27, 29:30, 32:33))
game19_SQL_2 <- game19_SQL_2 %>% relocate(reference_game, .before = scheduled)
game19_SQL_2 <- game19_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game19_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game19_cleaned.csv")


##2018-19 regular
game_reg18 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg18.json")
game_reg18_venue <- as.data.frame(game_reg18$games$venue)
game_reg18_venue[, 11] <- c(1:1274)
colnames(game_reg18_venue)[11] <- "id_2"
game_reg18_home <- as.data.frame(game_reg18$games$home)
game_reg18_home[, 6] <- c(1:1274)
colnames(game_reg18_home)[6] <- "id_2"
game_reg18_away <- as.data.frame(game_reg18$games$away)
game_reg18_away[, 6] <- c(1:1274)
colnames(game_reg18_away)[6] <- "id_2"
game_reg18_game <- as.data.frame(game_reg18$games)
game_reg18_game <- game_reg18_game[, -c(9:12)]
game_reg18_game[, 10] <- c(1:1274)
colnames(game_reg18_game)[10] <- "id_2"
game_reg18_all <- merge(game_reg18_game, game_reg18_venue, by = "id_2")
game_reg18_all <- merge(game_reg18_all, game_reg18_home, by = "id_2")
game_reg18_all <- merge(game_reg18_all, game_reg18_away, by = "id_2")
game_reg18_all <- game_reg18_all %>% relocate(title, .before = status)
game_reg18_all <- game_reg18_all %>% relocate(zip, .before = country)
game_reg18_all$playoff <- 0
colnames(game_reg18_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "playoff")

##2018-19 postseason
game_pst18 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post18.json")
game_pst18_venue <- as.data.frame(game_pst18$games$venue)
game_pst18_venue[, 11] <- c(1:105)
colnames(game_pst18_venue)[11] <- "id_2"
game_pst18_home <- as.data.frame(game_pst18$games$home)
game_pst18_home[, 7] <- c(1:105)
colnames(game_pst18_home)[7] <- "id_2"
game_pst18_away <- as.data.frame(game_pst18$games$away)
game_pst18_away[, 7] <- c(1:105)
colnames(game_pst18_away)[7] <- "id_2"
game_pst18_game <- as.data.frame(game_pst18$games)
game_pst18_game <- game_pst18_game[, -c(10:13)]
game_pst18_game[, 10] <- c(1:105)
colnames(game_pst18_game)[10] <- "id_2"
game_pst18_all <- merge(game_pst18_game, game_pst18_venue, by = "id_2")
game_pst18_all <- merge(game_pst18_all, game_pst18_home, by = "id_2")
game_pst18_all <- merge(game_pst18_all, game_pst18_away, by = "id_2")
game_pst18_all <- game_pst18_all%>% relocate(seed.x, .after = reference.y)
game_pst18_all <- game_pst18_all%>% relocate(seed.y, .after = reference)
game_pst18_all$playoff <- 1
colnames(game_pst18_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "seed_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "seed_away", "playoff")

##combine 2018-19 regular and postseason
game18 <- rbind.fill(game_reg18_all, game_pst18_all)
##write to .csv
write.csv(game18, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game18.csv")

##data manipulation
game18_SQL <- game18
game18_SQL$home_short <- gsub("SJ", "SJS", game18_SQL$home_short)
game18_SQL$away_short <- gsub("SJ", "SJS", game18_SQL$away_short)
game18_SQL$home_short <- gsub("TB", "TBL", game18_SQL$home_short)
game18_SQL$away_short <- gsub("TB", "TBL", game18_SQL$away_short)
game18_SQL$home_short <- gsub("LA", "LAK", game18_SQL$home_short)
game18_SQL$away_short <- gsub("LA", "LAK", game18_SQL$away_short)
game18_SQL$home_short <- gsub("NJ", "NJD", game18_SQL$home_short)
game18_SQL$away_short <- gsub("NJ", "NJD", game18_SQL$away_short)
game18_SQL$home_short <- gsub("FLAK", "FLA", game18_SQL$home_short)
game18_SQL$away_short <- gsub("FLAK", "FLA", game18_SQL$away_short)
game18_SQL$winner <- ifelse(game18_SQL$home_points > game18_SQL$away_points, game18_SQL$home_short, ifelse(game18_SQL$home_points == game18_SQL$away_points, "tie", game18_SQL$away_short))
game18_SQL$winner_score <- ifelse(game18_SQL$winner == game18_SQL$home_short, game18_SQL$home_points, ifelse(game18_SQL$winner == "tie", game18_SQL$home_points, game18_SQL$away_points))
game18_SQL$rivalry <- ifelse(game18_SQL$winner == game18_SQL$home_short, game18_SQL$away_short, ifelse(game18_SQL$winner == "tie", "tie", game18_SQL$home_short))
game18_SQL$rivalry_score <- ifelse(game18_SQL$winner == game18_SQL$home_short, game18_SQL$away_points, ifelse(game18_SQL$winner == "tie", game18_SQL$home_points, game18_SQL$home_points))
game18_SQL_1 <- subset(game18_SQL, home_points != 0)
game18_SQL_1 <- subset(game18_SQL_1, !grepl("all-star", tolower(game_title)))
game18_SQL_2 <- subset(game18_SQL_1, select = -c(1:5, 7:9, 11:22, 24:27, 29:30, 32:33))
game18_SQL_2 <- game18_SQL_2 %>% relocate(reference_game, .before = scheduled)
game18_SQL_2 <- game18_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game18_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game18_cleaned.csv")


##2017-18 regular
game_reg17 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg17.json")
game_reg17_venue <- as.data.frame(game_reg17$games$venue)
game_reg17_venue[, 11] <- c(1:1275)
colnames(game_reg17_venue)[11] <- "id_2"
game_reg17_home <- as.data.frame(game_reg17$games$home)
game_reg17_home[, 6] <- c(1:1275)
colnames(game_reg17_home)[6] <- "id_2"
game_reg17_away <- as.data.frame(game_reg17$games$away)
game_reg17_away[, 6] <- c(1:1275)
colnames(game_reg17_away)[6] <- "id_2"
game_reg17_game <- as.data.frame(game_reg17$games)
game_reg17_game <- game_reg17_game[, -c(9:11)]
game_reg17_game[, 10] <- c(1:1275)
colnames(game_reg17_game)[10] <- "id_2"
game_reg17_all <- merge(game_reg17_game, game_reg17_venue, by = "id_2")
game_reg17_all <- merge(game_reg17_all, game_reg17_home, by = "id_2")
game_reg17_all <- merge(game_reg17_all, game_reg17_away, by = "id_2")
game_reg17_all <- game_reg17_all %>% relocate(title, .before = status)
game_reg17_all$playoff <- 0
colnames(game_reg17_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "playoff")

##2017-18 postseason
game_pst17 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post17.json")
game_pst17_venue <- as.data.frame(game_pst17$games$venue)
game_pst17_venue[, 11] <- c(1:105)
colnames(game_pst17_venue)[11] <- "id_2"
game_pst17_home <- as.data.frame(game_pst17$games$home)
game_pst17_home[, 7] <- c(1:105)
colnames(game_pst17_home)[7] <- "id_2"
game_pst17_away <- as.data.frame(game_pst17$games$away)
game_pst17_away[, 7] <- c(1:105)
colnames(game_pst17_away)[7] <- "id_2"
game_pst17_game <- as.data.frame(game_pst17$games)
game_pst17_game <- game_pst17_game[, -c(10:12)]
game_pst17_game[, 10] <- c(1:105)
colnames(game_pst17_game)[10] <- "id_2"
game_pst17_all <- merge(game_pst17_game, game_pst17_venue, by = "id_2")
game_pst17_all <- merge(game_pst17_all, game_pst17_home, by = "id_2")
game_pst17_all <- merge(game_pst17_all, game_pst17_away, by = "id_2")
game_pst17_all <- game_pst17_all%>% relocate(seed.x, .after = reference.y)
game_pst17_all <- game_pst17_all%>% relocate(seed.y, .after = reference)
game_pst17_all$playoff <- 1
colnames(game_pst17_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "reference_home", "seed_home", "away_id", "away_name", "away_short", "sr_id_away", "reference_away", "seed_away", "playoff")

##combine 2017-18regular and postseason
game17 <- rbind.fill(game_reg17_all, game_pst17_all)
##write to .csv
write.csv(game17, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game17.csv")

##data manipulation
game17_SQL <- game17
game17_SQL$home_short <- gsub("SJ", "SJS", game17_SQL$home_short)
game17_SQL$away_short <- gsub("SJ", "SJS", game17_SQL$away_short)
game17_SQL$home_short <- gsub("TB", "TBL", game17_SQL$home_short)
game17_SQL$away_short <- gsub("TB", "TBL", game17_SQL$away_short)
game17_SQL$home_short <- gsub("LA", "LAK", game17_SQL$home_short)
game17_SQL$away_short <- gsub("LA", "LAK", game17_SQL$away_short)
game17_SQL$home_short <- gsub("NJ", "NJD", game17_SQL$home_short)
game17_SQL$away_short <- gsub("NJ", "NJD", game17_SQL$away_short)
game17_SQL$home_short <- gsub("FLAK", "FLA", game17_SQL$home_short)
game17_SQL$away_short <- gsub("FLAK", "FLA", game17_SQL$away_short)
game17_SQL$winner <- ifelse(game17_SQL$home_points > game17_SQL$away_points, game17_SQL$home_short, ifelse(game17_SQL$home_points == game17_SQL$away_points, "tie", game17_SQL$away_short))
game17_SQL$winner_score <- ifelse(game17_SQL$winner == game17_SQL$home_short, game17_SQL$home_points, ifelse(game17_SQL$winner == "tie", game17_SQL$home_points, game17_SQL$away_points))
game17_SQL$rivalry <- ifelse(game17_SQL$winner == game17_SQL$home_short, game17_SQL$away_short, ifelse(game17_SQL$winner == "tie", "tie", game17_SQL$home_short))
game17_SQL$rivalry_score <- ifelse(game17_SQL$winner == game17_SQL$home_short, game17_SQL$away_points, ifelse(game17_SQL$winner == "tie", game17_SQL$home_points, game17_SQL$home_points))
game17_SQL_1 <- subset(game17_SQL, home_points != 0)
game17_SQL_1 <- subset(game17_SQL_1, !grepl("all-star", tolower(game_title)))
game17_SQL_2 <- subset(game17_SQL_1, select = -c(1:5, 7:9, 11:22, 24:27, 29:30, 32:33))
game17_SQL_2 <- game17_SQL_2 %>% relocate(reference_game, .before = scheduled)
game17_SQL_2 <- game17_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game17_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game17_cleaned.csv")

##2016-17 regular
game_reg16 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg16.json")
game_reg16_venue <- as.data.frame(game_reg16$games$venue)
game_reg16_venue[, 11] <- c(1:1235)
colnames(game_reg16_venue)[11] <- "id_2"
game_reg16_home <- as.data.frame(game_reg16$games$home)
game_reg16_home[, 5] <- c(1:1235)
colnames(game_reg16_home)[5] <- "id_2"
game_reg16_away <- as.data.frame(game_reg16$games$away)
game_reg16_away[, 5] <- c(1:1235)
colnames(game_reg16_away)[5] <- "id_2"
game_reg16_game <- as.data.frame(game_reg16$games)
game_reg16_game <- game_reg16_game[, -c(9:12)]
game_reg16_game[, 10] <- c(1:1235)
colnames(game_reg16_game)[10] <- "id_2"
game_reg16_all <- merge(game_reg16_game, game_reg16_venue, by = "id_2")
game_reg16_all <- merge(game_reg16_all, game_reg16_home, by = "id_2")
game_reg16_all <- merge(game_reg16_all, game_reg16_away, by = "id_2")
game_reg16_all <- game_reg16_all %>% relocate(title, .before = status)
game_reg16_all <- game_reg16_all %>% relocate(zip, .before = country)
game_reg16_all$playoff <- 0
colnames(game_reg16_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "away_id", "away_name", "away_short", "sr_id_away", "playoff")

##2016-17 postseason
game_pst16 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post16.json")
game_pst16_venue <- as.data.frame(game_pst16$games$venue)
game_pst16_venue[, 11] <- c(1:105)
colnames(game_pst16_venue)[11] <- "id_2"
game_pst16_home <- as.data.frame(game_pst16$games$home)
game_pst16_home[, 6] <- c(1:105)
colnames(game_pst16_home)[6] <- "id_2"
game_pst16_away <- as.data.frame(game_pst16$games$away)
game_pst16_away[, 6] <- c(1:105)
colnames(game_pst16_away)[6] <- "id_2"
game_pst16_game <- as.data.frame(game_pst16$games)
game_pst16_game <- game_pst16_game[, -c(10:12)]
game_pst16_game[, 10] <- c(1:105)
colnames(game_pst16_game)[10] <- "id_2"
game_pst16_all <- merge(game_pst16_game, game_pst16_venue, by = "id_2")
game_pst16_all <- merge(game_pst16_all, game_pst16_home, by = "id_2")
game_pst16_all <- merge(game_pst16_all, game_pst16_away, by = "id_2")
game_pst16_all$playoff <- 1
colnames(game_pst16_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "home_points", "away_points", "sr_id_game", "reference_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "seed_home", "sr_id_home", "away_id", "away_name", "away_short", "seed_away", "sr_id_away", "playoff")

##combine 2016-17 regular and postseason
game16 <- rbind.fill(game_reg16_all, game_pst16_all)
##write to .csv
write.csv(game16, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game16.csv")

##data manipulation
game16_SQL <- game16
game16_SQL$home_short <- gsub("SJ", "SJS", game16_SQL$home_short)
game16_SQL$away_short <- gsub("SJ", "SJS", game16_SQL$away_short)
game16_SQL$home_short <- gsub("TB", "TBL", game16_SQL$home_short)
game16_SQL$away_short <- gsub("TB", "TBL", game16_SQL$away_short)
game16_SQL$home_short <- gsub("LA", "LAK", game16_SQL$home_short)
game16_SQL$away_short <- gsub("LA", "LAK", game16_SQL$away_short)
game16_SQL$home_short <- gsub("NJ", "NJD", game16_SQL$home_short)
game16_SQL$away_short <- gsub("NJ", "NJD", game16_SQL$away_short)
game16_SQL$home_short <- gsub("FLAK", "FLA", game16_SQL$home_short)
game16_SQL$away_short <- gsub("FLAK", "FLA", game16_SQL$away_short)
game16_SQL$winner <- ifelse(game16_SQL$home_points > game16_SQL$away_points, game16_SQL$home_short, ifelse(game16_SQL$home_points == game16_SQL$away_points, "tie", game16_SQL$away_short))
game16_SQL$winner_score <- ifelse(game16_SQL$winner == game16_SQL$home_short, game16_SQL$home_points, ifelse(game16_SQL$winner == "tie", game16_SQL$home_points, game16_SQL$away_points))
game16_SQL$rivalry <- ifelse(game16_SQL$winner == game16_SQL$home_short, game16_SQL$away_short, ifelse(game16_SQL$winner == "tie", "tie", game16_SQL$home_short))
game16_SQL$rivalry_score <- ifelse(game16_SQL$winner == game16_SQL$home_short, game16_SQL$away_points, ifelse(game16_SQL$winner == "tie", game16_SQL$home_points, game16_SQL$home_points))
game16_SQL_1 <- subset(game16_SQL, home_points != 0)
game16_SQL_1 <- subset(game16_SQL_1, !grepl("all-star", tolower(game_title)))
game16_SQL_2 <- subset(game16_SQL_1, select = -c(1:5, 7:9, 11:22, 24:26, 28, 30:31))
game16_SQL_2 <- game16_SQL_2 %>% relocate(reference_game, .before = scheduled)
game16_SQL_2 <- game16_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game16_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game16_cleaned.csv")


##2015-16 regular
game_reg15 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg15.json")
game_reg15_venue <- as.data.frame(game_reg15$games$venue)
game_reg15_venue[, 11] <- c(1:1236)
colnames(game_reg15_venue)[11] <- "id_2"
game_reg15_home <- as.data.frame(game_reg15$games$home)
game_reg15_home[, 5] <- c(1:1236)
colnames(game_reg15_home)[5] <- "id_2"
game_reg15_away <- as.data.frame(game_reg15$games$away)
game_reg15_away[, 5] <- c(1:1236)
colnames(game_reg15_away)[5] <- "id_2"
game_reg15_game <- as.data.frame(game_reg15$games)
game_reg15_game <- game_reg15_game[, -c(6:9)]
game_reg15_game[, 9] <- c(1:1236)
colnames(game_reg15_game)[9] <- "id_2"
game_reg15_all <- merge(game_reg15_game, game_reg15_venue, by = "id_2")
game_reg15_all <- merge(game_reg15_all, game_reg15_home, by = "id_2")
game_reg15_all <- merge(game_reg15_all, game_reg15_away, by = "id_2")
game_reg15_all <- game_reg15_all %>% relocate(title, .before = status)
game_reg15_all <- game_reg15_all %>% relocate(zip, .before = country)
game_reg15_all$playoff <- 0
colnames(game_reg15_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled",  "sr_id_game", "home_points", "away_points", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "away_id", "away_name", "away_short", "sr_id_away", "playoff")

##2015-16 postseason
game_pst15 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post15.json")
game_pst15_venue <- as.data.frame(game_pst15$games$venue)
game_pst15_venue[, 11] <- c(1:105)
colnames(game_pst15_venue)[11] <- "id_2"
game_pst15_home <- as.data.frame(game_pst15$games$home)
game_pst15_home[, 6] <- c(1:105)
colnames(game_pst15_home)[6] <- "id_2"
game_pst15_away <- as.data.frame(game_pst15$games$away)
game_pst15_away[, 6] <- c(1:105)
colnames(game_pst15_away)[6] <- "id_2"
game_pst15_game <- as.data.frame(game_pst15$games)
game_pst15_game <- game_pst15_game[, -c(7:9)]
game_pst15_game[, 7] <- c(1:105)
colnames(game_pst15_game)[7] <- "id_2"
game_pst15_all <- merge(game_pst15_game, game_pst15_venue, by = "id_2")
game_pst15_all <- merge(game_pst15_all, game_pst15_home, by = "id_2")
game_pst15_all <- merge(game_pst15_all, game_pst15_away, by = "id_2")
game_pst15_all$playoff <- 1
colnames(game_pst15_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "sr_id_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "seed_home", "sr_id_home", "away_id", "away_name", "away_short", "seed_away", "sr_id_away", "playoff")

##combine 2015-16 regular and postseason
game15 <- rbind.fill(game_reg15_all, game_pst15_all)
##write to .csv
write.csv(game15, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game15.csv")

##data manipulation
game15_SQL <- game15
game15_SQL$home_short <- gsub("SJ", "SJS", game15_SQL$home_short)
game15_SQL$away_short <- gsub("SJ", "SJS", game15_SQL$away_short)
game15_SQL$home_short <- gsub("TB", "TBL", game15_SQL$home_short)
game15_SQL$away_short <- gsub("TB", "TBL", game15_SQL$away_short)
game15_SQL$home_short <- gsub("LA", "LAK", game15_SQL$home_short)
game15_SQL$away_short <- gsub("LA", "LAK", game15_SQL$away_short)
game15_SQL$home_short <- gsub("NJ", "NJD", game15_SQL$home_short)
game15_SQL$away_short <- gsub("NJ", "NJD", game15_SQL$away_short)
game15_SQL$home_short <- gsub("FLAK", "FLA", game15_SQL$home_short)
game15_SQL$away_short <- gsub("FLAK", "FLA", game15_SQL$away_short)
game15_SQL$winner <- ifelse(game15_SQL$home_points > game15_SQL$away_points, game15_SQL$home_short, ifelse(game15_SQL$home_points == game15_SQL$away_points, "tie", game15_SQL$away_short))
game15_SQL$winner_score <- ifelse(game15_SQL$winner == game15_SQL$home_short, game15_SQL$home_points, ifelse(game15_SQL$winner == "tie", game15_SQL$home_points, game15_SQL$away_points))
game15_SQL$rivalry <- ifelse(game15_SQL$winner == game15_SQL$home_short, game15_SQL$away_short, ifelse(game15_SQL$winner == "tie", "tie", game15_SQL$home_short))
game15_SQL$rivalry_score <- ifelse(game15_SQL$winner == game15_SQL$home_short, game15_SQL$away_points, ifelse(game15_SQL$winner == "tie", game15_SQL$home_points, game15_SQL$home_points))
game15_SQL_1 <- subset(game15_SQL, home_points != 0)
game15_SQL_1 <- subset(game15_SQL_1, !grepl("all-star", tolower(game_title)))
game15_SQL_2 <- subset(game15_SQL_1, select = -c(1:5, 7:21, 23:25, 27, 29:30))
game15_SQL_2 <- game15_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game15_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game15_cleaned.csv")


##2014-15 regular
game_reg14 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg14.json")
game_reg14_venue <- as.data.frame(game_reg14$games$venue)
game_reg14_venue[, 11] <- c(1:1233)
colnames(game_reg14_venue)[11] <- "id_2"
game_reg14_home <- as.data.frame(game_reg14$games$home)
game_reg14_home[, 5] <- c(1:1233)
colnames(game_reg14_home)[5] <- "id_2"
game_reg14_away <- as.data.frame(game_reg14$games$away)
game_reg14_away[, 5] <- c(1:1233)
colnames(game_reg14_away)[5] <- "id_2"
game_reg14_game <- as.data.frame(game_reg14$games)
game_reg14_game <- game_reg14_game[, -c(6:9)]
game_reg14_game[, 9] <- c(1:1233)
colnames(game_reg14_game)[9] <- "id_2"
game_reg14_all <- merge(game_reg14_game, game_reg14_venue, by = "id_2")
game_reg14_all <- merge(game_reg14_all, game_reg14_home, by = "id_2")
game_reg14_all <- merge(game_reg14_all, game_reg14_away, by = "id_2")
game_reg14_all <- game_reg14_all %>% relocate(title, .before = status)
game_reg14_all <- game_reg14_all %>% relocate(zip, .before = country)
game_reg14_all$playoff <- 0
colnames(game_reg14_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled",  "sr_id_game", "home_points", "away_points", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "away_id", "away_name", "away_short", "sr_id_away", "playoff")

##2014-15 postseason
game_pst14 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post14.json")
game_pst14_venue <- as.data.frame(game_pst14$games$venue)
game_pst14_venue[, 11] <- c(1:105)
colnames(game_pst14_venue)[11] <- "id_2"
game_pst14_home <- as.data.frame(game_pst14$games$home)
game_pst14_home[, 6] <- c(1:105)
colnames(game_pst14_home)[6] <- "id_2"
game_pst14_away <- as.data.frame(game_pst14$games$away)
game_pst14_away[, 6] <- c(1:105)
colnames(game_pst14_away)[6] <- "id_2"
game_pst14_game <- as.data.frame(game_pst14$games)
game_pst14_game <- game_pst14_game[, -c(7:10)]
game_pst14_game[, 9] <- c(1:105)
colnames(game_pst14_game)[9] <- "id_2"
game_pst14_all <- merge(game_pst14_game, game_pst14_venue, by = "id_2")
game_pst14_all <- merge(game_pst14_all, game_pst14_home, by = "id_2")
game_pst14_all <- merge(game_pst14_all, game_pst14_away, by = "id_2")
game_pst14_all <- game_pst14_all %>% relocate(zip, .before = country)
game_pst14_all$playoff <- 1
colnames(game_pst14_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "sr_id_game", "home_points", "away_points", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "seed_home", "sr_id_home", "away_id", "away_name", "away_short", "seed_away", "sr_id_away", "playoff")

##combine 2014-15 regular and postseason
game14 <- rbind.fill(game_reg14_all, game_pst14_all)
##write to .csv
write.csv(game14, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game14.csv")

##data manipulation
game14_SQL <- game14
game14_SQL$home_short <- gsub("SJ", "SJS", game14_SQL$home_short)
game14_SQL$away_short <- gsub("SJ", "SJS", game14_SQL$away_short)
game14_SQL$home_short <- gsub("TB", "TBL", game14_SQL$home_short)
game14_SQL$away_short <- gsub("TB", "TBL", game14_SQL$away_short)
game14_SQL$home_short <- gsub("LA", "LAK", game14_SQL$home_short)
game14_SQL$away_short <- gsub("LA", "LAK", game14_SQL$away_short)
game14_SQL$home_short <- gsub("NJ", "NJD", game14_SQL$home_short)
game14_SQL$away_short <- gsub("NJ", "NJD", game14_SQL$away_short)
game14_SQL$home_short <- gsub("FLAK", "FLA", game14_SQL$home_short)
game14_SQL$away_short <- gsub("FLAK", "FLA", game14_SQL$away_short)
game14_SQL$winner <- ifelse(game14_SQL$home_points > game14_SQL$away_points, game14_SQL$home_short, ifelse(game14_SQL$home_points == game14_SQL$away_points, "tie", game14_SQL$away_short))
game14_SQL$winner_score <- ifelse(game14_SQL$winner == game14_SQL$home_short, game14_SQL$home_points, ifelse(game14_SQL$winner == "tie", game14_SQL$home_points, game14_SQL$away_points))
game14_SQL$rivalry <- ifelse(game14_SQL$winner == game14_SQL$home_short, game14_SQL$away_short, ifelse(game14_SQL$winner == "tie", "tie", game14_SQL$home_short))
game14_SQL$rivalry_score <- ifelse(game14_SQL$winner == game14_SQL$home_short, game14_SQL$away_points, ifelse(game14_SQL$winner == "tie", game14_SQL$home_points, game14_SQL$home_points))
game14_SQL_1 <- subset(game14_SQL, home_points != 0)
game14_SQL_1 <- subset(game14_SQL_1, !grepl("all-star", tolower(game_title)))
game14_SQL_2 <- subset(game14_SQL_1, select = -c(1:5, 7:21, 23:25, 27, 29:30))
game14_SQL_2 <- game14_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game14_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game14_cleaned.csv")


##2013-14 regular
game_reg13 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-reg13.json")
game_reg13_venue <- as.data.frame(game_reg13$games$venue)
game_reg13_venue[, 11] <- c(1:1233)
colnames(game_reg13_venue)[11] <- "id_2"
game_reg13_home <- as.data.frame(game_reg13$games$home)
game_reg13_home[, 5] <- c(1:1233)
colnames(game_reg13_home)[5] <- "id_2"
game_reg13_away <- as.data.frame(game_reg13$games$away)
game_reg13_away[, 5] <- c(1:1233)
colnames(game_reg13_away)[5] <- "id_2"
game_reg13_game <- as.data.frame(game_reg13$games)
game_reg13_game <- game_reg13_game[, -c(6:9)]
game_reg13_game[, 9] <- c(1:1233)
colnames(game_reg13_game)[9] <- "id_2"
game_reg13_all <- merge(game_reg13_game, game_reg13_venue, by = "id_2")
game_reg13_all <- merge(game_reg13_all, game_reg13_home, by = "id_2")
game_reg13_all <- merge(game_reg13_all, game_reg13_away, by = "id_2")
game_reg13_all <- game_reg13_all %>% relocate(title, .before = status)
game_reg13_all <- game_reg13_all %>% relocate(zip, .before = country)
game_reg13_all$playoff <- 0
colnames(game_reg13_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled",  "sr_id_game", "home_points", "away_points", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "sr_id_home", "away_id", "away_name", "away_short", "sr_id_away", "playoff")

##2013-14 postseason
game_pst13 <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/schedule-post13.json")
game_pst13_venue <- as.data.frame(game_pst13$games$venue)
game_pst13_venue[, 11] <- c(1:105)
colnames(game_pst13_venue)[11] <- "id_2"
game_pst13_home <- as.data.frame(game_pst13$games$home)
game_pst13_home[, 6] <- c(1:105)
colnames(game_pst13_home)[6] <- "id_2"
game_pst13_away <- as.data.frame(game_pst13$games$away)
game_pst13_away[, 6] <- c(1:105)
colnames(game_pst13_away)[6] <- "id_2"
game_pst13_game <- as.data.frame(game_pst13$games)
game_pst13_game <- game_pst13_game[, -c(7:9)]
game_pst13_game[, 7] <- c(1:105)
colnames(game_pst13_game)[7] <- "id_2"
game_pst13_all <- merge(game_pst13_game, game_pst13_venue, by = "id_2")
game_pst13_all <- merge(game_pst13_all, game_pst13_home, by = "id_2")
game_pst13_all <- merge(game_pst13_all, game_pst13_away, by = "id_2")
game_pst13_all <- game_pst13_all %>% relocate(zip, .before = country)
game_pst13_all$playoff <- 1
colnames(game_pst13_all) <- c("id_2", "game_id", "game_title", "status", "coverage", "scheduled", "sr_id_game", "venue_id", "venue_name", "venue_capacity", "venue_address", "venue_city", "venue_state", "venue_zipcode", "venue_country", "venue_timezone", "sr_id_venue", "home_id", "home_name", "home_short", "seed_home", "sr_id_home", "away_id", "away_name", "away_short", "seed_away", "sr_id_away", "playoff")

##combine 2013-14 regular and postseason
game13 <- rbind.fill(game_reg13_all, game_pst13_all)
##write to .csv
write.csv(game13, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game13.csv")

##data manipulation
game13_SQL <- game13
game13_SQL$home_short <- gsub("SJ", "SJS", game13_SQL$home_short)
game13_SQL$away_short <- gsub("SJ", "SJS", game13_SQL$away_short)
game13_SQL$home_short <- gsub("TB", "TBL", game13_SQL$home_short)
game13_SQL$away_short <- gsub("TB", "TBL", game13_SQL$away_short)
game13_SQL$home_short <- gsub("LA", "LAK", game13_SQL$home_short)
game13_SQL$away_short <- gsub("LA", "LAK", game13_SQL$away_short)
game13_SQL$home_short <- gsub("NJ", "NJD", game13_SQL$home_short)
game13_SQL$away_short <- gsub("NJ", "NJD", game13_SQL$away_short)
game13_SQL$home_short <- gsub("FLAK", "FLA", game13_SQL$home_short)
game13_SQL$away_short <- gsub("FLAK", "FLA", game13_SQL$away_short)
game13_SQL$winner <- ifelse(game13_SQL$home_points > game13_SQL$away_points, game13_SQL$home_short, ifelse(game13_SQL$home_points == game13_SQL$away_points, "tie", game13_SQL$away_short))
game13_SQL$winner_score <- ifelse(game13_SQL$winner == game13_SQL$home_short, game13_SQL$home_points, ifelse(game13_SQL$winner == "tie", game13_SQL$home_points, game13_SQL$away_points))
game13_SQL$rivalry <- ifelse(game13_SQL$winner == game13_SQL$home_short, game13_SQL$away_short, ifelse(game13_SQL$winner == "tie", "tie", game13_SQL$home_short))
game13_SQL$rivalry_score <- ifelse(game13_SQL$winner == game13_SQL$home_short, game13_SQL$away_points, ifelse(game13_SQL$winner == "tie", game13_SQL$home_points, game13_SQL$home_points))
game13_SQL_1 <- subset(game13_SQL, home_points != 0)
game13_SQL_1 <- subset(game13_SQL_1, !grepl("all-star", tolower(game_title)))
game13_SQL_2 <- subset(game13_SQL_1, select = -c(1:5, 7:21, 23:25, 27, 29:30))
game13_SQL_2 <- game13_SQL_2 %>% relocate(playoff, .after = rivalry_score)
write.csv(game13_SQL_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/game13_cleaned.csv")

##combine all rows from each season
game <- rbind.fill(game13_SQL_2, game14_SQL_2, game15_SQL_2, game16_SQL_2, game17_SQL_2, game18_SQL_2, game19_SQL_2, game20_SQL_2, game21_SQL_2, game22_SQL_2)
game <- game %>% relocate(reference_game, .before = scheduled)
rows_to_modify <- c(4884:4998)
game$scheduled[rows_to_modify] <- substr(game$scheduled[rows_to_modify], 1, nchar(game$scheduled[rows_to_modify])-5)
rows_to_modify_1 <- c(1197:2558)
game$scheduled[rows_to_modify_1] <- substr(game$scheduled[rows_to_modify_1], 1, nchar(game$scheduled[rows_to_modify_1])-5)
rows_to_modify_2 <- c(3759:3841)
game$scheduled[rows_to_modify_2] <- substr(game$scheduled[rows_to_modify_2], 1, nchar(game$scheduled[rows_to_modify_2])-5)
game$date <- str_sub(game$scheduled, 1, -11)
game <- game[, -2]
game <- game %>% relocate(date, .after = reference_game)
write.csv(game, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/game/0. game.csv")


# for player statistics table
player_stat_df <- data.frame()
page <- c(1:31)
year <- c(2011:2024)
for (x in year){
  for(i in page) {
    player_stat_url <- paste0("https://www.capfriendly.com/browse/active/", x, "/salary?stats-season=", x, "&age-calculation-date=today&display=skater-individual-advanced-stats,goalie-advanced-stats&hide=clauses,age,position,handed,expiry-status,salary,caphit&pg=", i)
    # check if the url is valid
    #if (url.exists(salary_url)) {
    #  salary_url <- salary_url
    #}
    url_html <- read_html(player_stat_url)
    url_nodes <- html_nodes(url_html, "table")
    player_stat_table <- html_table(url_nodes)[[1]]
    player_stat <- player_stat_url %>% 
      read_html() %>% 
      html_nodes("table") %>% 
      html_table()
    player_stat_df_1 <- player_stat[[1]] %>% 
      mutate(year = x)
    player_stat_df <- rbind.fill(player_stat_df, player_stat_df_1)
  }
}

colnames(player_stat_df) <- c("player_name", "team", "games_played", "goals", "assists", "points", "points_per_game", "plus/minus", "shots_on_goal", "shooting_percentage", "average_time_on_ice", "individual_expected_goals", "individual_shots_on_goal", "individual_corsi", "individual_fenwick", "individual_expected_goals_per60min", "individual_shots_on_goal_per60min", "individual_corsi_per60min", "individual_fenwick_per60min", "wins", "loses", "shutouts", "goals_against_average", "save_percentage", "goals_against_per60min_of_ice_time", "goaltender_related_expected_goals_against_per60min_of_ice_time", "goals_saved_above_expected_per60min_of_ice_time", "year")

player_stat_df_2 <- player_stat_df %>%
  separate(player_name, into = c("number", "first_name", "last_name", "more_name", "more_name_2"), sep = " ", remove = TRUE) %>% 
  mutate(player_name = paste(ifelse(!is.na(first_name), first_name, ""),
                             ifelse(!is.na(last_name), last_name, ""),
                             ifelse(!is.na(more_name), more_name, ""),
                             ifelse(!is.na(more_name_2), more_name_2, ""),
                             sep = " ")) %>% 
  separate(average_time_on_ice, into = c("average_min_on_ice", "average_sec_on_ice"), sep = ":", remove = TRUE) %>% 
  relocate(player_name, .before = team)
player_stat_df_2 <- player_stat_df_2[, -c(1:5)]
player_stat_df_2$player_name <- str_trim(player_stat_df_2$player_name, side = "right")

write.csv(player_stat_df_2, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/player stat/player_stat_scraped.csv")


# for salary table
#yearly_salary <- read_html("https://www.spotrac.com/nhl/rankings/earnings/")
#nodes_yearly_salary <- html_nodes(yearly_salary, xpath = '//tbody')
#yearly_salary_tab <- html_table(nodes_yearly_salary)[[1]]
#colnames(yearly_salary_tab) <- c("No", "name_drafted", "earnings_total", "seasons", "earnings_total_2")
salary_df <- data.frame()
page <- c(1:31)
year <- c(2011:2024)
for (x in year){
  for(i in page) {
    salary_url <- paste0("https://www.capfriendly.com/browse/active/", 
                         x,
                         "?stats-season=", 
                         x, 
                         "&age-calculation-date=today&display=draft,signing-age&hide=clauses,age,position,handed,expiry-status,caphit&pg=", 
                         i)
    # check if the url is valid
    #if (url.exists(salary_url)) {
    #  salary_url <- salary_url
    #}
    url_html <- read_html(salary_url)
    url_nodes <- html_nodes(url_html, "table")
    salary_table <- html_table(url_nodes)[[1]]
    salary <- salary_url %>% 
      read_html() %>% 
      html_nodes("table") %>% 
      html_table()
    salary_df_1 <- salary[[1]] %>% 
      mutate(year = x - 1)
    salary_df <- rbind.fill(salary_df, salary_df_1)
  }
}
head(salary_df)
# change column names
colnames(salary_df) <- c("player_name", "team", "drafted",
                         "games_played", "goals", "assists",
                         "points", "points_per_game",
                         "plus/minus", "shots_on_goal", 
                         "shooting_percentage",
                         "average_time_on_ice", "wins", 
                         "loses", "shootouts", 
                         "goals against average",
                         "saving_percentage", "signing_age",
                         "salary", "year")

# data management
# cut "$" and "," in the salary column, 
# convert the salary values to numeric, 
# drop rows that have 0 for their salaries, 
# and split the player_name column so that 
# the numbers leading the names do not matter
salary_df_2 <- salary_df %>%
  mutate(salary = str_replace_all(salary, "\\$", "")) %>% 
  mutate(salary = str_replace_all(salary, ",", "")) %>% 
  filter(as.numeric(unlist(salary)) != 0) %>% 
  separate(player_name, 
           into = c("number", "first_name", 
                    "last_name", "more_name", 
                    "more_name_2"),
           sep = " ", 
           remove = TRUE) %>% 
  mutate(player_name = paste(ifelse(!is.na(first_name),
                                    first_name, ""),
                             ifelse(!is.na(last_name),
                                    last_name, ""),
                             ifelse(!is.na(more_name),
                                    more_name, ""),
                             ifelse(!is.na(more_name_2),
                                    more_name_2, ""),
                             sep = " ")) %>% 
  relocate(player_name, .before = team) %>% 
  mutate(drafted_year = substr(drafted, 
                               nchar(drafted) - 9, 
                               nchar(drafted) - 6)) %>% 
  relocate(drafted_year, .after = drafted)
salary_df_2 <- salary_df_2[, -c(1:5, 8)]
salary_df_2$salary <- as.numeric(salary_df_2$salary)


# stanley cup
##yearly winners
stanley_cup <- read_html("https://blog.ticketcity.com/nhl/stanley-cup-champions/")
nodes_stanley_cup <- html_nodes(stanley_cup, "table")
stanley_cup_table <- html_table(nodes_stanley_cup)[[1]]
colnames(stanley_cup_table) <- c("year", "winning_team", "losing_team", "champion_wins_loses")
stanley_cup_table <- stanley_cup_table[-c(1, 36:98), ]
##trustees
stanley_cup_trustee <- read_html("https://en.wikipedia.org/wiki/Stanley_Cup")
nodes_stanley_trustee <- html_nodes(stanley_cup_trustee, "table")
stanley_trustee_table <- html_table(nodes_stanley_trustee)[[2]]
colnames(stanley_trustee_table) <- c("trustee_name", "appointed_year", "served_until", "succeeded")
stanley_trustee_table <- stanley_trustee_table[-1, ]
stanley_cup_table$trustee_1 <- c(as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]), as.character(stanley_trustee_table[7, 1]))
stanley_cup_table$trustee_2 <- c(as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[9, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]), as.character(stanley_trustee_table[8, 1]))
stanley_cup_table <- stanley_cup_table[-1, ]
stanley_cup_table$champion_wins_loses <- gsub("-", "--", stanley_cup_table$champion_wins_loses)
write.csv(stanley_cup_table, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/stanley cup/0. stanley cup.csv")


# for team table
hierarchy <- fromJSON("G:/My Drive/0. study abroad/academic/10. 2023 Fall/6. SurvMeth 727 Fundamentals of Computing and Data Display/2. Assignments/Final Project/hierarchy.json")
## Atlantics
hierarchy_df_atlantics <- as.data.frame(hierarchy["conferences"]$conferences$divisions[[1]]$teams[[1]])
hierarchy_df_atlantics<- hierarchy_df_atlantics[, -c(7:16)]
hierarchy_df_atlantics[, 7] <- c(1, 2, 3, 4, 5, 6, 7, 8)
colnames(hierarchy_df_atlantics)[7] <- "id_2"
hierarchy_df_atlantics_venue <- as.data.frame(hierarchy["conferences"]$conferences$divisions[[1]]$teams[[1]][["venue"]])
hierarchy_df_atlantics_venue[, 11] <- c(1, 2, 3, 4, 5, 6, 7, 8)
colnames(hierarchy_df_atlantics_venue)[11] <- "id_2"
hierarchy_df_atlantics_all <- merge(hierarchy_df_atlantics, hierarchy_df_atlantics_venue, by="id_2")
hierarchy_df_atlantics_all[, 18] <- c("Atlantic", "Atlantic", "Atlantic", "Atlantic", "Atlantic", "Atlantic", "Atlantic", "Atlantic")
hierarchy_df_atlantics_all[, 19] <- c("Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern")
colnames(hierarchy_df_atlantics_all)[c(18, 19)] <- c("division", "conference")
## Metros
hierarchy_df_metros <- as.data.frame(hierarchy["conferences"]$conferences$divisions[[1]]$teams[[2]])
hierarchy_df_metros <- hierarchy_df_metros[, -c(7:16)]
hierarchy_df_metros[, 7] <- c(1, 2, 3, 4, 5, 6, 7, 8)
colnames(hierarchy_df_metros)[7] <- "id_2"
hierarchy_df_metros_venue <- as.data.frame(hierarchy["conferences"]$conferences$divisions[[1]]$teams[[2]][["venue"]])
hierarchy_df_metros_venue[, 11] <- c(1, 2, 3, 4, 5, 6, 7, 8)
colnames(hierarchy_df_metros_venue)[11] <- "id_2"
hierarchy_df_metros_all <- merge(hierarchy_df_metros, hierarchy_df_metros_venue, by="id_2")
hierarchy_df_metros_all[, 18] <- c("Metropolitan", "Metropolitan", "Metropolitan", "Metropolitan", "Metropolitan", "Metropolitan", "Metropolitan", "Metropolitan")
hierarchy_df_metros_all[, 19] <- c("Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern", "Eastern")
colnames(hierarchy_df_metros_all)[c(18, 19)] <- c("division", "conference")
## Pacific
hierarchy_df_pacific <- as.data.frame(hierarchy["conferences"]$conferences$divisions[[2]]$teams[[1]])
hierarchy_df_pacific <- hierarchy_df_pacific[, -c(7:16)]
hierarchy_df_pacific[, 7] <- c(1, 2, 3, 4, 5, 6, 7, 8)
colnames(hierarchy_df_pacific)[7] <- "id_2"
hierarchy_df_pacific_venue <- as.data.frame(hierarchy["conferences"]$conferences$divisions[[2]]$teams[[1]][["venue"]])
hierarchy_df_pacific_venue[, 11] <- c(1, 2, 3, 4, 5, 6, 7, 8)
colnames(hierarchy_df_pacific_venue)[11] <- "id_2"
hierarchy_df_pacific_all <- merge(hierarchy_df_pacific, hierarchy_df_pacific_venue, by="id_2")
hierarchy_df_pacific_all[, 18] <- c("Pacific", "Pacific", "Pacific", "Pacific", "Pacific", "Pacific", "Pacific", "Pacific")
hierarchy_df_pacific_all[, 19] <- c("Western", "Western", "Western", "Western", "Western", "Western", "Western", "Western")
colnames(hierarchy_df_pacific_all)[c(18, 19)] <- c("division", "conference")
## Central
hierarchy_df_central <- as.data.frame(hierarchy["conferences"]$conferences$divisions[[2]]$teams[[2]])
hierarchy_df_central <- hierarchy_df_central[, -c(7:16)]
hierarchy_df_central[, 7] <- c(1, 2, 3, 4, 5, 6, 7, 8)
colnames(hierarchy_df_central)[7] <- "id_2"
hierarchy_df_central_venue <- as.data.frame(hierarchy["conferences"]$conferences$divisions[[2]]$teams[[2]][["venue"]])
hierarchy_df_central_venue[, 11] <- c(1, 2, 3, 4, 5, 6, 7, 8)
colnames(hierarchy_df_central_venue)[11] <- "id_2"
hierarchy_df_central_all <- merge(hierarchy_df_central, hierarchy_df_central_venue, by="id_2")
hierarchy_df_central_all[, 18] <- c("Central", "Central", "Central", "Central", "Central", "Central", "Central", "Central")
hierarchy_df_central_all[, 19] <- c("Western", "Western", "Western", "Western", "Western", "Western", "Western", "Western")
colnames(hierarchy_df_central_all)[c(18, 19)] <- c("division", "conference")
### merge them all
all_teams <- rbind(hierarchy_df_atlantics_all, hierarchy_df_metros_all, hierarchy_df_pacific_all, hierarchy_df_central_all)
all_teams <- all_teams[, -c(1, 2, 6, 7, 8, 17)]
colnames(all_teams) <- c("nickname", "market", "acronym", "home_arena", "capacity", "address", "city", "state", "zip", "country", "timezone", "division", "conference")
### export the file to csv
write.csv(all_teams, "G:/My Drive/0. study abroad/academic/10. 2023 Fall/3. SI 564 SQL & Databases/Homework/Final Project/0. Datasets/tables/team/team_info.csv")