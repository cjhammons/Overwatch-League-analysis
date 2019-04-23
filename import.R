require("httr")
require("jsonlite")

base <- "https://api.overwatchleague.com/"

#' Runs all import functions and stores them in globally available DataFrames.
#' 
#' The following DataFrames are created:
#' -teams_df
#' -players_df
#' -matches_df
#' -games_df
import_all <- function() {
  teams_df = import_teams()
  players_df = import_players()
  matches_df = import_matches()
  games_df = import_games()
}

#' Gets all teams currently in the Overwatch league
#' 
#' @return A DataFrame containing the teams
import_teams <- function(){
  return(as.data.frame(fromJSON(content(GET(paste(base, "v2/teams", sep="")), "text"),flatten=TRUE)$data))
}

#' Gets the current standings of the Overwatch League
#' 
#' @return A DataFrame containing the teams and their current standing
import_ranking <- function(){
  return(as.data.frame(fromJSON(content(GET(paste(base, "ranking", sep="")), "text"),flatten=TRUE)))
}

#' Gets all players in the Overwatch League
#' 
#' @return A DataFrame containing the playersa
import_players <- function(){
  return(as.data.frame(fromJSON(content(GET(paste(base, "players", sep="")), "text"),flatten=TRUE)$content))
}

#' Gets all matches (played, live, and scheduled) for the current Overwatch League season.
#' 
#' @return A DataFrame containing the matches
import_matches <- function(chronological=FALSE){
  df <- as.data.frame(fromJSON(content(GET(paste(base, "matches", sep="")), "text"),flatten=TRUE)$content)
  if (chronological){
    return(df[order(df$actualEndDate),])
  }
  return(df)
}

#' Gets games (maps) played for the current Overwathch League Season
#' 
#' @return A DataFrame containing the maps
import_games <- function(){
  #there is no enpoint to get only the games, so we must get the game data from the matches endpoint
  matches_df <- import_matches(chronological = TRUE)
  #now iterrate through each match and extract the game data
  games_df <- data.frame()
  for (row in 1:nrow(matches_df)) {
    status <- matches_df[row,"status"]
    if (identical(status, "CONCLUDED")) {
      games <- matches_df[row, "games"]
      games_df <- rbind(games_df, games[[1]])
    }
  }
  return(games_df)
}

