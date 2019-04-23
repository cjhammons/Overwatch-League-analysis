library("httr")
library("jsonlite")

base <- "https://api.overwatchleague.com/"

#' Runs all import functions and stores them in globally available DataFrames.
#' 
#' The following DataFrames are created:
#' -teams_df
#' -players_df
#' -matches_df
#' -games_df
#' 
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
  require("httr")
  require("jsonlite")
  return(as.data.frame(fromJSON(content(GET(paste(base, "v2/teams", sep="")), "text"),flatten=TRUE)$data))
}

#' Gets the current standings of the Overwatch League
#' 
#' @return A DataFrame containing the teams and their current standing
import_ranking <- function(){
  require("httr")
  require("jsonlite")
  return(as.data.frame(fromJSON(content(GET(paste(base, "ranking", sep="")), "text"),flatten=TRUE)))
}

#' Gets all players in the Overwatch League
#' 
#' @return A DataFrame containing the playersa
import_players <- function(){
  require("httr")
  require("jsonlite")
  return(as.data.frame(fromJSON(content(GET(paste(base, "players", sep="")), "text"),flatten=TRUE)$content))
}

#' Gets all matches (played, live, and scheduled) for the current Overwatch League season.
#' 
#' @param chronological Boolean indicating whether or not to sort the matches by chronological order
#' @return A DataFrame containing the matches
import_matches <- function(chronological=FALSE){
  require("httr")
  require("jsonlite")
  df <- as.data.frame(fromJSON(content(GET(paste(base, "matches", sep="")), "text"),flatten=TRUE)$content)
  if (chronological){
    return(df[order(df$actualEndDate),])
  }
  return(df)
}

#' Gets games (maps) played for the current Overwathch League Season
#' 
#' @param matches_df DataFrame of the matches. Since there is no endpoint for only the games a DataFrame 
#' of Matches is required. If one isn't provided import_matches() is called. @see import_matches()    
#' @return A DataFrame containing the games (maps)
import_games <- function(matches_df=import_matches()){
  
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

