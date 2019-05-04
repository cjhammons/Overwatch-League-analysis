library(sqldf)

#' Initializes a fresh table of all teams with starting Elo
#' 
#' @param teams_df DataFrame of the raw team data. If not provided, calls import_teams() @see import_teams()
#' @param starting_elo Elo value that all teams are initialized to.
#' @return Trimmed version of the raw teams dataframe containing only the following columns: 
#'    wins, losses, map_wins, map_losses, map_diff, and elo. 
init_elo_table <- function(teams_df=import_teams(), starting_elo=1000){
  
  elo_rankings_df <- data.frame()
  for (row in 1:nrow(teams_df)) {
    newRow <- data.frame(id=teams_df[row,"id"], 
                         name=teams_df[row,"name"],
                         wins=0,
                         losses=0,
                         map_wins=0,
                         map_losses=0,
                         map_diff=0,
                         elo=starting_elo)
    
    elo_rankings_df <- rbind(elo_rankings_df, newRow)
  }
  return(elo_rankings_df)
}

#'
#'
#'
#'
#' 
init_elo_history <- function(elo_table, starting_elo=1000) {
  history <- data.frame()
  for (row in nrow(elo_table)) {
    
  }
}

#' Performs a standard elo calculation.
#' See \url{https://metinmediamath.wordpress.com/2013/11/27/how-to-calculate-the-elo-rating-including-example/}
#' for information on the math used.
#' 
#' @param winnerElo The current Elo value of the winner
#' @param loserElo the current Elo value of the loser
#' @param k The K-factor used in the calculation (see link). Defaults to 32.
#' 
#' @return Returns a vector of the new winner and loser Elo values
#' @examples elo_calculatioin(2400, 2000)
#' @examples elo_calculatioin(2000, 2400)
#' @examples elo_calculatioin(2400, 2000, 60)
elo_calculation <- function(winnerElo, loserElo, k=32) {
  
  r1 <- 10^(winnerElo/400)
  r2 <- 10^(loserElo/400)
  
  e1 <- r1 / (r1+r2)
  e2 <- r2 / (r1+r2)
  
  new_winner_elo = winnerElo + k * (1 - e1)
  new_loser_elo = loserElo + k * (0 - e2)
  
  return_values <- c("winner"=new_winner_elo,"loser"=new_loser_elo)
  return(return_values)
}

#' Processes a single match and adjusts the league standings
#' 
#' @param match Match represented by a single row DataFrame extracted from the Matches DataFrame
#' @param elo_table League table containing Elo ratings that will be updated
#' @param K the k-factor to be used in the elo calculations.
#' @return League table updated with wins, losses, map wins, map losses, map diff, and Elo
process_match <- function(match, elo_table, k=32) {
  
  #Extract the team IDs and assign them appropiately
  competitor_ids <- match[1,"competitors"][[1]]$id
  winnerId <- match[1,"winner.id"]
  loserId <- -1
  if (identical(competitor_ids[1], winnerId)) {
    loserId <- competitor_ids[2]
  } else {
    loserId <- competitor_ids[1]
  }
  
  #Extract score and calculate map delta for each team
  winner_score <- -1
  loser_score <- -1
  scores <- match[1,"scores"][[1]]$value
  if (scores[1] > scores[2]) {
    winner_score <- scores[1]
    loser_score <- scores[2]
  } else {
    winner_score <- scores[2]
    loser_score <- scores[1]
  }
  winner_map_delta = winner_score - loser_score
  loser_map_delta = loser_score - winner_score
  
  #perform elo calculations

  winnerElo <- subset(elo_table, id==winnerId)$elo
  loserElo <- subset(elo_table, id==loserId)$elo
  new_elos <- elo_calculation(winnerElo = winnerElo, loserElo = loserElo, k=k)
  
  #Update each team's entry in the table
  
  elo_table[which(elo_table$id == winnerId),]$wins <- elo_table[which(elo_table$id == winnerId),]$wins + 1
  elo_table[which(elo_table$id == winnerId),]$map_wins <- elo_table[which(elo_table$id == winnerId),]$map_wins + winner_score
  elo_table[which(elo_table$id == winnerId),]$map_losses <- elo_table[which(elo_table$id == winnerId),]$map_losses + loser_score
  elo_table[which(elo_table$id == winnerId),]$map_diff <- elo_table[which(elo_table$id == winnerId),]$map_diff + winner_map_delta
  elo_table[which(elo_table$id == winnerId),]$elo <- elo_table[which(elo_table$id == winnerId),]$map_wins + new_elos['winner']

  elo_table[which(elo_table$id == loserId),]$losses <- elo_table[which(elo_table$id == loserId),]$losses + 1
  elo_table[which(elo_table$id == loserId),]$map_wins <- elo_table[which(elo_table$id == loserId),]$map_wins + loser_score
  elo_table[which(elo_table$id == loserId),]$map_losses <- elo_table[which(elo_table$id == loserId),]$map_losses + winner_score
  elo_table[which(elo_table$id == loserId),]$map_diff <- elo_table[which(elo_table$id == loserId),]$map_diff + loser_map_delta
  elo_table[which(elo_table$id == loserId),]$elo <- elo_table[which(elo_table$id == loserId),]$map_wins + new_elos['loser']
  
  return(elo_table)
}


process_matches <- function(elo_table=init_elo_table(), matches_df=import_matches(), stage=-1, playoffs=TRUE){
  
  for (match in 1:nrow(matches_df)) {
    status <- matches_df[match,"status"]
    if (identical(status, "CONCLUDED")) {
      elo_table <- process_match(matches_df[match,], elo_table = elo_table, k=50)
    }
  }
  
  if (stage > 0) {
    for (match in 1:nrow(matches_df)) {
      status <- matches_df[match,"status"]
      stage <- matches_df[match, "bracket.stage.tournament.series.id"]
      if (identical(status, "CONCLUDED") & identical(stage, stage_)) {
        elo_table <- process_match(matches_df[match,], elo_table = elo_table, k=50)
      }
    }
  }
  
  if (!playoffs) {
    for (match in 1:nrow(matches_df)) {
      status <- matches_df[match,"status"]
      if (identical(status, "CONCLUDED")) {
        elo_table <- process_match(matches_df[match,], elo_table = elo_table, k=50)
      }
    }
  }
  
  return(elo_table)
}






