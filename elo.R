library(sqldf)

#' Initializes a fresh table of all teams with starting Elo
#' 
#' @return DataFrame of the teams with a starting Elo rating of 1000 for all of them
init_elo_table <- function(){
  
  elo_rankings_df <- data.frame()
  for (row in 1:nrow(teams_df)) {
    newRow <- data.frame(id=teams_df[row,"id"], 
                         handle=teams_df[row,"handle"],
                         name=teams_df[row,"name"],
                         abbreviatedName=teams_df[row,"abbreviatedName"],
                         wins=0,
                         losses=0,
                         map_wins=0,
                         map_losses=0,
                         map_diff=0,
                         elo=1000)
    
    elo_rankings_df <- rbind(elo_rankings_df, newRow)
  }
  return(elo_rankings_df)
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
#'  
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

#'
#'
#'
#'
#'
#'
#'
#'
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
  new_elos <- elo_calculation(winnerElo = winnerElo, loserElo = loserElo)
  
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






