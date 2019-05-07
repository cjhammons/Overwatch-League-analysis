library(sqldf)
library("formattable")

#' Initializes a fresh table of all teams with starting Elo
#' 
#' @param teams_df DataFrame of the raw team data. If not provided, calls import_teams() @see import_teams()
#' @param starting_elo Elo value that all teams are initialized to.
#' @param starting_map_elo Elo value that all map ratings will be initialized to
#' @return Trimmed version of the raw teams dataframe containing only the following columns: 
#'    wins, losses, map_wins, map_losses, map_diff, elo, and map elo. 
init_elo_table <- function(teams_df=import_teams(), starting_elo=1000, starting_map_elo=1000){
  
  elo_rankings_df <- data.frame()
  for (row in 1:nrow(teams_df)) {
    newRow <- data.frame(id=teams_df[row,"id"], 
                         "name"=teams_df[row,"name"],
                         "name_short"=teams_df[row,"abbreviatedName"],
                         'wins'=0,
                         "losses"=0,
                         "map_wins"=0,
                         "map_losses"=0,
                         "map_diff"=0,
                         "elo"=starting_elo
                         #"dorado"=starting_map_elo,
                         #"junkertown"=starting_map_elo,
                         #"rialto"=starting_map_elo,
                         #"route-66"=starting_map_elo,
                         #"watchpoint-gibraltar"=starting_map_elo,
                         #"hanamura"=starting_map_elo,
                         #"horizon-lunar-colony"=starting_map_elo,
                         #"temple-of-anubis"=starting_map_elo,
                         #"volskaya-industries"=starting_map_elo,
                         #"blizzard-world"=starting_map_elo,
                         #"eichenwalde"=starting_map_elo,
                         #"hollywood"=starting_map_elo,
                         #"kings-row"=starting_map_elo,
                         #"numbani"=starting_map_elo,
                         #"busan"=starting_map_elo,
                         #"ilios"=starting_map_elo,
                         #"lijiang-tower"=starting_map_elo,
                         #"nepal"=starting_map_elo,
                         #"oasis"=starting_map_elo
                         )
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
#' @param use_dynamic_k Boolean indicating whether to use a static K value or calculate it based on the closeness of the match
#' @return League table updated with wins, losses, map wins, map losses, map diff, and Elo
process_match <- function(match, elo_table, k=32, use_dynamic_k=FALSE) {
  
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
  
  if (use_dynamic_k){
    k = (winner_score - loser_score) * 10
  }
  
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

#'Creates a table and processes all available matches
#'
#'@param k k-factor
#'@param matches_df DataFrame of matches
#'@param teams_df DataFrame of teams
#'@param use_dynamic_k Boolean indicating whether to use a static K value or calculate it based on the closeness of the match
#'@return Fully processed elo table
process_full_table <- function(k=32, matches_df=import_matches(), teams_df=import_teams(), games_df=NULL, use_dynamic_k=FALSE){
  
  elo_table <- init_elo_table(teams_df)
  
  #This loop will process all matches we imported and update our data in the elo table
  for (row in 1:nrow(matches_df)) {
    match <- matches_df[row,]
    if (identical(match[1,"status"], "CONCLUDED")) {
      elo_table <- process_match(match, elo_table = elo_table, k=k, use_dynamic_k = use_dynamic_k)
    }
  }
  
  return(elo_table)
}


calculate_dynamic_k <- function(match, games_df=import_games()){
  
}



#'Formats and displays an elo table
#'
#'@param table An elo table
display <- function(table){
  #ggplot(data = table) + geom_bar(mapping = aes(x = name_short, y = elo, fill = name_short), stat="identity")
  formattable(table[order(-table$elo),], list(map_wins=FALSE, 
                                                    id=FALSE, 
                                                    name_short=FALSE,
                                                    map_losses=FALSE,
                                                    elo=color_bar("lightblue"),
                                                    map_diff=sign_formatter
  ))
}