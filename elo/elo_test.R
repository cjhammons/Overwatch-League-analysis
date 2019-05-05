#'Runs the full process of getting match and team data, calculating elo, and testing accuracy
#'given a static k value.
#'
#'@param k k factor for the elo function
#'@param matches_df DataFrame of matches
#'@param teams_df DataFrame of teams
#'@return Accuracy of the elo model with the given k model
elo_full_test_static_k <- function(k, matches_df=import_matches(), teams_df=import_teams()) {
  #We create our own table, as the table from the API has a lot of irrelevant information.
  #We initialize all elo ratings to 2500
  elo_table <- init_elo_table(teams_df, 2500)
  
  #This loop will process all matches we imported and update our data in the elo table
  for (row in 1:nrow(matches_df)) {
    match <- matches_df[row,]
    status <- match[1,"status"]
    if (identical(status, "CONCLUDED")) {
      elo_table <- process_match(match, elo_table = elo_table, k=k)
    }
  }
  
  return(elo_test_accuracy(elo_table,matches_df))
}


#'Evaluates the Accuracy of an elo model by testing all completed matches
#'against the elo model's preditions
#'
#'@param elo_table The table of teams with their elo value
#'@param matches_df All matches played
#'@return percentage of correct predictions
elo_test_accuracy <- function(elo_table, matches_df=import_matches(TRUE)){
  total <- 0
  correct <- 0
  
  for (row in 1:nrow(matches_df)) {
    status <- matches_df[row,"status"]
    
    if (identical(status, "CONCLUDED")) {
      match <- matches_df[row,]
      
      competitor_ids <- match[1,"competitors"][[1]]$id
      winnerId <- match[1,"winner.id"]
      loserId <- -1
      if (identical(competitor_ids[1], winnerId)) {
        loserId <- competitor_ids[2]
      } else {
        loserId <- competitor_ids[1]
      }
      
      winnerElo <- elo_table[elo_table$id==winnerId,"elo"]
      loserElo <- elo_table[elo_table$id==loserId,"elo"]
      
      if (winnerElo > loserElo) {
        correct <- correct + 1
      }
      total <- total + 1
    }
  }
  return(correct / total)
}