#'Evaluates the Accuracy of an elo model by testing all completed matches
#'against the elo model's preditions
#'
#'@param elo_table The table of teams with their elo value
#'@param matches_df All matches played
#'@return percentage of correct predictions
test_elo_accuracy <- function(elo_table, matches_df=import_matches(TRUE)){
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