# This is a playground file

teams_df = import_teams()
players_df = import_players()
matches_df = import_matches(chronological=TRUE)
games_df = import_games(matches_df)

elo_table <- init_elo_table(teams_df, 2500)
for (match in 1:nrow(matches_df)) {
  status <- matches_df[match,"status"]
  if (identical(status, "CONCLUDED")) {
    elo_table <- process_match(matches_df[match,], elo_table = elo_table, k=50)
  }
}

browser()
elo_table <- init_elo_table(teams_df, 2500)
history <- data.frame()
for (row in nrow(elo_table)) {
  newRow <- data.frame(placeholder_name=2500)
  names(newRow)[names(newRow) == "placeholder_name"] <- elo_table[row,'id']
  history <- rbind(history, newRow)
}