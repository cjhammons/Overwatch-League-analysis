teams_df = import_teams()
players_df = import_players()
matches_df = import_matches(chronological=TRUE)
games_df = import_games()

#matches_df[with(matches_df, order(matches_df$actualEndDate))]
elo_table <- init_elo_table()
for (match in 1:nrow(matches_df)) {
  status <- matches_df[match,"status"]
  if (identical(status, "CONCLUDED")) {
    elo_table <- process_match(matches_df[match,], elo_table = elo_table)
  }
}