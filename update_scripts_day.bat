cd /d "C:\Users\james\R_Projects\NBA"
Rscript OddsScraper\master_processing_script.R
Rscript Scripts\archive_odds_for_the_day.R
Rscript Scripts\get_team_last_n_opponent_stats.R
quarto render Reports\todays_bets.qmd --to html
copy Reports\todays_bets.html "C:\Users\james\OneDrive\NBA Odds Archive"
