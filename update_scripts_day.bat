cd /d "C:\Users\james\R_Projects\NBA"
Rscript OddsScraper\master_processing_script.R
Rscript Scripts\archive_odds_for_the_day.R
Rscript Scripts\get_team_last_n_opponent_stats.R
echo 1 | quarto publish quarto-pub Reports\todays_bets.qmd
cd /d "C:\Users\james\R_Projects\NBA\Apps\SGM_APP"
echo 1 | quarto publish quarto-pub todays_betmakers_under_sgms.qmd
