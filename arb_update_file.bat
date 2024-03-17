C:/Python311/python.exe c:/Users/james/R_Projects/NBA/OddsScraper/get_bet365_html.py 
C:/Python311/python.exe c:/Users/james/R_Projects/NBA/OddsScraper/get_bet365_player.py     

cd /d "C:\Users\james\R_Projects\NBA"
Rscript Scripts\get_arbs.R
echo 1 | quarto publish quarto-pub Reports\arbs.qmd
