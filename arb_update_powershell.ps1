# Set the current directory to your project folder
Set-Location -Path "C:\Users\james\R_Projects\NBA"

# Remove .json and .txt files in specific directories
Remove-Item -Path "C:\Users\james\R_Projects\NBA\OddsScraper\Neds\*.json"
Remove-Item -Path "C:\Users\james\R_Projects\NBA\Data\BET365_HTML\*.txt"

# Execute Python and R scripts
& "C:/Python311/python.exe" "c:/Users/james/R_Projects/NBA/OddsScraper/get_bet365_html.py"
& "C:/Python311/python.exe" "c:/Users/james/R_Projects/NBA/OddsScraper/get_bet365_player.py"

& "C:/Python312/python.exe" "c:/Users/james/R_Projects/NBA/OddsScraper/Neds/get_neds_urls.py"
& "Rscript" "OddsScraper\Neds\get_neds_match_urls.R"
& "C:/Python312/python.exe" "c:/Users/james/R_Projects/NBA/OddsScraper/Neds/get_match_json.py"

# Execute R script for getting arbs
& "Rscript" "Scripts\get_arbs.R"

# Publish report using Quarto
echo "1" | & "quarto" "publish" "quarto-pub" "Reports\arbs.qmd"
