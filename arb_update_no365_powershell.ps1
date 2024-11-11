# Set the current directory to your project folder
Set-Location -Path "C:\Users\james\OneDrive\Desktop\Projects\NBA"

# Remove .json and .txt files in specific directories
Remove-Item -Path "C:\Users\james\OneDrive\Desktop\Projects\NBA\OddsScraper\Neds\*.json"

# Execute Python and R scripts
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" C:\Users\james\OneDrive\Desktop\Projects\NBA/OddsScraper/TAB/get-TAB-response.py 

& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "C:\Users\james\OneDrive\Desktop\Projects\NBA/OddsScraper/Neds/get_neds_urls.py"
& "Rscript" "OddsScraper\Neds\get_neds_match_urls.R"
& "C:/Users/james/AppData/Local/Microsoft/WindowsApps/python3.12.exe" "C:\Users\james\OneDrive\Desktop\Projects\NBA/OddsScraper/Neds/get_match_json.py"

# Execute R script for getting arbs
& "Rscript" "Scripts\get_arbs.R"

# Publish report using Quarto
echo "1" | & "quarto" "publish" "quarto-pub" "Reports\nba_arbs.qmd"
# echo "1" | & "quarto" "publish" "quarto-pub" "Reports\Top_Down.qmd"

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
$commitMessage = "automated commit and timestamp " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss")
git commit -m $commitMessage

# Push the commit to the 'main' branch on 'origin'
git push origin main
