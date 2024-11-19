#!/bin/bash

# Give access to normal path vars
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd ~/Projects/NBA || exit

# Remove .json and .txt files in specific directories
rm OddsScraper/Neds/*.json

# Execute Python and R scripts
python3 OddsScraper/TAB/get-TAB-response.py
python3 OddsScraper/Neds/get_neds_urls.py
Rscript OddsScraper/Neds/get_neds_match_urls.R
python3 OddsScraper/Neds/get_match_json.py

# Execute R script for getting arbs
Rscript Scripts/get_arbs.R

# Publish report using Quarto
echo "1" | quarto publish quarto-pub Reports/nba_arbs.qmd

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main
