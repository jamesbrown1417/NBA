#!/bin/bash

# Give access to normal path vars
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd ~/Projects/NBA || exit

# Remove .json and .txt files in specific directories
rm OddsScraper/Neds/*.json

# Remove Scraped Odds Files
rm Data/scraped_odds/*.csv

# Execute Python and R scripts
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/TAB/get-TAB-response.py
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_neds_urls.py
Rscript OddsScraper/Neds/get_neds_match_urls.R
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Neds/get_match_json.py

# Execute R script for getting arbs
Rscript Scripts/get_arbs.R

# Export web app JSON snapshot
Rscript Scripts/export_nba_arbs_web_data.R

# Optional: deploy the static web app to Netlify (placeholder)
# netlify deploy --prod --dir Apps/NBA_ARBS_WEB --site YOUR_NETLIFY_SITE_ID

# Publish report using Quarto
echo "1" | quarto publish netlify Reports/nba_arbs.qmd

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main
