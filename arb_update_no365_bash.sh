#!/bin/bash

# Give access to normal path vars
export PATH="/opt/homebrew/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Load nvm (if installed) so npm/netlify from Node installs are available in non-interactive shells.
if [ -s "$HOME/.nvm/nvm.sh" ]; then
  # shellcheck disable=SC1090
  . "$HOME/.nvm/nvm.sh"
  nvm use --silent >/dev/null 2>&1 || true
fi

if ! command -v npm >/dev/null 2>&1; then
  echo "Error: npm not found. Install Node.js and ensure npm is on PATH."
  exit 1
fi

if ! command -v netlify >/dev/null 2>&1; then
  echo "Error: netlify CLI not found. Run: npm install -g netlify-cli"
  exit 1
fi

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

# Build + deploy the static web app to Netlify
(cd Apps/NBA_ARBS_WEB && npm run build)
netlify deploy --prod --dir Apps/NBA_ARBS_WEB/dist --site 805c47ab-d26f-4406-a2b0-4e0986ddb7c8

# Publish report using Quarto
# echo "1" | quarto publish netlify Reports/nba_arbs.qmd

# Automatically stage all changes
git add .

# Commit changes with a message including "automated commit" and the current timestamp
commitMessage="automated commit and timestamp $(date '+%Y-%m-%d %H:%M:%S')"
git commit -m "$commitMessage"

# Push the commit to the 'main' branch on 'origin'
git push origin main
