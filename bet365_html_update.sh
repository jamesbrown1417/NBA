#!/bin/bash

# Give access to normal path vars
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd ~/Projects/NBA || exit

# Execute Python Script
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 OddsScraper/Bet365/get_bet365_html.py