#!/bin/bash

# Give access to normal path vars
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd ~/Projects/NBA || exit

# Execute the Python script for API data
/Users/jamesbrown/.pyenv/versions/3.12.5/bin/python3 Scripts/get_all_data_current_season.py
