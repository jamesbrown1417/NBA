#!/bin/bash

# Set the current directory to your project folder
cd ~/Projects/NBA || exit

# Execute the Python script for API data
python3 Scripts/get_all_data_current_season.py
