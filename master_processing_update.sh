#!/bin/bash

# Give access to normal path vars
export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# Set the current directory to your project folder
cd ~/Projects/NBA || exit

# Execute the R script for master processing
Rscript OddsScraper/master_processing_script.R
