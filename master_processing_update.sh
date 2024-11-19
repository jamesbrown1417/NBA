#!/bin/bash

# Set the current directory to your project folder
cd ~/Projects/NBA || exit

# Execute the R script for master processing
Rscript OddsScraper/master_processing_script.R
