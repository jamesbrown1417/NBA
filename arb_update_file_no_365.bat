cd /d "C:\Users\james\R_Projects\NBA"
Rscript Scripts\get_arbs.R
echo 1 | quarto publish quarto-pub Reports\arbs.qmd
