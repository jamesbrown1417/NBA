library(tidyverse)

# Create function to fix player names across books
fix_player_names <- function(name_vector) {
  name_vector <- name_vector |> 
    str_trim() |> 
    str_replace_all("  +", " ") |> 
    str_remove_all("\\s*\\(.*\\)$")

  fixed <- case_when(
    # Punctuation and suffixes
    name_vector == "P.J Washington" ~ "P.J. Washington",
    name_vector == "PJ Washington" ~ "P.J. Washington",
    name_vector == "Bruce Brown Jr" ~ "Bruce Brown",
    name_vector == "Wendell Carter" ~ "Wendell Carter Jr.",
    name_vector == "Jabari Smith" ~ "Jabari Smith Jr.",
    name_vector == "Jabari Smith Jr" ~ "Jabari Smith Jr.",
    name_vector == "Kelly Oubre" ~ "Kelly Oubre Jr.",
    name_vector == "Derrick Jones" ~ "Derrick Jones Jr.",
    name_vector == "Tim Hardaway" ~ "Tim Hardaway Jr.",
    name_vector == "Michael Porter" ~ "Michael Porter Jr.",
    name_vector == "Gary Trent" ~ "Gary Trent Jr.",
    name_vector == "Gary Payton Ii" ~ "Gary Payton II",
    name_vector == "Dereck Lively" ~ "Dereck Lively II",
    name_vector == "Jimmy Butler" ~ "Jimmy Butler III",

    # Preferred first-name variants
    name_vector == "Cam Johnson" ~ "Cameron Johnson",
    name_vector == "Cam Payne" ~ "Cameron Payne",
    name_vector == "Cameron Thomas" ~ "Cam Thomas",
    name_vector == "Joshua Giddey" ~ "Josh Giddey",

    # Casing corrections
    name_vector == "Lebron James" ~ "LeBron James",
    name_vector == "D'angelo Russell" ~ "D'Angelo Russell",
    name_vector == "De'andre Hunter" ~ "De'Andre Hunter",
    name_vector == "Lamelo Ball" ~ "LaMelo Ball",
    name_vector == "Fred Vanvleet" ~ "Fred VanVleet",
    name_vector == "Demar Derozan" ~ "DeMar DeRozan",
    name_vector == "Rj Barrett" ~ "RJ Barrett",
    name_vector == "Zach Lavine" ~ "Zach LaVine",
    name_vector == "Miles Mcbride" ~ "Miles McBride",
    name_vector == "Jaden Mcdaniels" ~ "Jaden McDaniels",
    name_vector == "Cj Mccollum" ~ "CJ McCollum",
    name_vector == "Donte Divincenzo" ~ "Donte DiVincenzo",
    
    # Apostrophes
    name_vector == "DAngelo Russell" ~ "D'Angelo Russell",
    name_vector == "DeAndre Hunter" ~ "De'Andre Hunter",

    # Expanded / abbreviated forms
    name_vector == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
    name_vector == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
    name_vector == "N. Alexander-Walker" ~ "Nickeil Alexander-Walker",
    name_vector == "G. Antetokounmpo" ~ "Giannis Antetokounmpo",
    name_vector == "Alexandre Sarr" ~ "Alex Sarr",

    # Diacritics
    name_vector == "Jakob Pöltl" ~ "Jakob Poeltl",
    name_vector == "Jusuf Nurkic" ~ "Jusuf Nurkić",
    name_vector == "Luka Doncic" ~ "Luka Dončić",
    name_vector == "Nikola Jokic" ~ "Nikola Jokić",
    name_vector == "Nikola Vucevic" ~ "Nikola Vučević",
    name_vector == "Nikola Jovic" ~ "Nikola Jović",
    name_vector == "Bogdan Bogdanovic" ~ "Bogdan Bogdanović",
    name_vector == "Bojan Bogdanovic" ~ "Bojan Bogdanović",
    name_vector == "Dennis Schroder" ~ "Dennis Schröder",
    name_vector == "Kristaps Porzingis" ~ "Kristaps Porziņģis",
    
    # Weird Tab names
    name_vector == "Shai G-Alexander" ~ "Shai Gilgeous-Alexander",

    # Fallback
    .default = name_vector
  )

  return(fixed)
}

# Create function to fix player names in TAB
fix_player_names_tab_initials <- function(name_vector) {
  name_vector <- name_vector |> 
    str_trim() |> 
    str_replace_all("  +", " ") |> 
    str_remove_all("\\s*\\(.*\\)$")
  
  fixed <- case_when(
    # Expanded / abbreviated forms
    name_vector == "I Hartstein" ~ "I Hartenstein",
    name_vector == "G-Alexander" ~ "S Gilgeous-Alexander",
    name_vector == "Jab Smith" ~ "J Smith Jr.",
    name_vector == "B Podziemsk" ~ "B Podziemski", 
    name_vector == "Dray Green" ~ "D Green",
    name_vector == "Gab Vincent" ~ "G Vincent",
    name_vector == "J Vanderbil" ~ "J Vanderbilt",
    name_vector == "J Butler" ~ "J Butler III",
    name_vector == "L Doncic" ~ "L Dončić",
    name_vector == "N Jokic" ~ "N Jokić",
    name_vector == "N Vucevic" ~ "N Vučević",
    name_vector == "B Bogdanovic" ~ "B Bogdanović",
    name_vector == "D Schroder" ~ "D Schröder",
    name_vector == "J Champ" ~ "J Champagnie",
    name_vector == "K Johnson" ~ "K Johnson",
    name_vector == "M Porter" ~ "M Porter Jr.",
    name_vector == "V Wembanyam" ~ "V Wembanyama",
    
    # Fallback
    .default = name_vector
  )
  
  return(fixed)
}
