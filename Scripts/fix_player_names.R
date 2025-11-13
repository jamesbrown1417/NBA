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
    name_vector == "Royce ONeale" ~ "Royce O'Neale",

    # Expanded / abbreviated forms
    name_vector == "K. Caldwell-Pope" ~ "Kentavious Caldwell-Pope",
    name_vector == "S. Gilgeous-Alexander" ~ "Shai Gilgeous-Alexander",
    name_vector == "N. Alexander-Walker" ~ "Nickeil Alexander-Walker",
    name_vector == "Nickeil A-Walker" ~ "Nickeil Alexander-Walker",
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
    name_vector == "B Sensabaug"      ~ "B Sensabaugh",
    name_vector == "C M-Boyles"       ~ "C Murray-Boyles",
    name_vector == "C M Boyles"       ~ "C Murray-Boyles",
    name_vector == "Don Mitchel"      ~ "Do Mitchell",
    name_vector == "D Mitchel"        ~ "Do Mitchell",
    name_vector == "J Nurkic"         ~ "J Nurkić",
    name_vector == "Jar Walker"       ~ "Ja Walker",
    name_vector == "J Walker"      ~ "Ja Walker",
    name_vector == "K Filipwski"      ~ "K Filipowski",
    name_vector %in% c("Key George",
                       "K George")    ~ "Ke George",
    name_vector == "L Nance"          ~ "L Nance Jr.",
    name_vector == "N AlexWalk"       ~ "N Alexander-Walker",
    name_vector == "N Alex Walk"      ~ "N Alexander-Walker",
    name_vector == "R O Neale"        ~ "R O'Neale",
    name_vector == "S Mamklvili"      ~ "S Mamukelashvili",
    name_vector == "S Mamuklvili"     ~ "S Mamukelashvili",
    name_vector == "S Mykhailuk"      ~ "S Mykhailiuk",
    name_vector == "W Clayton"        ~ "W Clayton Jr.",
    
    name_vector == "I Hartstein"      ~ "I Hartenstein",
    name_vector == "G-Alexander"      ~ "S Gilgeous-Alexander",
    name_vector == "Jab Smith"        ~ "J Smith Jr.",
    name_vector == "B Podziemsk"      ~ "B Podziemski", 
    name_vector == "Dray Green"       ~ "D Green",
    name_vector == "Gab Vincent"      ~ "G Vincent",
    name_vector == "J Vanderbil"      ~ "J Vanderbilt",
    name_vector == "J Butler"         ~ "J Butler III",
    name_vector == "L Doncic"         ~ "L Dončić",
    name_vector == "N Jokic"          ~ "N Jokić",
    name_vector == "N Vucevic"        ~ "N Vučević",
    name_vector == "B Bogdanovic"     ~ "B Bogdanović",
    name_vector == "D Schroder"       ~ "D Schröder",
    name_vector == "J Champ"          ~ "Ju Champagnie",
    name_vector == "J Champagnie"     ~ "Ju Champagnie",
    name_vector == "K Johnson"        ~ "Key Johnson",
    name_vector == "M Porter"         ~ "M Porter Jr.",
    name_vector == "M Porter Jr"      ~ "M Porter Jr.",
    name_vector == "G Trent Jr"       ~ "G Trent Jr.",
    name_vector == "D Jones Jr"       ~ "D Jones Jr.",
    name_vector == "N Jovic"          ~ "N Jović",
    
    # Collapse double-initial first names to single-initial to match join_name
    name_vector == "OG Anunoby"       ~ "O Anunoby",
    name_vector == "AJ Green"         ~ "A Green",
    name_vector == "CJ McCollum"      ~ "C McCollum",
    name_vector == "RJ Barrett"       ~ "R Barrett",
    name_vector == "PJ Washington"    ~ "P Washington",
    name_vector == "L Ball"           ~ "L Ball",
    name_vector == "A Wiggins"        ~ "A Wiggins",
    name_vector == "T Mann"           ~ "T Mann",
    name_vector == "M Bridges"        ~ "M Bridges",
    
    name_vector == "V Wembanyam"      ~ "V Wembanyama",
    
    .default = name_vector
  )
  
  return(fixed)
}
