library(tidyverse)

# Create function to fix NBA Team Names
fix_team_names <- function(team_vector) {
  new_vector <-
    case_when(
    str_detect(team_vector, "Philadelphia") ~ "Philadelphia 76ers",
    str_detect(team_vector, "Milwaukee") ~ "Milwaukee Bucks",
    str_detect(team_vector, "Chicago") ~ "Chicago Bulls",
    str_detect(team_vector, "Cleveland") ~ "Cleveland Cavaliers",
    str_detect(team_vector, "Boston") ~ "Boston Celtics",
    str_detect(team_vector, "Clippers") ~ "Los Angeles Clippers",
    str_detect(team_vector, "Memphis") ~ "Memphis Grizzlies",
    str_detect(team_vector, "Atlanta") ~ "Atlanta Hawks",
    str_detect(team_vector, "Miami") ~ "Miami Heat",
    str_detect(team_vector, "Charlotte") ~ "Charlotte Hornets",
    str_detect(team_vector, "Utah") ~ "Utah Jazz",
    str_detect(team_vector, "Sacramento") ~ "Sacramento Kings",
    str_detect(team_vector, "(New York)|(Knicks)") ~ "New York Knicks",
    str_detect(team_vector, "Lakers") ~ "Los Angeles Lakers",
    str_detect(team_vector, "Orlando") ~ "Orlando Magic",
    str_detect(team_vector, "Dallas") ~ "Dallas Mavericks",
    str_detect(team_vector, "Brooklyn") ~ "Brooklyn Nets",
    str_detect(team_vector, "Denver") ~ "Denver Nuggets",
    str_detect(team_vector, "Indiana") ~ "Indiana Pacers",
    str_detect(team_vector, "New Orleans") ~ "New Orleans Pelicans",
    str_detect(team_vector, "Detroit") ~ "Detroit Pistons",
    str_detect(team_vector, "Toronto") ~ "Toronto Raptors",
    str_detect(team_vector, "Houston") ~ "Houston Rockets",
    str_detect(team_vector, "San Antonio") ~ "San Antonio Spurs",
    str_detect(team_vector, "Phoenix") ~ "Phoenix Suns",
    str_detect(team_vector, "Oklahoma") ~ "Oklahoma City Thunder",
    str_detect(team_vector, "Minnesota") ~ "Minnesota Timberwolves",
    str_detect(team_vector, "Portland") ~ "Portland Trail Blazers",
    str_detect(team_vector, "Golden State") ~ "Golden State Warriors",
    str_detect(team_vector, "Washington") ~ "Washington Wizards"
  )
  
  return(new_vector)
}
