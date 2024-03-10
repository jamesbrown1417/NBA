library(tidyverse)

# Create function to fix NBA Team Names
fix_team_names <- function(team_vector) {
  new_vector <- case_when(
    str_detect(team_vector, "(Philadelphia)|(76ers)") ~ "Philadelphia 76ers",
    str_detect(team_vector, "(Milwaukee)|(Bucks)") ~ "Milwaukee Bucks",
    str_detect(team_vector, "(Chicago)|(Bulls)") ~ "Chicago Bulls",
    str_detect(team_vector, "(Cleveland)|(Cavaliers)") ~ "Cleveland Cavaliers",
    str_detect(team_vector, "(Boston)|(Celtics)") ~ "Boston Celtics",
    str_detect(team_vector, "(Los Angeles)|(Clippers)") ~ "Los Angeles Clippers",
    str_detect(team_vector, "(Memphis)|(Grizzlies)") ~ "Memphis Grizzlies",
    str_detect(team_vector, "(Atlanta)|(Hawks)") ~ "Atlanta Hawks",
    str_detect(team_vector, "(Miami)|(Heat)") ~ "Miami Heat",
    str_detect(team_vector, "(Charlotte)|(Hornets)") ~ "Charlotte Hornets",
    str_detect(team_vector, "(Utah)|(Jazz)") ~ "Utah Jazz",
    str_detect(team_vector, "(Sacramento)|(Kings)") ~ "Sacramento Kings",
    str_detect(team_vector, "(New York)|(Knicks)") ~ "New York Knicks",
    str_detect(team_vector, "(Los Angeles)|(Lakers)") ~ "Los Angeles Lakers",
    str_detect(team_vector, "(Orlando)|(Magic)") ~ "Orlando Magic",
    str_detect(team_vector, "(Dallas)|(Mavericks)") ~ "Dallas Mavericks",
    str_detect(team_vector, "(Brooklyn)|(Nets)") ~ "Brooklyn Nets",
    str_detect(team_vector, "(Denver)|(Nuggets)") ~ "Denver Nuggets",
    str_detect(team_vector, "(Indiana)|(Pacers)") ~ "Indiana Pacers",
    str_detect(team_vector, "(New Orleans)|(Pelicans)") ~ "New Orleans Pelicans",
    str_detect(team_vector, "(Detroit)|(Pistons)") ~ "Detroit Pistons",
    str_detect(team_vector, "(Toronto)|(Raptors)") ~ "Toronto Raptors",
    str_detect(team_vector, "(Houston)|(Rockets)") ~ "Houston Rockets",
    str_detect(team_vector, "(San Antonio)|(Spurs)") ~ "San Antonio Spurs",
    str_detect(team_vector, "(Phoenix)|(Suns)") ~ "Phoenix Suns",
    str_detect(team_vector, "(Oklahoma City)|(Thunder)") ~ "Oklahoma City Thunder",
    str_detect(team_vector, "(Minnesota)|(Timberwolves)") ~ "Minnesota Timberwolves",
    str_detect(team_vector, "(Portland)|(Trail Blazers)") ~ "Portland Trail Blazers",
    str_detect(team_vector, "(Golden State)|(Warriors)") ~ "Golden State Warriors",
    str_detect(team_vector, "(Washington)|(Wizards)") ~ "Washington Wizards"
  )
  return(new_vector)
}
