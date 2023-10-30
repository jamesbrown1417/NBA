# Import necessary libraries
import requests
import pandas as pd

# Get Data--------------------------------------------------------------

# Read in team data
df_teams = pd.read_csv('Data/all_teams.csv')

# Fetch the JSON data from the URL
response = requests.get("https://cdn.nba.com/static/json/staticData/scheduleLeagueV2.json")
json_data = response.json()

# Get timestamp of scraping
metadata_time = json_data['meta']['time']

# Empty lists
all_match_ids = []
match_type = []
all_match_dates = []
all_home_team_ids = []
all_away_team_ids = []

# Get the league data
for game in json_data['leagueSchedule']['gameDates']:
    for match in game['games']:
        all_match_ids.append(match['gameId'])
        match_type.append(match['seriesText'])
        all_match_dates.append(match['gameDateTimeUTC'])
        all_home_team_ids.append(match['homeTeam']['teamId'])
        all_away_team_ids.append(match['awayTeam']['teamId'])

# Create a dataframe
df_schedule = pd.DataFrame({
    'match_id': all_match_ids,
    'match_type': match_type, # 'P' = Preseason, 'R' = Regular Season, 'T' = Playoffs, 'O' = All-Star, 'S' = Special (e.g. exhibition)
    'match_date_utc': all_match_dates,
    'home_team_id': all_home_team_ids,
    'away_team_id': all_away_team_ids
})

# Join home team name to the schedule using teamId
df_schedule = df_schedule.merge(df_teams[['id', 'full_name']], left_on='home_team_id', right_on='id')

# Rename full_name to home_team_name
df_schedule = df_schedule.rename(columns={'full_name': 'home_team_name'})

# Join away team name to the schedule using teamId
df_schedule = df_schedule.merge(df_teams[['id', 'full_name']], left_on='away_team_id', right_on='id')

# Rename full_name to away_team_name
df_schedule = df_schedule.rename(columns={'full_name': 'away_team_name'})

# Remove id_x and id_y columns
df_schedule = df_schedule.drop(columns=['id_x', 'id_y'])

# Order by match_date_utc
df_schedule = df_schedule.sort_values(by='match_date_utc')

# If match_type is an empty string, set to 'Regular Season'
df_schedule['match_type'] = df_schedule['match_type'].apply(lambda x: 'Regular Season' if x == '' else x)

# Filter to remove Preseason games
df_schedule = df_schedule[df_schedule['match_type'] != 'Preseason']

# Create a column for the match date time in Adelaide timezone
df_schedule['match_date_adl'] = pd.to_datetime(df_schedule['match_date_utc']).dt.tz_convert('Australia/Adelaide')

# Add date scraped col
df_schedule['date_scraped'] = metadata_time

# Write out CSV
df_schedule.to_csv('Data/NBA_schedule.csv', index=False)