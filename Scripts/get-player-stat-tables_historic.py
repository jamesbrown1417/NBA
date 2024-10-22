import pandas as pd
from nba_api.stats.static import teams
from nba_api.stats.endpoints import leaguegamefinder, boxscoretraditionalv3
from urllib3.exceptions import ReadTimeoutError
from requests.exceptions import HTTPError
import time

# Get list of all NBA teams
nba_teams = teams.get_teams()

# ==========================================
# Function to Get Match IDs
# ==========================================

def get_all_match_ids(season_name):
    # Fetch all games for the specified season and season type
    gamefinder = leaguegamefinder.LeagueGameFinder(season_nullable=season_name, season_type_nullable='Regular Season')
    games_dict = gamefinder.get_dict()
    games = games_dict['resultSets'][0]['rowSet']
    
    # Create a DataFrame to hold the data
    df_match_ids = pd.DataFrame(games, columns=games_dict['resultSets'][0]['headers'])
    
    # Create home and away team columns
    df_match_ids['IS_HOME'] = df_match_ids['MATCHUP'].str.contains('vs.')
    df_match_ids['TEAM_TYPE'] = df_match_ids.apply(lambda row: 'HOME_TEAM' if row['IS_HOME'] else 'AWAY_TEAM', axis=1)
    
    # Filter to only team IDs in the NBA
    df_match_ids = df_match_ids[df_match_ids['TEAM_ID'].isin([team['id'] for team in nba_teams])]
    
    # Pivot the DataFrame so each GAME_ID has a single row
    df_pivot = df_match_ids.pivot(index='GAME_ID', columns='TEAM_TYPE', values='TEAM_NAME')
    
    # Reset the index for the new DataFrame
    df_pivot.reset_index(inplace=True)
    
    # Add GAME_DATE (it will be the same for both rows so we just take the first one)
    game_dates = df_match_ids.groupby('GAME_ID')['GAME_DATE'].first().reset_index()
    df_pivot = pd.merge(df_pivot, game_dates, on='GAME_ID', how='left')
    
    return df_pivot

# ==========================================
# Functions to Fetch Season Data
# ==========================================
def fetch_single_match_data(match_id, retries=3, delay=10):
    for i in range(retries):
        try:
            player_stats = boxscoretraditionalv3.BoxScoreTraditionalV3(game_id=match_id, timeout=120)  # Increased timeout to 120 seconds
            return player_stats.get_data_frames()[0]
        except (HTTPError, ReadTimeoutError) as e:
            wait_time = delay * (2 ** i)  # Exponential backoff
            print(f"An error occurred: {e}. Retrying in {wait_time} seconds.")
            time.sleep(wait_time)
    print(f"Failed to fetch data for {match_id} after {retries} retries.")
    return None

def fetch_season_data(match_id_list):
    all_player_stats = []
    
    total_matches = len(match_id_list)
    completed_matches = 0
    
    for match_id in match_id_list:
        try:
            player_stats_df = fetch_single_match_data(match_id)
            all_player_stats.append(player_stats_df)
            
            completed_matches += 1
            progress = (completed_matches / total_matches) * 100
            
            print(f"Done with match_id: {match_id} - Progress: {progress:.2f}%")
        except Exception as e:
            print(f"An error occurred while processing match_id: {match_id}. Error: {e}")

    all_player_stats_df = pd.concat(all_player_stats)
    return all_player_stats_df

# ==========================================
# Fetch and Save Data for 2023-24 Season
# ==========================================

# Fetch match IDs for 2023-24 season
matches_2023_24 = get_all_match_ids("2023-24")
match_id_list_2023_24 = list(set(matches_2023_24.GAME_ID))

# Get data for 2023-24 season
df_2023_24 = fetch_season_data(match_id_list_2023_24)

# Add match information
df_2023_24 = pd.merge(df_2023_24, matches_2023_24, left_on='gameId', right_on='GAME_ID', how='left')

# Reset Index
df_2023_24.reset_index(inplace=True)

# Write out as a CSV file
df_2023_24.to_csv('Data/all_player_stats_2023-2024.csv', index=False)
