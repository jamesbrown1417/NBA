import pandas as pd
from nba_api.stats.static import teams
from nba_api.stats.endpoints import leaguegamefinder, boxscoretraditionalv3, boxscoreadvancedv3, boxscoreplayertrackv3, boxscoremiscv3, boxscorescoringv3
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
# Fetch Match ID, Game Date, Match Name
# ==========================================

# 2024-25 season 
matches_2024_25 = get_all_match_ids("2024-25")
match_id_list_2024_25 = list(set(matches_2024_25.GAME_ID))

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
# Get IDs already done
# ==========================================

# Read in current dataset
current_data = pd.read_csv('Data/all_player_stats_2024-2025.csv', dtype={'gameId': str})

# Filter to only games not in current dataset
match_id_list_2024_25_new = list(set(matches_2024_25.GAME_ID) - set(current_data.gameId))

# ==========================================
# Fetch and Save Data for 2024-25 Season
# ==========================================

# Get data for 2024-25 season
df_2024_25 = fetch_season_data(match_id_list_2024_25_new)

# Add match information
df_2024_25 = pd.merge(df_2024_25, matches_2024_25, left_on='gameId', right_on='GAME_ID', how='left')

# Bind rows of old and new data
df_2024_25 = pd.concat([current_data, df_2024_25])

# Write out as a CSV file
df_2024_25.to_csv('Data/all_player_stats_2024-2025.csv', index=False)

#====================================================================================================
# Get Advanced Team Stats   
#====================================================================================================

#=====================================================#
#                Get for season 2024-25               #
#=====================================================#

# ==========================================
# Functions to Fetch Advanced Team Stats
# ==========================================

def fetch_single_match_team_stats(match_id, retries=3, delay=10):
    for i in range(retries):
        try:
            team_stats = boxscoreadvancedv3.BoxScoreAdvancedV3(game_id=match_id, timeout=120)  # Increased timeout
            return team_stats.get_data_frames()[1]
        except (HTTPError, ReadTimeoutError) as e:
            wait_time = delay * (2 ** i)  # Exponential backoff
            print(f"An error occurred: {e}. Retrying in {wait_time} seconds.")
            time.sleep(wait_time)
    print(f"Failed to fetch team stats for {match_id} after {retries} retries.")
    return None

def fetch_advanced_team_stats(match_id_list):
    all_team_stats = []
    
    total_matches = len(match_id_list)
    completed_matches = 0
    
    for match_id in match_id_list:
        team_stats_df = fetch_single_match_team_stats(match_id)
        if team_stats_df is not None:
            all_team_stats.append(team_stats_df)
            completed_matches += 1
            progress = (completed_matches / total_matches) * 100
            print(f"Done with match_id: {match_id} - Progress: {progress:.2f}%")
     
    all_team_stats_df = pd.concat(all_team_stats, ignore_index=True)
    return all_team_stats_df

# =================================================
# Fetch and Save Advanced Data for 2024-25 Season
# =================================================

# Assuming you have an existing CSV with previous data for advanced team stats
try:
    current_advanced_data = pd.read_csv('Data/advanced_box_scores_2024-2025.csv', dtype={'gameId': str})
    existing_match_ids = current_advanced_data['gameId'].unique()
except FileNotFoundError:
    current_advanced_data = pd.DataFrame()
    existing_match_ids = []

# Filter out match IDs that have already been fetched
new_match_ids = list(set(matches_2024_25.GAME_ID) - set(existing_match_ids))

# Fetch new data
new_advanced_team_stats_df = fetch_advanced_team_stats(new_match_ids)

# Combine old and new data
combined_advanced_team_stats_df = pd.concat([current_advanced_data, new_advanced_team_stats_df], ignore_index=True)

# Write out as a CSV file
combined_advanced_team_stats_df.to_csv('Data/advanced_box_scores_2024-2025.csv', index=False)

#====================================================================================================
# Get Player Track Stats   
#====================================================================================================

#=====================================================#
#                Get for season 2024-25               #
#=====================================================#

# ==========================================
# Functions to Fetch Player Track Stats
# ==========================================

def fetch_single_match_player_track(match_id, retries=3, delay=10):
    for i in range(retries):
        try:
            player_stats = boxscoreplayertrackv3.BoxScorePlayerTrackV3(game_id=match_id, timeout=120)  # Increased timeout
            return player_stats.get_data_frames()[0]
        except (HTTPError, ReadTimeoutError) as e:
            wait_time = delay * (2 ** i)  # Exponential backoff
            print(f"An error occurred: {e}. Retrying in {wait_time} seconds.")
            time.sleep(wait_time)
    print(f"Failed to fetch player track stats for {match_id} after {retries} retries.")
    return None

def fetch_player_track_stats(match_id_list):
    all_player_track_stats = []
    
    total_matches = len(match_id_list)
    completed_matches = 0
    
    for match_id in match_id_list:
        player_track_stats_df = fetch_single_match_player_track(match_id)
        if player_track_stats_df is not None:
            all_player_track_stats.append(player_track_stats_df)
            completed_matches += 1
            progress = (completed_matches / total_matches) * 100
            print(f"Done with match_id: {match_id} - Progress: {progress:.2f}%")
    
    all_player_track_stats_df = pd.concat(all_player_track_stats, ignore_index=True)
    return all_player_track_stats

# ====================================================
# Fetch and Save Player Track Data for 2024-25 Season
# ====================================================

# Assuming you have an existing CSV with previous data for player track stats
try:
    current_player_track_data = pd.read_csv('Data/player_track_box_scores_2024-2025.csv', dtype={'gameId': str})
    existing_match_ids = current_player_track_data['gameId'].unique()
except FileNotFoundError:
    current_player_track_data = pd.DataFrame()
    existing_match_ids = []

# Filter out match IDs that have already been fetched
new_match_ids = list(set(matches_2024_25.GAME_ID) - set(existing_match_ids))

# Fetch new data
new_player_track_stats_df = fetch_player_track_stats(new_match_ids)

# Combine old and new data
combined_player_track_stats_df = pd.concat([current_player_track_data, new_player_track_stats_df], ignore_index=True)

# Write out as a CSV file
combined_player_track_stats_df.to_csv('Data/player_track_box_scores_2024-2025.csv', index=False)

#====================================================================================================
# Get Team Misc Box Score Stats
#====================================================================================================

#=====================================================#
#                Get for season 2024-25               #
#=====================================================#

# ==========================================
# Functions to Fetch misc Team Stats
# ==========================================

def fetch_single_match_misc_stats(match_id, retries=3, delay=10):
    for i in range(retries):
        try:
            team_stats = boxscoremiscv3.BoxScoreMiscV3(game_id=match_id, timeout=120)  # Increased timeout
            return team_stats.get_data_frames()[1]
        except (HTTPError, ReadTimeoutError) as e:
            wait_time = delay * (2 ** i)  # Exponential backoff
            print(f"An error occurred: {e}. Retrying in {wait_time} seconds.")
            time.sleep(wait_time)
    print(f"Failed to fetch team stats for {match_id} after {retries} retries.")
    return None

def fetch_misc_team_stats(match_id_list):
    all_team_stats = []
    
    total_matches = len(match_id_list)
    completed_matches = 0
    
    for match_id in match_id_list:
        team_stats_df = fetch_single_match_misc_stats(match_id)
        if team_stats_df is not None:
            all_team_stats.append(team_stats_df)
            completed_matches += 1
            progress = (completed_matches / total_matches) * 100
            print(f"Done with match_id: {match_id} - Progress: {progress:.2f}%")
    
    all_team_stats_df = pd.concat(all_team_stats, ignore_index=True)
    return all_team_stats_df

# =================================================
# Fetch and Save misc Data for 2024-25 Season
# =================================================

# Assuming you have an existing CSV with previous data for misc team stats
try:
    current_misc_data = pd.read_csv('Data/misc_box_scores_2024-2025.csv', dtype={'gameId': str})
    existing_match_ids = current_misc_data['gameId'].unique()
except FileNotFoundError:
    current_misc_data = pd.DataFrame()
    existing_match_ids = []

# Filter out match IDs that have already been fetched
new_match_ids = list(set(matches_2024_25.GAME_ID) - set(existing_match_ids))

# Fetch new data
new_misc_team_stats_df = fetch_misc_team_stats(new_match_ids)

# Combine old and new data
combined_misc_team_stats_df = pd.concat([current_misc_data, new_misc_team_stats_df], ignore_index=True)

# Write out as a CSV file
combined_misc_team_stats_df.to_csv('Data/misc_box_scores_2024-2025.csv', index=False)

#====================================================================================================
# Get Player Misc Box Score Stats
#====================================================================================================

#=====================================================#
#                Get for season 2024-25               #
#=====================================================#

# ==========================================
# Functions to Fetch misc Player Stats
# ==========================================

def fetch_single_match_misc_stats_player(match_id, retries=3, delay=10):
    for i in range(retries):
        try:
            team_stats = boxscoremiscv3.BoxScoreMiscV3(game_id=match_id, timeout=120)  # Increased timeout
            return team_stats.get_data_frames()[0]
        except (HTTPError, ReadTimeoutError) as e:
            wait_time = delay * (2 ** i)  # Exponential backoff
            print(f"An error occurred: {e}. Retrying in {wait_time} seconds.")
            time.sleep(wait_time)
    print(f"Failed to fetch team stats for {match_id} after {retries} retries.")
    return None

def fetch_misc_player_stats(match_id_list):
    all_team_stats = []
    
    total_matches = len(match_id_list)
    completed_matches = 0
    
    for match_id in match_id_list:
        team_stats_df = fetch_single_match_misc_stats_player(match_id)
        if team_stats_df is not None:
            all_team_stats.append(team_stats_df)
            completed_matches += 1
            progress = (completed_matches / total_matches) * 100
            print(f"Done with match_id: {match_id} - Progress: {progress:.2f}%")
    
    all_team_stats_df = pd.concat(all_team_stats, ignore_index=True)
    return all_team_stats_df

# =================================================
# Fetch and Save misc Data for 2024-25 Season
# =================================================

# Assuming you have an existing CSV with previous data for misc team stats
try:
    current_misc_data_player = pd.read_csv('Data/misc_box_scores_player_2024-2025.csv', dtype={'gameId': str})
    existing_match_ids = current_misc_data['gameId'].unique()
except FileNotFoundError:
    current_misc_data_player = pd.DataFrame()
    existing_match_ids = []

# Filter out match IDs that have already been fetched
new_match_ids = list(set(matches_2024_25.GAME_ID) - set(existing_match_ids))

# Fetch new data
new_misc_player_stats_df = fetch_misc_player_stats(new_match_ids)

# Combine old and new data
combined_misc_player_stats_df = pd.concat([current_misc_data_player, new_misc_player_stats_df], ignore_index=True)

# Write out as a CSV file
combined_misc_player_stats_df.to_csv('Data/misc_box_scores_player_2024-2025.csv', index=False)
