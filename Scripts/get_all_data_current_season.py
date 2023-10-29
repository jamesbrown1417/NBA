import pandas as pd
import time
from nba_api.stats.static import players
from nba_api.stats.endpoints import playergamelogs

def fetch_player_stats(player_name: str, seasons: list):
    try:
        # Retrieve player's ID
        player_info = players.find_players_by_full_name(player_name)
        if not player_info:
            print(f"No data found for player: {player_name}")
            return pd.DataFrame()
        player_id = player_info[0]['id']

        # Fetch game logs for the player for the specified seasons
        frames = [playergamelogs.PlayerGameLogs(player_id_nullable=player_id, season_nullable=season, timeout=100).get_data_frames()[0] for season in seasons]

        # Combine game logs from all specified seasons
        df_player = pd.concat(frames)
        
        # Add player name to DataFrame and return
        df_player['Player'] = player_name
        return df_player
    
    except:
        print(f"Error fetching data for player: {player_name}")
        return pd.DataFrame()

# Get list of all player names and IDs active in the 2023-24 season
all_players = players.get_players()
df_all_players = pd.DataFrame(all_players)
df_all_players = df_all_players[df_all_players['is_active'] == True]

# 2023-24 season----------------------------------------------

# Fetch stats for all active players
all_player_stats = []
for _, row in df_all_players.iterrows():
    player_stats = fetch_player_stats(row['full_name'], ["2023-24"])
    all_player_stats.append(player_stats)
    print(f"Processed {row['full_name']}")
    
    # Sleep for 2 seconds to prevent rate-limiting
    time.sleep(0.6)

# Concatenate all player stats into a single DataFrame
df_all_player_stats = pd.concat(all_player_stats)

# Write out as a CSV file
df_all_player_stats.to_csv('Data/all_player_stats_2023-2024.csv', index=False)