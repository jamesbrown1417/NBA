from nba_api.stats.endpoints import boxscoreadvancedv3
from nba_api.stats.endpoints import leaguegamefinder
from nba_api.stats.static import teams
import pandas as pd

# Get all teams
nba_teams = teams.get_teams()

#===================================================================================================#
#                                Get for season 2022-23                                             #
#===================================================================================================#

# Initialize an empty list to store match IDs
all_match_ids = []

# Define the season
season = '2022-23'

# Loop through all teams to get matches
for team in nba_teams:
    team_id = team['id']
    gamefinder = leaguegamefinder.LeagueGameFinder(team_id_nullable=team_id, season_nullable=season)
    games_dict = gamefinder.get_dict()
    games = games_dict['resultSets'][0]['rowSet']

    # Extract match IDs and add them to the list
    for game in games:
        game_id = game[4]
        all_match_ids.append(game_id)

# Remove duplicates
all_match_ids = list(set(all_match_ids))

# Initialize an empty list to store dataframes
all_team_stats = []

# Loop through all match IDs to get team stats
for match_id in all_match_ids:
    team_stats = boxscoreadvancedv3.BoxScoreAdvancedV3(game_id=match_id)
    team_stats_df = team_stats.get_data_frames()[1]
    all_team_stats.append(team_stats_df)
    print("done with match_id: " + str(match_id))
    
# Concatenate all dataframes
all_team_stats_df = pd.concat(all_team_stats)

# Save dataframe to csv
all_team_stats_df.to_csv('advanced_box_scores_2022-2023.csv', index=False)

#===================================================================================================#
#                                Get for season 2021-22                                             #
#===================================================================================================#

# Initialize an empty list to store match IDs
all_match_ids = []

# Define the season
season = '2021-22'

# Loop through all teams to get matches
for team in nba_teams:
    team_id = team['id']
    gamefinder = leaguegamefinder.LeagueGameFinder(team_id_nullable=team_id, season_nullable=season)
    games_dict = gamefinder.get_dict()
    games = games_dict['resultSets'][0]['rowSet']

    # Extract match IDs and add them to the list
    for game in games:
        game_id = game[4]
        all_match_ids.append(game_id)

# Remove duplicates
all_match_ids = list(set(all_match_ids))

# Initialize an empty list to store dataframes
all_team_stats = []

# Loop through all match IDs to get team stats
for match_id in all_match_ids:
    team_stats = boxscoreadvancedv3.BoxScoreAdvancedV3(game_id=match_id)
    team_stats_df = team_stats.get_data_frames()[1]
    all_team_stats.append(team_stats_df)
    print("done with match_id: " + str(match_id))
    
# Concatenate all dataframes
all_team_stats_df = pd.concat(all_team_stats)

# Save dataframe to csv
all_team_stats_df.to_csv('advanced_box_scores_2021-2022.csv', index=False)