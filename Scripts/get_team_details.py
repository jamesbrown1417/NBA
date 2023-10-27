from nba_api.stats.static import teams
from nba_api.stats.endpoints import commonteamroster
import pandas as pd

#===============================================================================
# Get list of all teams
#===============================================================================

all_teams = teams.get_teams()
df_teams = pd.DataFrame(all_teams)

#===============================================================================
# Get Current Rosters
#===============================================================================

all_rosters = []

for team_id in df_teams['id'].unique():
    roster = commonteamroster.CommonTeamRoster(team_id=team_id)
    data_frames = roster.get_data_frames()
    roster_df = data_frames[0]
    print(roster_df)
    all_rosters.append(roster_df)
    
df_rosters = pd.concat(all_rosters)

#===============================================================================
# Write out CSVs
#===============================================================================

# Write out all teams
df_teams.to_csv('Data/all_teams.csv', index=False)

# Write out all rosters
df_rosters.to_csv('Data/all_rosters.csv', index=False)
