import pandas as pd
from nba_api.stats.static import players
from nba_api.stats.endpoints import playergamelog
import matplotlib.pyplot as plt
import seaborn as sns

def plot_player_stats(player_name: str, seasons: list, stat: str = 'PTS', reference_value: int = 20):
    # Retrieve player's ID
    player_info = players.find_players_by_full_name(player_name)
    if not player_info:
        print(f"No data found for player: {player_name}")
        return
    player_id = player_info[0]['id']

    # Initialize an empty DataFrame to store game logs
    df_player = pd.DataFrame()

    # Fetch game logs for the player for each season
    for season in seasons:
        gamelog = playergamelog.PlayerGameLog(player_id=player_id, season=season)
        df_season = gamelog.get_data_frames()[0]
        df_player = pd.concat([df_player, df_season])

    # Calculate the proportion of games where the player achieved the specified stat above the reference value
    df_player[f'{reference_value}+ {stat}'] = df_player[stat] >= reference_value
    player_mean = df_player[f'{reference_value}+ {stat}'].mean()
    player_implied_odds = 1 / player_mean

    # Set Seaborn aesthetic enhancements
    sns.set_style("whitegrid")
    plt.figure(figsize=(12,7))

    # Create a scatter plot using Seaborn
    sns.scatterplot(x=df_player['GAME_DATE'], y=df_player[stat], color='blue', label=f'{stat} by Game')
    plt.axhline(y=reference_value, color='red', linestyle='--', label=f'{reference_value} {stat} Reference')

    # Adding title, labels, and text for mean and implied odds
    plt.title(f'{player_name} {stat} by Game', fontsize=16)
    plt.xlabel('Game Date', fontsize=14)
    plt.ylabel(stat, fontsize=14)
    plt.xticks(rotation=45, fontsize=12)
    plt.yticks(fontsize=12)
    plt.legend(fontsize=12)

    # Display mean and implied odds on the plot
    text_str = f"Observed Probability: {player_mean:.2f}\nImplied Odds: {player_implied_odds:.2f}"
    plt.text(0.02, 0.95, text_str, transform=plt.gca().transAxes, fontsize=12, verticalalignment='top', bbox=dict(boxstyle="round,pad=0.3", edgecolor="black", facecolor="aliceblue"))

    # Display the plot
    plt.tight_layout()
    plt.show()

# Example usage:
plot_player_stats("Kevin Durant", ["2022-23"], stat='AST', reference_value=4.5)
