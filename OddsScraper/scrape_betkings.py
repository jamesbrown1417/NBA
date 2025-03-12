# Load necessary libraries
import requests
import pandas as pd
import numpy as np
import json
import re
from typing import List, Dict, Any, Optional, Union
import asyncio
from playwright.async_api import async_playwright

# Get teams table
teams = pd.read_csv("Data/all_teams.csv")

# Get player names table
player_names_all = pd.read_csv("Data/all_rosters.csv")
player_names_all = player_names_all[['PLAYER', 'TeamID']].rename(columns={'PLAYER': 'player_full_name'})
player_names_all = pd.merge(
    player_names_all,
    teams[['id', 'full_name']],
    left_on='TeamID',
    right_on='id',
    how='left'
)
player_names_all['first_initial'] = player_names_all['player_full_name'].str[0]
# Fix the surname extraction using a capture group
player_names_all['surname'] = player_names_all['player_full_name'].str.extract(r'(?<=\s)(.*$)')
player_names_all['join_name'] = player_names_all['first_initial'] + ' ' + player_names_all['surname']
player_names_all = player_names_all.rename(columns={'full_name': 'team_name'})

# unique join names
player_names_unique = player_names_all.groupby('join_name').filter(lambda x: len(x) == 1)

# Non unique names (take first two letters of first name)
player_names_non_unique = player_names_all.groupby('join_name').filter(lambda x: len(x) > 1)
player_names_non_unique['first_initial'] = player_names_non_unique['player_full_name'].str[:2]
player_names_non_unique['join_name'] = player_names_non_unique['first_initial'] + ' ' + player_names_non_unique['surname']

player_names = pd.concat([player_names_unique, player_names_non_unique])
player_names.loc[player_names['player_full_name'] == "Keyontae Johnson", 'join_name'] = "Key Johnson"
player_names.loc[player_names['player_full_name'] == "Miles Bridges", 'join_name'] = "Mil Bridges"
player_names.loc[player_names['player_full_name'] == "Jaylin Williams", 'join_name'] = "Jay Williams"

# Function to get event IDs using Playwright
async def get_event_ids():
    betkings_main_nba_url = "https://www.betkings.com.au/sportsbook/Basketball/United%20States/NBA"
    event_ids = []
    
    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True)
        page = await browser.new_page()
        
        # Navigate to the main NBA page
        await page.goto(betkings_main_nba_url)
        
        # Wait for the event links to load
        await page.wait_for_selector('.event-participants')
        
        # Extract all href links with class 'event-participants'
        event_links = await page.eval_on_selector_all('.event-participants', 
                                                     'elements => elements.map(el => el.getAttribute("href"))')
        
        # Extract the event ID from the href links
        for link in event_links:
            match = re.search(r'(?<=\?e=)\d+', link)
            if match:
                event_ids.append(match.group())
                
        await browser.close()
    
    return event_ids

# Run the async function to get event IDs
event_ids = asyncio.run(get_event_ids())

# Define URLs with meaningful names
player_points_url = [f"https://site-gateway.betkings.com.au/sportsbook/event/{event_id}?includeMarketGroups=PLAYER_POINTS&locale=ENG" for event_id in event_ids]
player_rebounds_url = [f"https://site-gateway.betkings.com.au/sportsbook/event/{event_id}?includeMarketGroups=PLAYER_REBOUNDS&locale=ENG" for event_id in event_ids]
player_assists_url = [f"https://site-gateway.betkings.com.au/sportsbook/event/{event_id}?includeMarketGroups=PLAYER_ASSISTS&locale=ENG" for event_id in event_ids]
player_blocks_url = [f"https://site-gateway.betkings.com.au/sportsbook/event/{event_id}?includeMarketGroups=PLAYER_BLOCKS&locale=ENG" for event_id in event_ids]
player_steals_url = [f"https://site-gateway.betkings.com.au/sportsbook/event/{event_id}?includeMarketGroups=PLAYER_STEALS&locale=ENG" for event_id in event_ids]
player_three_pointers_url = [f"https://site-gateway.betkings.com.au/sportsbook/event/{event_id}?includeMarketGroups=PLAYER_THREE_POINTERS&locale=ENG" for event_id in event_ids]
player_par_url = [f"https://site-gateway.betkings.com.au/sportsbook/event/{event_id}?includeMarketGroups=PLAYER_POINTS_ASSISTS_AND_REBOUNDS&locale=ENG" for event_id in event_ids]

# Function to retrieve JSON data and save it to a named list
def get_json_data(url):
    headers = {
        'accept': 'application/json',
        'accept-encoding': 'gzip, deflate, br, zstd',
        'accept-language': 'en-US,en;q=0.9',
        'content-type': 'application/json',
        'device': 'desktop',
        'origin': 'https://www.betkings.com.au',
        'priority': 'u=1, i',
        'referer': 'https://www.betkings.com.au/',
        'sec-ch-ua': '"Chromium";v="130", "Google Chrome";v="130", "Not?A_Brand";v="99"',
        'sec-ch-ua-mobile': '?0',
        'sec-ch-ua-platform': '"Windows"',
        'sec-fetch-dest': 'empty',
        'sec-fetch-mode': 'cors',
        'sec-fetch-site': 'same-site',
        'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36',
        'version': '3.41.2',
        'x-local-language': 'en-US',
        'x-local-platform': 'Browser',
        'x-local-time-zone': 'Australia/Adelaide',
        'x-project-id': '2'
    }
    
    response = requests.get(url, headers=headers)
    
    if response.status_code == 200:
        json_data = response.json()
        return json_data
    else:
        print(f"Failed to retrieve data. Status code: {response.status_code}")
        return None

# Function to take a list of JSON data and extract the necessary information
def process_props(json_data):
    if json_data is None or 'marketTypes' not in json_data:
        return pd.DataFrame()
    
    prop_data_list = []
    
    # Number of markets to iterate over
    for market_type in json_data['marketTypes']:
        # Get the match name and market type
        match_name = json_data['name']
        market = market_type['name']
        markets = market_type['markets']
        
        # Loop over markets and extract player data
        prop_name_1 = []
        prop_name_2 = []
        price_1 = []
        price_2 = []
        
        for market_item in markets:
            if 'outcome' in market_item and len(market_item['outcome']) > 0:
                prop_name_1.append(market_item['outcome'][0]['name'])
                price_1.append(market_item['outcome'][0]['odds'])
                
                # Check if there are two outcomes
                if len(market_item['outcome']) == 1:
                    prop_name_2.append(None)
                    price_2.append(None)
                else:
                    prop_name_2.append(market_item['outcome'][1]['name'])
                    price_2.append(market_item['outcome'][1]['odds'])
        
        # Combine
        prop_name = prop_name_1 + prop_name_2
        price = price_1 + price_2
        
        # Create a DataFrame with the extracted data
        prop_data = pd.DataFrame({
            'match_name': match_name,
            'market': market,
            'prop_name': prop_name,
            'price': price
        })
        
        prop_data_list.append(prop_data)
    
    if prop_data_list:
        output_df = pd.concat(prop_data_list)
        output_df = output_df.dropna(subset=['prop_name'])
        output_df.columns = ['match_name', 'market_name', 'prop_name', 'price']
        return output_df
    else:
        return pd.DataFrame(columns=['match_name', 'market_name', 'prop_name', 'price'])

#===============================================================================
# Get player points data
#===============================================================================

# Get JSON data for player points
player_points_data = [get_json_data(url) for url in player_points_url]

# Extract data from JSON
player_points_dfs = [process_props(data) for data in player_points_data if data is not None]
player_points_df = pd.concat(player_points_dfs) if player_points_dfs else pd.DataFrame()

# Get Just Overs
player_points_overs = player_points_df[player_points_df['prop_name'].str.contains('Over', na=False)].copy()
player_points_overs[['player_name', 'line']] = player_points_overs['prop_name'].str.split(' Over ', n=1, expand=True)
player_points_overs['line'] = player_points_overs['line'].str.replace('\\(', '', regex=True)
player_points_overs['line'] = player_points_overs['line'].str.replace('\\)', '', regex=True)
player_points_overs['line'] = player_points_overs['line'].astype(float)
player_points_overs = player_points_overs[['match_name', 'market_name', 'player_name', 'line', 'price']]
player_points_overs = player_points_overs.rename(columns={'match_name': 'match', 'market_name': 'market_name', 'price': 'over_price'})
player_points_overs['market_name'] = 'Player Points'

# Get Just Unders
player_points_unders = player_points_df[player_points_df['prop_name'].str.contains('Under', na=False)].copy()
player_points_unders[['player_name', 'line']] = player_points_unders['prop_name'].str.split(' Under ', n=1, expand=True)
player_points_unders['line'] = player_points_unders['line'].str.replace('\\(', '', regex=True)
player_points_unders['line'] = player_points_unders['line'].str.replace('\\)', '', regex=True)
player_points_unders['line'] = player_points_unders['line'].astype(float)
player_points_unders = player_points_unders[['match_name', 'market_name', 'player_name', 'line', 'price']]
player_points_unders = player_points_unders.rename(columns={'match_name': 'match', 'market_name': 'market_name', 'price': 'under_price'})
player_points_unders['market_name'] = 'Player Points'

# Combine
player_points_all = pd.merge(player_points_overs, player_points_unders, on=['match', 'market_name', 'player_name', 'line'], how='left')

# Join with player names data
player_points_all = pd.merge(player_points_all, player_names[['player_full_name', 'team_name']], 
                            left_on='player_name', right_on='player_full_name', how='left')
player_points_all = player_points_all.rename(columns={'team_name': 'player_team'})

# Process match information
player_points_all[['home_team', 'away_team']] = player_points_all['match'].str.split(' vs ', expand=True)
player_points_all['opposition_team'] = np.where(player_points_all['home_team'] == player_points_all['player_team'], 
                                               player_points_all['away_team'], 
                                               player_points_all['home_team'])
player_points_all['match'] = player_points_all['home_team'] + ' v ' + player_points_all['away_team']
player_points_all = player_points_all[['match', 'home_team', 'away_team', 'market_name', 'player_name', 
                                     'player_team', 'opposition_team', 'line', 'over_price', 'under_price']]
player_points_all['agency'] = 'BetKings'

#===============================================================================
# Get player assists data
#===============================================================================

# Get JSON data for player assists
player_assists_data = [get_json_data(url) for url in player_assists_url]

# Extract data from JSON
player_assists_dfs = [process_props(data) for data in player_assists_data if data is not None]
player_assists_df = pd.concat(player_assists_dfs) if player_assists_dfs else pd.DataFrame()

# Get Just Overs
player_assists_overs = player_assists_df[player_assists_df['prop_name'].str.contains('Over', na=False)].copy()
player_assists_overs[['player_name', 'line']] = player_assists_overs['prop_name'].str.split(' Over ', n=1, expand=True)
player_assists_overs['line'] = player_assists_overs['line'].str.replace('\\(', '', regex=True)
player_assists_overs['line'] = player_assists_overs['line'].str.replace('\\)', '', regex=True)
player_assists_overs['line'] = player_assists_overs['line'].astype(float)
player_assists_overs = player_assists_overs[['match_name', 'market_name', 'player_name', 'line', 'price']]
player_assists_overs = player_assists_overs.rename(columns={'match_name': 'match', 'market_name': 'market_name', 'price': 'over_price'})
player_assists_overs['market_name'] = 'Player Assists'

# Get Just Unders
player_assists_unders = player_assists_df[player_assists_df['prop_name'].str.contains('Under', na=False)].copy()
player_assists_unders[['player_name', 'line']] = player_assists_unders['prop_name'].str.split(' Under ', n=1, expand=True)
player_assists_unders['line'] = player_assists_unders['line'].str.replace('\\(', '', regex=True)
player_assists_unders['line'] = player_assists_unders['line'].str.replace('\\)', '', regex=True)
player_assists_unders['line'] = player_assists_unders['line'].astype(float)
player_assists_unders = player_assists_unders[['match_name', 'market_name', 'player_name', 'line', 'price']]
player_assists_unders = player_assists_unders.rename(columns={'match_name': 'match', 'market_name': 'market_name', 'price': 'under_price'})
player_assists_unders['market_name'] = 'Player Assists'

# Combine
player_assists_all = pd.merge(player_assists_overs, player_assists_unders, on=['match', 'market_name', 'player_name', 'line'], how='left')

# Join with player names data
player_assists_all = pd.merge(player_assists_all, player_names[['player_full_name', 'team_name']], 
                             left_on='player_name', right_on='player_full_name', how='left')
player_assists_all = player_assists_all.rename(columns={'team_name': 'player_team'})

# Process match information
player_assists_all[['home_team', 'away_team']] = player_assists_all['match'].str.split(' vs ', expand=True)
player_assists_all['opposition_team'] = np.where(player_assists_all['home_team'] == player_assists_all['player_team'], 
                                                player_assists_all['away_team'], 
                                                player_assists_all['home_team'])
player_assists_all['match'] = player_assists_all['home_team'] + ' v ' + player_assists_all['away_team']
player_assists_all = player_assists_all[['match', 'home_team', 'away_team', 'market_name', 'player_name', 
                                       'player_team', 'opposition_team', 'line', 'over_price', 'under_price']]
player_assists_all['agency'] = 'BetKings'

#===============================================================================
# Get player rebounds data
#===============================================================================

# Get JSON data for player rebounds
player_rebounds_data = [get_json_data(url) for url in player_rebounds_url]

# Extract data from JSON
player_rebounds_dfs = [process_props(data) for data in player_rebounds_data if data is not None]
player_rebounds_df = pd.concat(player_rebounds_dfs) if player_rebounds_dfs else pd.DataFrame()

# Get Just Overs
player_rebounds_overs = player_rebounds_df[player_rebounds_df['prop_name'].str.contains('Over', na=False)].copy()
player_rebounds_overs[['player_name', 'line']] = player_rebounds_overs['prop_name'].str.split(' Over ', n=1, expand=True)
player_rebounds_overs['line'] = player_rebounds_overs['line'].str.replace('\\(', '', regex=True)
player_rebounds_overs['line'] = player_rebounds_overs['line'].str.replace('\\)', '', regex=True)
player_rebounds_overs['line'] = player_rebounds_overs['line'].astype(float)
player_rebounds_overs = player_rebounds_overs[['match_name', 'market_name', 'player_name', 'line', 'price']]
player_rebounds_overs = player_rebounds_overs.rename(columns={'match_name': 'match', 'market_name': 'market_name', 'price': 'over_price'})
player_rebounds_overs['market_name'] = 'Player Rebounds'

# Get Just Unders
player_rebounds_unders = player_rebounds_df[player_rebounds_df['prop_name'].str.contains('Under', na=False)].copy()
player_rebounds_unders[['player_name', 'line']] = player_rebounds_unders['prop_name'].str.split(' Under ', n=1, expand=True)
player_rebounds_unders['line'] = player_rebounds_unders['line'].str.replace('\\(', '', regex=True)
player_rebounds_unders['line'] = player_rebounds_unders['line'].str.replace('\\)', '', regex=True)
player_rebounds_unders['line'] = player_rebounds_unders['line'].astype(float)
player_rebounds_unders = player_rebounds_unders[['match_name', 'market_name', 'player_name', 'line', 'price']]
player_rebounds_unders = player_rebounds_unders.rename(columns={'match_name': 'match', 'market_name': 'market_name', 'price': 'under_price'})
player_rebounds_unders['market_name'] = 'Player Rebounds'

# Combine
player_rebounds_all = pd.merge(player_rebounds_overs, player_rebounds_unders, on=['match', 'market_name', 'player_name', 'line'], how='left')

# Join with player names data
player_rebounds_all = pd.merge(player_rebounds_all, player_names[['player_full_name', 'team_name']], 
                              left_on='player_name', right_on='player_full_name', how='left')
player_rebounds_all = player_rebounds_all.rename(columns={'team_name': 'player_team'})

# Process match information
player_rebounds_all[['home_team', 'away_team']] = player_rebounds_all['match'].str.split(' vs ', expand=True)
player_rebounds_all['opposition_team'] = np.where(player_rebounds_all['home_team'] == player_rebounds_all['player_team'], 
                                                 player_rebounds_all['away_team'], 
                                                 player_rebounds_all['home_team'])
player_rebounds_all['match'] = player_rebounds_all['home_team'] + ' v ' + player_rebounds_all['away_team']
player_rebounds_all = player_rebounds_all[['match', 'home_team', 'away_team', 'market_name', 'player_name', 
                                         'player_team', 'opposition_team', 'line', 'over_price', 'under_price']]
player_rebounds_all['agency'] = 'BetKings'

#===============================================================================
# Write out
#===============================================================================

# Points
player_points_all.to_csv("Data/scraped_odds/betkings_player_points.csv", index=False)

# Assists
player_assists_all.to_csv("Data/scraped_odds/betkings_player_assists.csv", index=False)

# Rebounds
player_rebounds_all.to_csv("Data/scraped_odds/betkings_player_rebounds.csv", index=False)

# Main execution
if __name__ == "__main__":
    # Run the scraper
    print("Starting BetKings scraper...")
    print(f"Found {len(event_ids)} events")
    print("Scraping completed. Files saved to Data/scraped_odds/ directory.")