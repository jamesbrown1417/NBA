"""
Single-run Bet365 scraper using one driverless Chrome instance.

Performs both steps in order:
1) Load main market page and save H2H HTML
2) Collect player prop URLs and save each match's player HTML
"""

# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime
import asyncio
import pandas as pd
import os
from dotenv import load_dotenv

# Load environment variables: try default .env, then fallback to 'env'
load_dotenv()
# Fallback to a non-dotted 'env' file present in the repo
if os.getenv('BET365USER') is None or os.getenv('BET365PW') is None:
    load_dotenv('/Users/jamesbrown/Projects/NBA/env')

# Read credentials after loading
username = os.getenv('BET365USER')
password = os.getenv('BET365PW')

# Validate credentials early with a clear error
if not username or not password:
    raise RuntimeError(
        "Missing Bet365 credentials. Set BET365USER and BET365PW in .env or env, or export them in the environment."
    )

# Player prop URL suffixes and their corresponding market buttons
PROP_CATEGORIES = {
    'I43': {
        'name': 'Points',
        'buttons': ['Points O/U']
    },
    'I45': {
        'name': 'Threes',
        'buttons': ['Threes Made O/U']
    },
    'I46': {
        'name': 'Assists',
        'buttons': ['Assists O/U']
    },
    'I47': {
        'name': 'Rebounds',
        'buttons': ['Rebounds O/U']
    },
    'I48': {
        'name': 'Combos',
        'buttons': ['Double Double', 'Triple Double', 'Points, Assists & Rebounds', 'Points, Assists & Rebounds O/U']
    },
    'I49': {
        'name': 'Defence',
        'buttons': ['Steals & Blocks O/U']
    },
}

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in fixture dataset====================================================
NBA_schedule = pd.read_csv("Data/NBA_schedule.csv")

# Get number of matches currently in play
NBA_schedule["match_date_adl"] = pd.to_datetime(NBA_schedule["match_date_adl"], utc=True).dt.tz_convert('Australia/Adelaide')
now = pd.Timestamp.now(tz='Australia/Adelaide')

# Get only matches from the closest day with games that is on
NBA_schedule = NBA_schedule[NBA_schedule["match_date_adl"].dt.date >= now.date()]
NBA_schedule = NBA_schedule[NBA_schedule["match_date_adl"].dt.date == NBA_schedule["match_date_adl"].dt.date.min()]

# Get number of games that have started
started = NBA_schedule[NBA_schedule["match_date_adl"] < now]

# Create range from len started to len schedule
match_range = range(len(started), len(NBA_schedule))


async def collect_h2h_and_urls(driver):
    """Navigate to main page, save H2H HTML, and return list of player URLs per match."""
    await driver.get('https://www.bet365.com.au/#/AC/B18/C20604387/D48/E1453/F10/')
    await driver.sleep(2)
    # Always perform login each run
    print("Attempting login...")
    login_element = await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')] | //button[contains(@class, 'hrm-79') and contains(text(), 'Log In')]", timeout=10)
    await driver.sleep(2)
    await login_element.click()
    await driver.sleep(1)

    username_field = await driver.find_element(By.XPATH, "//input[@placeholder='Username or email address']", timeout=10)
    await username_field.clear()
    await driver.sleep(0.3)
    await username_field.send_keys(username)
    print("Entered username")

    password_field = await driver.find_element(By.XPATH, "//input[@placeholder='Password']", timeout=10)
    await password_field.clear()
    await driver.sleep(0.3)
    await password_field.send_keys(password)
    print("Entered password")

    login_button = await driver.find_element(By.XPATH, "//span[starts-with(@class, 'slm')]", timeout=5)
    await login_button.click()
    print("Clicked login button")

    print("Waiting 2 seconds...")
    await driver.sleep(2)
    
    await driver.minimize_window()
    
    # Wait for market container after login
    elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroup_Wrapper')]", timeout=10)
    print("Market container found after login")

    # Save HTML    
    body_html = await elem.get_attribute('outerHTML')

    with open("OddsScraper/Bet365/HTML/h2h_html.txt", 'w') as f:
        f.write(body_html)

    print("Waiting 2 seconds...")
    await driver.sleep(2)

    # Find team rows to discover match URLs
    team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'scb-ParticipantFixtureDetailsHigherBasketball_TeamNames')]")

    for team_element in team_elements:
        try:
            print(await team_element.get_attribute('innerText'))
        except Exception:
            pass

    # Collect base URLs for each match
    match_data = []  # List of dicts: {'match_index': int, 'base_url': str}
    
    for index in match_range:
        print(f"Getting base URL for match {index}")
        team_elements = await driver.find_elements(
            By.XPATH,
            "//div[contains(@class, 'scb-ParticipantFixtureDetailsHigherBasketball_TeamNames')]",
        )

        if index >= len(team_elements):
            print(
                f"Skipping match {index}: Index out of range. (Found {len(team_elements)} matches on site, tried accessing index {index})"
            )
            continue

        await driver.execute_script(
            "arguments[0].scrollIntoView(true);", team_elements[index]
        )
        await driver.execute_script("window.scrollBy(0, -150)")
        await driver.sleep(0.1)

        await team_elements[index].click()

        cur_url = await driver.current_url
        match_data.append({
            'match_index': index,
            'base_url': cur_url
        })

        await driver.back()
        await driver.sleep(0.5)

    # Optionally persist URL list for debugging/traceability
    try:
        all_urls = []
        for match in match_data:
            for suffix in PROP_CATEGORIES.keys():
                all_urls.append(f"{match['base_url']}{suffix}/")
        with open("OddsScraper/Bet365/player_urls.csv", 'w') as f:
            f.write('\n'.join(all_urls))
    except Exception:
        pass

    return match_data


async def scrape_player_pages(driver, match_data):
    """Iterate matches and their category URLs, expand relevant sections, and save HTML."""
    
    async def maybe_click(xpath_text, label):
        """Attempt to click a market expansion button by its text."""
        try:
            el = await driver.find_element(
                By.XPATH, 
                f"//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='{xpath_text}']"
            )
            await driver.execute_script("arguments[0].scrollIntoView(true);", el)
            await driver.execute_script("window.scrollBy(0, -150)")
            await el.click()
            print(f"  Clicked '{label}'")
            await driver.sleep(1.5)
        except Exception:
            print(f"  No '{label}' button found")

    async def click_show_more_buttons():
        """Click all visible 'Show more' buttons on the page."""
        button_elements = await driver.find_elements(
            By.XPATH, 
            "//div[contains(@class, 'msl-ShowMore_Link ') and contains(text(), 'Show more')]"
        )
        for button_element in button_elements:
            try:
                await driver.execute_script("arguments[0].scrollIntoView(true);", button_element)
                await driver.execute_script("window.scrollBy(0, -150)")
                await button_element.click()
                await driver.sleep(1)
            except Exception:
                pass

    for match in match_data:
        match_index = match['match_index']
        base_url = match['base_url']
        
        print(f"\n{'='*60}")
        print(f"Processing match {match_index}")
        print(f"{'='*60}")
        
        for suffix, category_info in PROP_CATEGORIES.items():
            category_name = category_info['name']
            buttons_to_click = category_info['buttons']
            url = f"{base_url}{suffix}/"
            
            try:
                print(f"\n--- {category_name} ({suffix}) ---")
                print(f"URL: {url}")
                
                await driver.get(url)

                # Wait for a market group button to appear
                await driver.find_element(
                    By.XPATH, 
                    "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ')]", 
                    timeout=5
                )
                
                await driver.sleep(1.5)
                
                # Click only the buttons relevant to this category
                for button_text in buttons_to_click:
                    await maybe_click(button_text, button_text)
                
                # Click all "Show more" buttons
                await click_show_more_buttons()

                # Grab and write the player page HTML for this match/category
                elem = await driver.find_element(
                    By.XPATH, 
                    "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]"
                )
                body_html_players = await elem.get_attribute('outerHTML')
                
                filename = f"OddsScraper/Bet365/HTML/body_html_players_match_{match_index}_{category_name.lower()}.txt"
                with open(filename, 'w') as f:
                    f.write(body_html_players)
                print(f"  Saved: {filename}")

            except Exception as e:
                print(f"  Error with {category_name}: {e}. Continuing...")
                continue


async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        match_data = await collect_h2h_and_urls(driver)
        await scrape_player_pages(driver, match_data)


if __name__ == "__main__":
    asyncio.run(main())