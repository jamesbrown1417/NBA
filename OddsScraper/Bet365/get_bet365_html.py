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

# Load environment variables
load_dotenv()
username = os.getenv('BET365USER')
password = os.getenv('BET365PW')

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in fixture dataset====================================================
NBA_schedule = pd.read_csv("Data/NBA_schedule.csv")

# Get only matches after current time
NBA_schedule["match_date_adl"] = pd.to_datetime(NBA_schedule["match_date_adl"], utc=True).dt.tz_convert('Australia/Adelaide')
now = pd.Timestamp.now(tz='Australia/Adelaide')
NBA_schedule = NBA_schedule[NBA_schedule["match_date_adl"] > now]

# Get only matches from the closest day
NBA_schedule = NBA_schedule[NBA_schedule["match_date_adl"].dt.date == NBA_schedule["match_date_adl"].dt.date.min()]

async def collect_h2h_and_urls(driver):
    """Navigate to main page, save H2H HTML, and return list of player URLs."""
    await driver.get('https://www.bet365.com.au/#/AC/B18/C20604387/D48/E1453/F10/')
    await driver.sleep(2)
    # Always perform login each run
    print("Attempting login...")
    login_element = await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')]", timeout=10)
    await login_element.click()
    await driver.sleep(1)

    username_field = await driver.find_element(By.XPATH, "//input[@placeholder='Username or email address']", timeout=10)
    await username_field.clear()
    await driver.sleep(0.3)
    await username_field.send_keys(username)
    print(f"Set username via send_keys: {username}")

    password_field = await driver.find_element(By.XPATH, "//input[@placeholder='Password']", timeout=10)
    await password_field.clear()
    await driver.sleep(0.3)
    await password_field.send_keys(password)
    print(f"Set password via send_keys: {password}")

    login_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'lms-LoginButton ')]", timeout=5)
    await login_button.click()
    print("Clicked login button")

    # Wait for market container after login
    elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'gl-MarketGroup_Wrapper')]", timeout=10)
    print("Market container found after login")

    # Save HTML    
    body_html = await elem.get_attribute('outerHTML')

    with open("OddsScraper/Bet365/HTML/h2h_html.txt", 'w') as f:
        f.write(body_html)

    # Find team rows to discover match URLs
    team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'scb-ParticipantFixtureDetailsHigherBasketball_TeamNames')]")

    for team_element in team_elements:
        try:
            print(await team_element.get_attribute('innerText'))
        except Exception:
            pass

    player_urls = []
    for index in range(len(team_elements)):
        # Re-find elements as DOM may refresh
        team_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'scb-ParticipantFixtureDetailsHigherBasketball_TeamNames')]")

        await driver.execute_script("arguments[0].scrollIntoView(true);", team_elements[index])
        await driver.execute_script("window.scrollBy(0, -150)")
        await driver.sleep(0.1)

        await team_elements[index].click()

        cur_url = await driver.current_url
        modified_player_url = cur_url + "I43/"
        player_urls.append(modified_player_url)

        await driver.back()

    # Optionally persist URL list for debugging/traceability
    try:
        with open("OddsScraper/Bet365/player_urls.csv", 'w') as f:
            f.write('\n'.join(player_urls))
    except Exception:
        pass

    return player_urls


async def scrape_player_pages(driver, player_urls):
    """Iterate player URLs, expand sections, and save player HTML per match."""
    for index, url in enumerate(player_urls, start=1):
        try:
            await driver.get(url)

            # Wait for a market group button to appear
            await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text ')]", timeout=5)
            print(f"Getting URL {url} which is match {index}")

            # Expand standard markets if present
            async def maybe_click(xpath_text, label):
                try:
                    el = await driver.find_element(By.XPATH, f"//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='{xpath_text}']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", el)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await el.click()
                    print(f"Clicked {label}")
                    await driver.sleep(2)
                except Exception:
                    print(f"No {label} button was found")
            
            await driver.sleep(2)
            await maybe_click('Assists O/U', 'Player Assists')
            await maybe_click('Assists', 'Player Assists Milestones')
            await maybe_click('Rebounds O/U', 'Player Rebounds')
            await maybe_click('Rebounds', 'Player Rebounds Milestones')
            await maybe_click('Threes Made O/U', 'Player Threes Made')
            await maybe_click('Threes Made', 'Player Threes Made Milestones')
            await maybe_click('Steals O/U', 'Player Steals')
            await maybe_click('Blocks O/U', 'Player Blocks')

            # Click all visible "Show more" buttons
            button_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'msl-ShowMore_Link ') and contains(text(), 'Show more')]")
            for button_element in button_elements:
                try:
                    await driver.execute_script("arguments[0].scrollIntoView(true);", button_element)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await button_element.click()
                    await driver.sleep(1)
                except Exception:
                    pass      

            # Grab and write the player page HTML for this match
            elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
            body_html_players = await elem.get_attribute('outerHTML')
            with open(f"OddsScraper/Bet365/HTML/body_html_players_match_{index}.txt", 'w') as f:
                f.write(body_html_players)

        except Exception as e:
            print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
            continue


async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        player_urls = await collect_h2h_and_urls(driver)
        await scrape_player_pages(driver, player_urls)


if __name__ == "__main__":
    asyncio.run(main())
