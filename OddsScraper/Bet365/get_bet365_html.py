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
    load_dotenv('env')

# Read credentials after loading
username = os.getenv('BET365USER')
password = os.getenv('BET365PW')

# Validate credentials early with a clear error
if not username or not password:
    raise RuntimeError(
        "Missing Bet365 credentials. Set BET365USER and BET365PW in .env or env, or export them in the environment."
    )

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
    await driver.get('https://www.bet365.com.au/#/AC/B18/C20604387/D48/E1453/F10/')
    await driver.sleep(2)

    # login
    login = await driver.find_element(By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')]")
    await login.click()
    await driver.sleep(1)

    user = await driver.find_element(By.XPATH, "//input[@placeholder='Username or email address']")
    await user.send_keys(username)
    pwd = await driver.find_element(By.XPATH, "//input[@placeholder='Password']")
    await pwd.send_keys(password)

    btn = await driver.find_element(By.XPATH, "//span[contains(@class, 'slm2-52')]")
    await btn.click()
    await driver.sleep(3)

    FIXTURE_XPATH = "//div[contains(@class,'scb-ParticipantFixtureDetailsHigherBasketball-wide') and not(contains(@class,'Hidden'))]"
    CLOCK_XPATH = ".//div[contains(@class,'pi-CouponParticipantClockInPlay_Extra') or contains(@class,'pi-CouponParticipantClockInPlay_GameTimerWrapper')]"
    TEAM_XPATH = ".//div[contains(@class,'scb-ParticipantFixtureDetailsHigherBasketball_TeamNames')]"

    all_games = await driver.find_elements(By.XPATH, FIXTURE_XPATH)
    pre_games = [g for g in all_games if not await g.find_elements(By.XPATH, CLOCK_XPATH)]

    player_urls = []
    for g in pre_games:
        team_block = await g.find_element(By.XPATH, TEAM_XPATH)
        await driver.execute_script("arguments[0].scrollIntoView({block:'center'});", team_block)
        await driver.sleep(0.2)
        await team_block.click()
        cur = await driver.current_url
        player_urls.append(cur.rstrip('/') + '/I43/')
        await driver.back()
        await driver.sleep(0.5)

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
