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

# Player prop categories on the match page and the market groups to expand.
PROP_CATEGORIES = {
    'I43': {
        'name': 'Points',
        'tab': 'Points',
        'buttons': ['Points O/U', 'Points High', 'Points Low']
    },
    'I45': {
        'name': 'Threes',
        'tab': 'Threes',
        'buttons': ['Threes Made O/U']
    },
    'I46': {
        'name': 'Assists',
        'tab': 'Assists',
        'buttons': ['Assists O/U']
    },
    'I47': {
        'name': 'Rebounds',
        'tab': 'Rebounds',
        'buttons': ['Rebounds O/U']
    },
    'I48': {
        'name': 'Combos',
        'tab': 'Combos',
        'buttons': ['Double Double', 'Triple Double']
    },
    'I49': {
        'name': 'Defence',
        'tab': 'Defence',
        'buttons': ['Steals O/U', 'Blocks O/U']
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

# Remove games that have finished (started more than 3 hours ago)
NBA_schedule = NBA_schedule[NBA_schedule["match_date_adl"] > now - pd.Timedelta(hours=3)]

# Get number of games that have started
started = NBA_schedule[NBA_schedule["match_date_adl"] < now]

# Create range from len started to len schedule
match_range = range(len(started), len(NBA_schedule))

# Lower-case projection for case-insensitive XPath text matching.
XPATH_LOWER_TEXT = (
    "translate(normalize-space(string(.)), "
    "'ABCDEFGHIJKLMNOPQRSTUVWXYZ', "
    "'abcdefghijklmnopqrstuvwxyz')"
)


async def find_first_element(driver, locator_candidates, timeout_per_candidate=3):
    """Try locators in order and return the first element that can be found."""
    last_error = None
    for by, value in locator_candidates:
        try:
            return await driver.find_element(by, value, timeout=timeout_per_candidate)
        except Exception as exc:
            last_error = exc
    if last_error:
        raise last_error
    raise RuntimeError("No locator candidates provided")


async def click_element(driver, element):
    """Scroll to an element and click it with a JS fallback."""
    try:
        await driver.execute_script(
            "arguments[0].scrollIntoView({block: 'center'});", element
        )
        await driver.execute_script("window.scrollBy(0, -150)")
    except Exception:
        pass

    try:
        await element.click()
    except Exception:
        await driver.execute_script("arguments[0].click();", element)


async def collect_h2h_and_urls(driver):
    """Navigate to main page, save H2H HTML, and return match page URLs."""
    await driver.get('https://www.bet365.com.au/#/AC/B18/C20604387/D48/E1453/F10/')
    await driver.sleep(2)
    # Always perform login each run
    print("Attempting login...")
    login_locator_candidates = [
        # Most stable header container when logged out.
        (By.XPATH, "//div[contains(@class, 'hm-MainHeaderRHSLoggedOutWide_Login')]"),
        # Dynamic hrm-* class token, matched by prefix and label text.
        (
            By.XPATH,
            f"//span[contains(@class, 'hrm-') and (contains({XPATH_LOWER_TEXT}, 'log in') or contains({XPATH_LOWER_TEXT}, 'login'))]",
        ),
        # Generic clickable fallback based on visible label.
        (
            By.XPATH,
            f"//*[self::button or self::a][contains({XPATH_LOWER_TEXT}, 'log in') or contains({XPATH_LOWER_TEXT}, 'login')]",
        ),
    ]
    login_element = await find_first_element(
        driver, login_locator_candidates, timeout_per_candidate=4
    )
    await driver.sleep(2)
    try:
        await login_element.click()
    except Exception:
        await driver.execute_script("arguments[0].click();", login_element)
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

    login_submit_locator_candidates = [
        (
            By.XPATH,
            f"//input[@placeholder='Password']/ancestor::form//*[self::button or self::span][contains({XPATH_LOWER_TEXT}, 'log in') or contains({XPATH_LOWER_TEXT}, 'login')]",
        ),
        (By.XPATH, "//span[starts-with(@class, 'slm')]"),
    ]
    login_button = await find_first_element(
        driver, login_submit_locator_candidates, timeout_per_candidate=3
    )
    try:
        await login_button.click()
    except Exception:
        await driver.execute_script("arguments[0].click();", login_button)
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

    # Persist match page URLs for debugging/traceability.
    try:
        pd.DataFrame(match_data).to_csv(
            "OddsScraper/Bet365/player_urls.csv", index=False
        )
    except Exception:
        pass

    return match_data


async def scrape_player_pages(driver, match_data):
    """Iterate matches, click category tabs, expand markets, and save HTML."""

    async def click_market_tab(tab_label, expected_labels):
        """Open a match-page category tab and wait for its markets to render."""
        expected_xpath = " or ".join(
            [f"normalize-space()='{label}'" for label in expected_labels]
        )
        tab_element = await driver.find_element(
            By.XPATH,
            (
                "//div[contains(@class, 'sph-MarketGroupNavBarButton_Content') "
                f"and normalize-space()='{tab_label}']"
            ),
            timeout=10,
        )
        await click_element(driver, tab_element)
        await driver.find_element(
            By.XPATH,
            (
                "//div["
                "(contains(@class, 'cm-MarketGroupWithIconsButton_Text') "
                "or contains(@class, 'sc-MarketGroupButtonWithStats_Text')) "
                f"and ({expected_xpath})]"
            ),
            timeout=10,
        )
        print(f"  Opened '{tab_label}' tab")

    async def maybe_expand_market(xpath_text, label):
        """Expand a market group by its text if it is currently collapsed."""
        try:
            text_element = await driver.find_element(
                By.XPATH,
                (
                    "//div["
                    "(contains(@class, 'cm-MarketGroupWithIconsButton_Text') "
                    "or contains(@class, 'sc-MarketGroupButtonWithStats_Text')) "
                    f"and normalize-space()='{xpath_text}']"
                ),
                timeout=3,
            )
            toggle_button = await text_element.find_element(
                By.XPATH,
                (
                    "./ancestor::div["
                    "contains(@class, 'cm-MarketGroupWithIconsButton') "
                    "or contains(@class, 'sc-MarketGroupButtonWithStats')"
                    "][1]"
                ),
            )
            classes = await toggle_button.get_attribute('class') or ''
            if 'gl-MarketGroup_Open' in classes:
                print(f"  '{label}' already expanded")
                return

            await click_element(driver, toggle_button)
            print(f"  Clicked '{label}'")
            await driver.sleep(1.5)
        except Exception:
            print(f"  No '{label}' button found")

    async def click_show_more_buttons():
        """Click all visible 'Show more' buttons on the page."""
        for _ in range(10):
            button_elements = await driver.find_elements(
                By.XPATH,
                (
                    "//div[contains(@class, 'msl-ShowMore_Link') "
                    "and contains(normalize-space(), 'Show more')]"
                ),
            )

            clicked = False
            for button_element in button_elements:
                try:
                    await click_element(driver, button_element)
                    await driver.sleep(1)
                    clicked = True
                    break
                except Exception:
                    continue

            if not clicked:
                break

    for match in match_data:
        match_index = match['match_index']
        base_url = match['base_url']
        
        print(f"\n{'='*60}")
        print(f"Processing match {match_index}")
        print(f"{'='*60}")
        
        for _, category_info in PROP_CATEGORIES.items():
            category_name = category_info['name']
            tab_label = category_info['tab']
            buttons_to_click = category_info['buttons']
            
            try:
                print(f"\n--- {category_name} ---")
                print(f"Match URL: {base_url}")
                
                await driver.get(base_url)
                await driver.find_element(
                    By.XPATH,
                    "//div[contains(@class, 'sph-MarketGroupNavBarButton_Content')]",
                    timeout=10,
                )
                await driver.sleep(1.5)

                # Open the correct category tab on the match page.
                await click_market_tab(tab_label, buttons_to_click)
                await driver.sleep(1)
                
                # Click only the buttons relevant to this category
                for button_text in buttons_to_click:
                    await maybe_expand_market(button_text, button_text)
                
                # Click all "Show more" buttons
                await click_show_more_buttons()

                # Grab and write the player page HTML for this match/category
                elem = await driver.find_element(
                    By.XPATH,
                    "//div[contains(@class, 'wcl-PageContainer_Colcontainer')]",
                    timeout=10,
                )
                body_html_players = await elem.get_attribute('outerHTML')
                
                filename = f"OddsScraper/Bet365/HTML/body_html_players_match_{match_index}_{category_name.lower()}.txt"
                with open(filename, 'w') as f:
                    f.write(body_html_players)
                print(f"  Saved: {filename}")

            except Exception as e:
                print(
                    f"  Error with {category_name}: {type(e).__name__}: {e!r}. Continuing..."
                )
                continue


async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")

    async with webdriver.Chrome(options=options) as driver:
        match_data = await collect_h2h_and_urls(driver)
        await scrape_player_pages(driver, match_data)


if __name__ == "__main__":
    asyncio.run(main())
