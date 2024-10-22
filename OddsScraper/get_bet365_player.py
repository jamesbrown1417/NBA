# Import Modules=============================================================
from selenium_driverless import webdriver
from selenium_driverless.types.by import By
from datetime import datetime

# Get current timestamp=======================================================
now = datetime.now()
time_stamp = now.strftime("%Y-%m-%d_%H-%M-%S")

# Read in CSV of URLs=========================================================
import pandas as pd
# Read csv (no header col)
url_df = pd.read_csv('Data/BET365_HTML/urls.csv', header=None)

# Convert first column to a list
url_df = url_df[0]

# Keep only first two URLs for now
# url_df = url_df[0:2]

# Get H2H HTML===============================================================
import asyncio

async def main():
    options = webdriver.ChromeOptions()
    # options.add_argument("--headless=True")
    
    async with webdriver.Chrome(options=options) as driver:
        for index, url in enumerate(url_df, start=1): # Start counting from 1 for match_n
            try:
                await driver.get(url)
                await driver.sleep(1)
                
                # If there is a button that says Player Points Low, click it
                try:
                    player_points_low_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Points Low']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_points_low_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_points_low_button.click()
                    await driver.sleep(1)
                except:
                    pass
                
                # If there is a button that says Player Points High, click it
                try:
                    player_points_high_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Points High']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_points_high_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_points_high_button.click()
                    await driver.sleep(1)
                except:
                    pass
                
                # If there is a button that says Assists O/U, click it
                try:
                    player_assists_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Assists O/U')]")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_assists_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_assists_button.click()
                    await driver.sleep(1)
                except:
                    pass
                
                # If there is a button that says Assists, click it
                try:
                    player_assists_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Assists')]")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_assists_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_assists_milestones_button.click()
                    await driver.sleep(1)
                except:
                    pass

               # If there is a button that says Rebounds O/U, click it
                try:
                    player_rebounds_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Rebounds O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_rebounds_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_rebounds_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Rebounds, click it
                try:
                    player_rebounds_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Rebounds']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_rebounds_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_rebounds_milestones_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Steals O/U, click it
                try:
                    player_steals_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Steals O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_steals_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_steals_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Steals, click it
                try:
                    player_steals_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Steals']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_steals_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_steals_milestones_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Blocks O/U, click it
                try:
                    player_blocks_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Blocks O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_blocks_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_blocks_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Blocks, click it
                try:
                    player_blocks_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Blocks']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_blocks_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_blocks_milestones_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Threes Made O/U, click it
                try:
                    player_threes_made_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Threes Made O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_threes_made_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_threes_made_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Threes Made, click it
                try:
                    player_threes_made_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Threes Made']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_threes_made_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_threes_made_milestones_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Points, Assists and Rebounds O/U, click it
                try:
                    player_pras_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Points, Assists & Rebounds O/U']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_pras_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_pras_button.click()
                    await driver.sleep(1)
                except:
                    pass

                # If there is a button that says Points, Assists and Rebounds, click it
                try:
                    player_pras_milestones_button = await driver.find_element(By.XPATH, "//div[contains(@class, 'cm-MarketGroupWithIconsButton_Text') and text()='Points, Assists & Rebounds']")
                    await driver.execute_script("arguments[0].scrollIntoView(true);", player_pras_milestones_button)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await player_pras_milestones_button.click()
                    await driver.sleep(1)
                except:
                    pass
                
                # Get all elements with class 'msl-ShowMore_Link ' that has text 'Show more'
                button_elements = await driver.find_elements(By.XPATH, "//div[contains(@class, 'msl-ShowMore_Link ') and contains(text(), 'Show more')]")
                
                print(len(button_elements))
                    
                # Scroll into view of each button, click it and wait 1 second
                for button_element in button_elements:
                    await driver.execute_script("arguments[0].scrollIntoView(true);", button_element)
                    await driver.execute_script("window.scrollBy(0, -150)")
                    await button_element.click()
                    await driver.sleep(1)
                    
                # Write out html to file------------------------------------------------
                # wait for elem to exist
                elem = await driver.find_element(By.XPATH, "//div[contains(@class, 'wcl-PageContainer_Colcontainer ')]")
                body_html_players = await elem.get_attribute('outerHTML')
                with open(f"Data/BET365_HTML/body_html_players_match_{index}.txt", 'w') as f:
                    f.write(body_html_players)
                        
            except Exception as e:
                print(f"An error occurred with URL {url}: {e}. Moving to the next URL.")
                continue  # Proceed to the next iteration of the loop

asyncio.run(main())                    