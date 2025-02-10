import os
import asyncio
import random
from selenium_driverless import webdriver
from selenium_driverless.types.by import By

async def main():
    # Retrieve credentials from environment variables (or use placeholder values)
    username = os.environ.get("NEDS_USERNAME", "your_username")
    password = os.environ.get("NEDS_PASSWORD", "your_password")
    
    # Define a realistic user agent string
    realistic_ua = (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/115.0.0.0 Safari/537.36"
    )
    
    # Set up Chrome options with a realistic user agent and automation flag overrides
    options = webdriver.ChromeOptions()
    options.add_argument(f'--user-agent={realistic_ua}')
    
    async with webdriver.Chrome(options=options) as driver:
        # Inject a stealth script as early as possible to mask automation indicators.
        stealth_script = """
            Object.defineProperty(navigator, 'webdriver', { get: () => false });
            Object.defineProperty(navigator, 'plugins', { get: () => [1, 2, 3, 4, 5] });
            Object.defineProperty(navigator, 'languages', { get: () => ['en-US', 'en'] });
            if (window.navigator.__proto__.hasOwnProperty('webdriver')) {
                delete window.navigator.__proto__.webdriver;
            }
        """
        await driver.execute_script(stealth_script)
        
        # Navigate to the basketball page.
        basketball_url = "https://www.neds.com.au/sports/basketball/usa/nba"
        print("Navigating to the basketball page...")
        await driver.get(basketball_url, wait_load=True)
        await driver.sleep(2)
        
        # Simulate human-like scrolling to mimic natural user interaction.
        await driver.execute_script("window.scrollBy(0, 150);")
        await driver.sleep(random.uniform(0.5, 1))
        await driver.execute_script("window.scrollBy(0, -150);")
        await driver.sleep(random.uniform(0.5, 1))
        
        # Find and click the header login button.
        print("Clicking the login button...")
        login_button = await driver.find_element(By.CSS_SELECTOR, 'button[data-testid="header-login"]', timeout=15)
        await login_button.click(move_to=True)
        await driver.sleep(2)
        
        # Wait for the login form to load (username input should be present).
        print("Waiting for login form to load...")
        username_input = await driver.find_element(By.CSS_SELECTOR, 'input#username', timeout=15)
        
        # Simulate human-like typing for the username.
        print("Typing username...")
        for char in username:
            await username_input.send_keys(char)
            await driver.sleep(random.uniform(0.1, 0.2))
        
        # Locate the password field and type the password.
        print("Typing password...")
        password_input = await driver.find_element(By.CSS_SELECTOR, 'input#password', timeout=15)
        for char in password:
            await password_input.send_keys(char)
            await driver.sleep(random.uniform(0.1, 0.2))
        
        # Short pause before submitting.
        await driver.sleep(1)
        
        # Locate and click the submit button (with id="accept").
        print("Submitting the login form...")
        login_submit = await driver.find_element(By.CSS_SELECTOR, 'button#accept', timeout=15)
        await login_submit.click(move_to=True)
        
        # Wait a bit to observe the result or handle any additional challenge manually.
        await driver.sleep(10)

asyncio.run(main())