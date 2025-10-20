import requests
import json

# Define the URL for the GET request (for NBA odds)
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/Basketball/competitions/NBA?jurisdiction=SA"


from selenium_driverless import webdriver
import asyncio
import json
import os

OUTPUT_PATH = "/Users/jamesbrown/Projects/NBA/OddsScraper/TAB/tab_response.json"

async def main():
    options = webdriver.ChromeOptions()
    options.add_argument("--disable-blink-features=AutomationControlled")

    
    async with webdriver.Chrome(options=options) as driver:
        await driver.minimize_window()
        # First establish session on main site
        await driver.get("https://www.tab.com.au")
        await driver.sleep(3)
        
        # Now fetch the API directly through the browser
        api_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/Basketball/competitions/NBA?jurisdiction=SA"
        await driver.get(api_url)
        await driver.sleep(2)
        
        # Get the JSON response
        page_content = await driver.page_source
        
        # Parse JSON from the page
        try:
            # Browser displays JSON as text in <pre> tags
            if "<pre" in page_content:
                import re
                json_match = re.search(r'<pre[^>]*>(.+?)</pre>', page_content, re.DOTALL)
                if json_match:
                    json_str = json_match.group(1)
                else:
                    json_str = page_content
            else:
                json_str = page_content
            
            data = json.loads(json_str)
            
            # Create directory if it doesn't exist
            os.makedirs(os.path.dirname(OUTPUT_PATH), exist_ok=True)
            
            # Save to the specified path
            with open(OUTPUT_PATH, "w", encoding='utf-8') as f:
                json.dump(data, f, indent=2)
            print(f"[SUCCESS] Saved API data to {OUTPUT_PATH}")
            
            # Quick summary of what was saved
            if "matches" in data:
                print(f"[INFO] Saved {len(data.get('matches', []))} matches")
            
        except json.JSONDecodeError:
            print("[ERROR] Could not parse JSON from response")
            # Save debug file in same directory
            debug_path = OUTPUT_PATH.replace('.json', '_debug.html')
            with open(debug_path, "w", encoding='utf-8') as f:
                f.write(page_content)
            print(f"[DEBUG] Saved raw response to {debug_path}")

asyncio.run(main())
