from playwright.async_api import async_playwright
import asyncio
import pandas as pd
import json
import os

# Load the URL list from the CSV file using platform-agnostic paths
csv_path = os.path.join("OddsScraper", "Neds", "neds_nba_match_urls.csv")
match_urls = pd.read_csv(csv_path)

# Get the URLs as a list
urls = match_urls["url"].tolist()

async def main():
    # Reload URLs from the CSV file
    csv_path = os.path.join("OddsScraper", "Neds", "neds_nba_match_urls.csv")
    match_urls = pd.read_csv(csv_path)
    urls = match_urls["url"].tolist()

    # Ensure the output directory exists
    output_dir = os.path.join("OddsScraper", "Neds")
    os.makedirs(output_dir, exist_ok=True)

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True)
        page = await browser.new_page()

        # Counter to generate unique file names
        file_counter = 1

        # Listen to responses instead of requests
        async def handle_response(response):
            nonlocal file_counter
            request = response.request
            if 'card' in request.url:
                try:
                    json_body = await response.json()
                    file_path = os.path.join(output_dir, f"data_{file_counter}.json")
                    with open(file_path, 'w', encoding='utf-8') as f:
                        json.dump(json_body, f, ensure_ascii=False, indent=4)
                    print(f"Saved JSON to {file_path}")
                    file_counter += 1
                except Exception as e:
                    print(f"Error reading JSON from {request.url}: {e}")

        # Register the response handler
        page.on('response', handle_response)

        # Process each URL
        for url in urls:
            await page.goto(url)
            # Add any necessary waits here to ensure the page loads completely

        await browser.close()

asyncio.run(main())
