import requests
import json

# Define the URL for the GET request (for NBA odds)
tab_url = "https://api.beta.tab.com.au/v1/tab-info-service/sports/Basketball/competitions/NBA?jurisdiction=SA&numTopMarkets=5"

# Set the headers required for the request (simulating a browser request)
headers = {
    "accept": "application/json, text/plain, */*",
    "accept-language": "en-US,en;q=0.9",
    "origin": "https://www.tab.com.au",
    "referer": "https://www.tab.com.au/",
    "sec-ch-ua": '"Not/A)Brand";v="8", "Chromium";v="129", "Google Chrome";v="129"',
    "sec-ch-ua-mobile": "?0",
    "sec-ch-ua-platform": '"Windows"',
    "sec-fetch-dest": "empty",
    "sec-fetch-mode": "cors",
    "sec-fetch-site": "same-site",
    "user-agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36"
}

# Attempt the GET request with a 10-second timeout. If the request fails, set response to None.
try:
    response = requests.get(tab_url, headers=headers, timeout=10)
except requests.exceptions.RequestException:
    response = None

# Check if the response is valid and the request was successful (status code 200).
if response is not None and response.status_code == 200:
    try:
        # Parse the response body as JSON and store it in tab_response.
        tab_response = response.json()
    except ValueError:
        # Handle the case where the response is not a valid JSON.
        tab_response = None
else:
    # If the request failed or the response was invalid, set tab_response to None.
    tab_response = None

# If the response was successfully parsed, write the JSON data to a file.
if tab_response is not None:
    # Specify the path and file name where the JSON data will be saved.
    with open("OddsScraper/TAB/tab_response.json", "w") as json_file:
        # Write the JSON data to the file with pretty formatting (indentation).
        json.dump(tab_response, json_file, indent=4)
        print("Succesfully Saved TAB Response!")
else:
    # Print a message if there's no data to write.
    print("No data to write to file.")
