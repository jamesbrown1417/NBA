from nba_api.stats.static import teams
from nba_api.stats.endpoints import commonteamroster
import pandas as pd
import time
import sys
from requests.exceptions import ReadTimeout, ConnectionError, RequestException
from urllib3.exceptions import ReadTimeoutError

#===============================================================================
# Get list of all teams
#===============================================================================

USE_COLOR = sys.stdout.isatty()
RESET = "\033[0m"
BLUE = "\033[94m"
CYAN = "\033[96m"
GREEN = "\033[92m"
YELLOW = "\033[93m"
RED = "\033[91m"
MAGENTA = "\033[95m"


def style(text, color=""):
    if USE_COLOR and color:
        return f"{color}{text}{RESET}"
    return text


def log_status(message, emoji="ℹ️", color=CYAN):
    print(f"{style(emoji, color)} {style(message, color)}")


def progress_bar(current, total, width=24):
    if total <= 0:
        return "░" * width
    filled = int(width * current / total)
    return "█" * filled + "░" * (width - filled)

all_teams = teams.get_teams()
df_teams = pd.DataFrame(all_teams)

#===============================================================================
# Retry helper for roster fetches
#===============================================================================

def fetch_team_roster(team_id, retries=5, timeout=120, delay=3):
    for attempt in range(1, retries + 1):
        try:
            roster = commonteamroster.CommonTeamRoster(team_id=team_id, timeout=timeout)
            return roster.get_data_frames()[0]
        except (ReadTimeout, ReadTimeoutError, ConnectionError, RequestException) as err:
            if attempt == retries:
                log_status(
                    (
                        f"Failed to fetch roster for team_id={team_id} "
                        f"after {retries} attempts: {err}"
                    ),
                    emoji="❌",
                    color=RED,
                )
                return None

            wait_time = delay * (2 ** (attempt - 1))
            log_status(
                (
                    f"Roster request failed for team_id={team_id} "
                    f"(attempt {attempt}/{retries}): {err}. "
                    f"Retrying in {wait_time}s."
                ),
                emoji="⏳",
                color=YELLOW,
            )
            time.sleep(wait_time)

#===============================================================================
# Get Current Rosters
#===============================================================================

all_rosters = []
failed_team_ids = []
team_info = df_teams.drop_duplicates(subset=["id"])[["id", "full_name"]]
total_teams = len(team_info)

log_status(
    f"Starting roster pull for {total_teams} teams...",
    emoji="🚀",
    color=MAGENTA,
)

for i, team in enumerate(team_info.itertuples(index=False), start=1):
    team_id = team.id
    team_name = team.full_name
    pct_before = ((i - 1) / total_teams) * 100
    log_status(
        f"{progress_bar(i - 1, total_teams)} {pct_before:6.2f}% | Team {i}/{total_teams} | {team_name} ({team_id})",
        emoji="🏀",
        color=BLUE,
    )

    roster_df = fetch_team_roster(team_id=team_id)

    pct_after = (i / total_teams) * 100
    if roster_df is not None:
        all_rosters.append(roster_df)
        log_status(
            f"{progress_bar(i, total_teams)} {pct_after:6.2f}% | Completed {team_name} ({team_id})",
            emoji="✅",
            color=GREEN,
        )
    else:
        failed_team_ids.append(team_id)
        log_status(
            f"{progress_bar(i, total_teams)} {pct_after:6.2f}% | Missing {team_name} ({team_id})",
            emoji="⚠️",
            color=YELLOW,
        )

    # Small pause helps avoid hammering stats.nba.com
    time.sleep(0.5)

if all_rosters:
    df_rosters = pd.concat(all_rosters, ignore_index=True)
else:
    df_rosters = pd.DataFrame()
    log_status("No roster data was fetched.", emoji="⚠️", color=YELLOW)

#===============================================================================
# Write out CSVs
#===============================================================================

# Write out all teams
df_teams.to_csv('Data/all_teams.csv', index=False)
log_status("Saved Data/all_teams.csv", emoji="💾", color=CYAN)

# Write out all rosters
df_rosters.to_csv('Data/all_rosters.csv', index=False)
log_status("Saved Data/all_rosters.csv", emoji="💾", color=CYAN)

if failed_team_ids:
    log_status(f"Completed with missing rosters for team IDs: {failed_team_ids}", emoji="⚠️", color=YELLOW)

success_count = total_teams - len(failed_team_ids)
success_pct = (success_count / total_teams) * 100 if total_teams else 0
log_status(
    f"Finished: {success_count}/{total_teams} teams succeeded ({success_pct:.2f}%).",
    emoji="🎯",
    color=GREEN if not failed_team_ids else CYAN,
)
