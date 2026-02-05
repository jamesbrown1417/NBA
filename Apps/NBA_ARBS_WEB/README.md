# NBA Arbs Web App (Local)

This is a local JavaScript dashboard that mirrors the main workflows from `Reports/nba_arbs.qmd`:
- Arbitrage calculator
- Top-Down tabs (+ Sportsbet-under toggle)
- Arbs tabs
- Middles table
- Multi Legs agency tables

## 1) Export data from RDS to JSON

From repo root:

```bash
Rscript Scripts/export_nba_arbs_web_data.R
```

This writes:

```text
Apps/NBA_ARBS_WEB/data/nba-arbs-data.json
```

## 2) Run the web app locally

From repo root:

```bash
python3 -m http.server 4173 --directory Apps/NBA_ARBS_WEB
```

Then open:

```text
http://localhost:4173
```

## Notes

- The app is dependency-free (no npm install required).
- Data file is external so you can refresh it whenever odds/arbs update.
