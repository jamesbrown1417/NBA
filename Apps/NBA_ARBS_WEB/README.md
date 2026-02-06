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
node Apps/NBA_ARBS_WEB/server.js
```

Then open:

```text
http://localhost:4173
```

## Notes

- The app is dependency-free (no npm install required).
- Data file is external so you can refresh it whenever odds/arbs update.
- The in-app `Refresh Data` button works only when running via `server.js` (it calls `Rscript Scripts/export_nba_arbs_web_data.R`).
- If you use `python3 -m http.server`, the app is read-only and cannot run refresh from inside the browser.
- The sidebar shows per-dataset freshness using each source `.rds` file's last-modified timestamp (`mtime`), plus row/column counts.
