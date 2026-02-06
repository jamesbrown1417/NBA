# NBA Arbs Web App (Vite + React + Tailwind)

Modern local dashboard that mirrors `Reports/nba_arbs.qmd` workflows:
- Arbitrage calculator
- Top-Down tabs + Sportsbet-under toggle
- Arbs tabs
- Middles
- Multi Legs
- Dataset freshness sidebar
- Margin rarity row shading

## Stack

- Vite
- React + TypeScript
- Tailwind CSS

## Data pipeline

Generate JSON from your `.rds` inputs:

```bash
Rscript Scripts/export_nba_arbs_web_data.R
```

Output path:

```text
Apps/NBA_ARBS_WEB/public/data/nba-arbs-data.json
```

## Local development

From repo root:

```bash
cd Apps/NBA_ARBS_WEB
npm install
npm run dev
```

Open:

```text
http://localhost:5173
```

Notes:
- In this dev mode, in-app refresh is usually disabled (no `/api/refresh` route from Vite dev server).

## Local production-style run (with in-app refresh)

From repo root:

```bash
cd Apps/NBA_ARBS_WEB
npm install
npm run build
npm run serve
```

Open:

```text
http://localhost:4173
```

This mode serves `dist/` and enables:
- `POST /api/refresh` (runs `Rscript Scripts/export_nba_arbs_web_data.R`)
- live JSON reads from `public/data/nba-arbs-data.json`

## Static Netlify deploy

From repo root:

```bash
Rscript Scripts/export_nba_arbs_web_data.R
cd Apps/NBA_ARBS_WEB
npm install
npm run build
```

Deploy `Apps/NBA_ARBS_WEB/dist`.

Notes:
- Static Netlify deploy works for dashboard viewing.
- In-app refresh button will be disabled in static hosting.

## Margin rarity colors

- `> 0 and < 1.0`: grey (`#A6A8AD`)
- `>= 1.0 and < 2.5`: green (`#5FCB5A`)
- `>= 2.5 and < 5.0`: blue (`#4D8FFF`)
- `>= 5.0`: orange (`#FF9A2E`)

## Dataset freshness panel

Shows source file `mtime`, shape, and source path for:
- `all_arbs.rds`
- `all_middles.rds`
- `tab_points_miss_by_one.rds`
- `betright_points_miss_by_one.rds`
- `processed_odds/*.rds`
