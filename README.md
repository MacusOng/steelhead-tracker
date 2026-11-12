# Let's Follow the Steelhead

Interactive R/Shiny dashboard for Okanagan steelhead (*Oncorhynchus mykiss*) PIT-tag detections.
Sibling of **Let's Follow the Sockeye** (`MacusOng/sockeye-tracker`); same CSS FTT workflow and
conventions. Author: Macus Ong, Okanagan Nation Alliance.

Cohort = fish released **2019 onward** (3,000 fish). Source: PTAGIS complete tag history, all event
types (Mark / Observation / Recapture / Recovery), 19 Aug 2026.

## Where things live

- Code (public): `https://github.com/MacusOng/steelhead-tracker`
  (root: `.gitignore`, `README.md`, `VERSION`; `App_Dev/`: `app.R`, `CHANGELOG.md`,
  `Steelhead_FTT_Dashboard.Rproj`, `data/`, `tools/check_version.R`, `tools/deploy_shinyapps.R`)
- Full version log: `App_Dev/CHANGELOG.md` (newest first)
- Live website (after first deploy): `https://macus-ong.shinyapps.io/steelhead-tracker/`
- Data: the six derived CSVs in `App_Dev/data/` are open/derived data and **are** committed (small,
  and the app needs them). This differs from sockeye-tracker, which keeps data out of git — switch to
  the `data_public/` pattern later if you want parity.

## Conventions (same as the sockeye dashboard)

- Semantic versioning; `VERSION` + `App_Dev/CHANGELOG.md` move together; each change is a version bump.
- Full site names only in the UI — no station codes.
- Dates shown as recorded (UTC).
- A mandatory **Guide** tab walks a new user through every page.

## Update routine (PowerShell for git, RStudio for R)

1. **PowerShell:** `cd "<project root>"`, then `git status` must be clean.
2. **Apply changes:** paste the "updater" R script Claude gives you into the **RStudio console** — it
   edits `App_Dev/app.R`, bumps `VERSION` and `App_Dev/CHANGELOG.md`, and prints `Done`.
   (Downloads from chat don't reliably reach your machine, and `Rscript` isn't on PATH in PowerShell.)
3. **Test:** RStudio console `shiny::runApp("App_Dev")`.
4. **PowerShell:** `git add App_Dev/app.R App_Dev/CHANGELOG.md VERSION`,
   `git commit -m "vX.Y.Z ..."`, `git push`.
5. **Deploy:** RStudio console `setwd("<project root>"); source("App_Dev/tools/deploy_shinyapps.R")`
   (type `y`).

## Run it locally

Needs **R ≥ 4.1**. In RStudio:

```r
install.packages(c("shiny","bslib","bsicons","dplyr","tidyr","readr",
                   "ggplot2","plotly","DT","scales","viridisLite"))
shiny::runApp("App_Dev")
```

## Roadmap

- 0.2.0 — Upload & auto-refresh (drop a new PTAGIS all-event export; app re-runs the analysis live)
- 0.3.0 — Cohort tracking by release year (tagged → outmigrated → recovered → returned, per year)
- 0.4.0 — Corridor map (detection sites + the confirmed-return path), PTAGIS lat/longs
- 0.5.0 — Estuary predation deep-dive panel

## Caveats

Detections show presence and movement only. Steelhead vs rainbow (same species) and repeat spawning
need scale ageing / microchemistry / genetics. "Confirmed return" is a floor.
