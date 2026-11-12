# Changelog — Let's Follow the Steelhead

All notable changes to this app. Newest first. Semantic versioning (MAJOR.MINOR.PATCH).

## 0.4.0 — 2026-11-12
Sidebar **Filters / Settings** now a rounded two-column segmented pill. **Theme picker** shows clickable
palette cards (gradient header + colour strip + name) instead of a dropdown, selected one outlined in
coral. Top-right **live UTC clock** with a hover **world-clock popover** (Vancouver, Kuala Lumpur,
London, Sydney, Johannesburg — one per continent). Swimming fish now **tint to the active theme** and
trail **rising bubbles**.

## 0.3.0 — 2026-11-12
Left **sidebar** added across the whole app. **Filters** (release location / stream, mark year,
release year) now subset every tab, with a live "N of 3,000 fish selected" count and a Reset button.
**Settings** holds 8 colour themes (Classic, Light, Dark, Midnight, Graphite, Forest, Sand, High
Contrast) and a 10-font interface picker with a live "quick brown fox" preview. New dependency:
`shinyjs`; Google Fonts loaded for the picker.

## 0.2.0 — 2026-11-12
Rebrand to **Where's O Mykiss** with an animated, alternating rainbow-trout / steelhead that swims
across the title. New "River current" theme (blue→teal gradient navbar, colorful value boxes). Added
a **Beta** badge, a **Built 2026-11-12 UTC** stamp and **Built by Macus Ong** in the top-right.
New `App_Dev/www/styles.css`.

## 0.1.0 — 2026-11-12
Initial release. Interactive Shiny app for Okanagan steelhead (*O. mykiss*) PIT detections,
2019+ cohort, built from the event-aware analysis (Mark / Observation / Recapture / Recovery events).
Tabs: Overview (fate of the cohort), Tributary timing, Presence, Outmigration & predation,
Fish explorer, Data, Guide. Confirmed-return fish `3DD.003D53EC37` featured in Fish explorer.
