# Changelog — Let's Follow the Steelhead

All notable changes to this app. Newest first. Semantic versioning (MAJOR.MINOR.PATCH).

## 0.7.1 — 2026-11-13
Theme refresh: the duplicate **Light** and **Midnight** themes are replaced by **Viridis** (soft mint and
teal, light) and **Magma** (deep plum with coral and apricot accents, dark). Charts, tables, map and
navbar follow each new theme.

## 0.7.0 — 2026-11-13
Three new tabs. **Cohorts**: fate of each release-year cohort (percent or count) with a summary table.
**Corridor map**: detection sites from the Okanagan to the Columbia estuary, sized by fish count, on a map
that follows the theme (approximate coordinates). **Estuary predation**: journey of each fish recovered on a
bird colony, recoveries by colony, and how to read recovery dates. Exported plots now also carry the active
filters in the footer, above the version and Generated line. Includes the 0.6.1 export fixes.

## 0.6.1 — 2026-11-13
Export fixes. The version/Generated footer is now positioned by a fixed pixel offset below the plot, so it
appears on **every** plot (tall plots such as the presence heat map previously pushed it out of view). Axes
auto-size their margins so long site names and titles are no longer cut off, and legends moved above the
plot. PNG export is now 2x resolution.

## 0.6.0 — 2026-11-13
New **Stories** tab: two hero cards — *The one that came home* (the single confirmed natal return) and
*One that didn't make it* (an East Sand Island predation loss) — each with a **Trace this fish in Fish
explorer** button that jumps to its timeline; plus an auto-rotating **notable-fish** showcase. **Dark
themes now theme the charts** (dark panels, light text/gridlines) and the data tables, so nothing
renders as a bright white box. (Includes the v0.5.1 in-figure export stamp.)

## 0.5.1 — 2026-11-13
Every plot now carries an in-figure stamp (`Where's O Mykiss vX.Y.Z · Generated YYYY-MM-DD HH:MM:SS UTC`)
so the version and generation time ride along in the PNG you export from the plot toolbar — not just the
on-screen footer.

## 0.5.0 — 2026-11-12
Top-right clock now shows **date and time** (YYYY-MM-DD HH:MM:SS UTC). Added a **provenance footer**
under the plots on the Overview and Outmigration tabs: active filters, app version, and a
"Generated YYYY-MM-DD HH:MM:SS UTC" stamp. **Outmigration & predation** tab gained an animated
**seabird banner** (flapping gulls + a diving tern snatching a smolt) and a plain-language
**"What do these numbers mean?"** card that turns the current selection into a sentence plus a simple
proportion bar (eaten by birds / never seen again / returned home).

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
