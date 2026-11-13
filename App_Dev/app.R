library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(viridisLite)

# =====================================================================
# Where's O Mykiss  —  Okanagan O. mykiss PIT detection explorer
# Part of the CSS FTT dashboard family (sibling of sockeye-tracker).
# Data: PTAGIS complete tag history, all event types, 19 Aug 2026.
# Cohort = fish released 2019+. Dates are as recorded (UTC).
# =====================================================================
APP_VERSION <- "0.7.1"
BUILT_UTC   <- "2026-11-12"

# ---- load data ----
pf   <- read_csv("data/per_fish.csv",         show_col_types = FALSE)
ev   <- read_csv("data/events_clean.csv",     show_col_types = FALSE) |>
          mutate(event_date = as.Date(event_date),
                 doy = as.integer(format(event_date, "%j")))
fs   <- read_csv("data/fate_summary.csv",     show_col_types = FALSE)
rec  <- read_csv("data/recoveries.csv",       show_col_types = FALSE)

# mark year per fish (tag event); here equals release year, but kept as its own filter
mark_year <- ev |> filter(event_type == "Mark") |>
  group_by(tag_code) |> summarise(mark_year = min(event_year), .groups = "drop")
pf <- pf |> left_join(mark_year, by = "tag_code") |>
  mutate(mark_year = ifelse(is.na(mark_year), release_year, mark_year))

# ---- lookups ----
pal <- list(navy="#1F3B57", blue="#2a78d6", orange="#eb6834", aqua="#1baf7a",
            red="#9E2B25", green="#1E6B3A", grey="#9aa3a8")
creek_codes <- c("OKI","OKV","OKS","OKW")
creek_names <- c(OKI="Inkaneep Creek", OKV="Vaseux Creek",
                 OKS="Shingle Creek", OKW="Shuttleworth Creek")
down_sites  <- c("RRJ","BCC","B2J","MCJ","JDJ","PD5","TWX")
fate_cols <- c(
  "confirmed return (adult, natal)"     = pal$green,
  "recovered - predation island"        = pal$red,
  "recovered - in-basin"                = "#b5651d",
  "left basin / outmigrant (no return)" = pal$orange,
  "in-basin only (no outmigration)"     = pal$blue,
  "marked only (never re-detected)"     = pal$grey)
fate_order <- fs$fate
fate_def   <- setNames(fs$definition, fs$fate)

locs   <- sort(unique(pf$release_site))
ry_min <- min(pf$release_year, na.rm = TRUE); ry_max <- max(pf$release_year, na.rm = TRUE)
my_min <- min(pf$mark_year,   na.rm = TRUE);  my_max <- max(pf$mark_year,   na.rm = TRUE)

# plot background/foreground per theme, so charts match (no white boxes on dark themes)
plot_theme_cols <- list(
  classic  = list(bg="#ffffff", fg="#0e2d49", grid="#e4e9ee"),
  viridis  = list(bg="#f3f9f6", fg="#1c3b44", grid="#d3e6dd"),
  dark     = list(bg="#15181c", fg="#e6edf3", grid="#333a42"),
  magma    = list(bg="#140b1f", fg="#f3e6ef", grid="#3a2150"),
  graphite = list(bg="#1b1b1d", fg="#e4e4e6", grid="#34343a"),
  forest   = list(bg="#0f1a12", fg="#d7e6d8", grid="#24352a"),
  sand     = list(bg="#f5efe2", fg="#3a2f1e", grid="#e0d6c0"),
  highcon  = list(bg="#000000", fg="#ffffff", grid="#333300"))

theme_okn <- function(col = list(bg="#ffffff", fg="#222", grid="#e6e6e6")) {
  theme_minimal(base_size = 13) +
    theme(plot.background   = element_rect(fill = col$bg, color = NA),
          panel.background  = element_rect(fill = col$bg, color = NA),
          panel.grid.major  = element_line(color = col$grid),
          panel.grid.minor  = element_blank(),
          text              = element_text(color = col$fg),
          axis.text         = element_text(color = col$fg),
          plot.title        = element_text(face = "bold", color = col$fg),
          legend.background = element_rect(fill = col$bg, color = NA),
          legend.key        = element_rect(fill = col$bg, color = NA),
          legend.position   = "bottom")
}

# stamp a plotly figure with version + generated time (rides along in the PNG export),
# and paint its background to match the active theme
stamp_fig <- function(p, col = list(bg="#ffffff", fg="#333"), filt = NULL) {
  gen <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  # Footer is placed with a PIXEL offset (yshift) from the bottom of the plot area, so it
  # lands in the same spot on every plot regardless of height. Axes use automargin so long
  # labels (site names, titles) are never clipped; legend moves to the top.
  p |> plotly::layout(
    paper_bgcolor = col$bg, plot_bgcolor = col$bg, font = list(color = col$fg),
    margin = list(l = 20, r = 30, t = 56, b = 112, pad = 4),
    xaxis = list(automargin = TRUE, title = list(standoff = 10)),
    yaxis = list(automargin = TRUE, title = list(standoff = 10)),
    legend = list(orientation = "h", x = 0, y = 1.02, xanchor = "left", yanchor = "bottom"),
    annotations = list(list(
      text = paste0(if (!is.null(filt)) paste0(filt, "<br>"),
                    "Where's O Mykiss v", APP_VERSION, "  \u00b7  Generated ", gen, " UTC"),
      x = 1, y = 0, xref = "paper", yref = "paper", yshift = -78,
      xanchor = "right", yanchor = "top", align = "right", showarrow = FALSE,
      font = list(size = 10, color = col$fg)))) |>
    plotly::config(toImageButtonOptions = list(format = "png", scale = 2, filename = "wheres_o_mykiss"))
}

# ---- 8 themes ----
mk <- function(bg, fg, primary, secondary = "#1aa37d")
  bs_theme(version = 5, bg = bg, fg = fg, primary = primary, secondary = secondary)
themes <- list(
  classic  = mk("#f6f9fb", "#0e2d49", "#1f5f9e"),
  viridis  = mk("#f3f9f6", "#1c3b44", "#21918c", "#5ec962"),
  dark     = mk("#15181c", "#e6edf3", "#3b82f6"),
  magma    = mk("#140b1f", "#f3e6ef", "#de4968", "#fe9f6d"),
  graphite = mk("#1b1b1d", "#e4e4e6", "#e03a3a", "#8a8a8e"),
  forest   = mk("#0f1a12", "#d7e6d8", "#2f9e5e", "#6cc08a"),
  sand     = mk("#f5efe2", "#3a2f1e", "#b5801f", "#caa45a"),
  highcon  = mk("#000000", "#ffffff", "#ffd400", "#ffffff"))
theme_choices <- c("Classic"="classic","Viridis"="viridis","Dark"="dark","Magma"="magma",
                   "Graphite"="graphite","Forest"="forest","Sand"="sand","High Contrast"="highcon")

# ---- 10 fonts ----
font_stacks <- c(
  "Space Grotesk" = "'Space Grotesk', system-ui, sans-serif",
  "Inter"         = "'Inter', system-ui, sans-serif",
  "Rubik"         = "'Rubik', system-ui, sans-serif",
  "DM Sans"       = "'DM Sans', system-ui, sans-serif",
  "Work Sans"     = "'Work Sans', system-ui, sans-serif",
  "Montserrat"    = "'Montserrat', system-ui, sans-serif",
  "Oswald"        = "'Oswald', system-ui, sans-serif",
  "Archivo"       = "'Archivo', system-ui, sans-serif",
  "Arial"         = "Arial, Helvetica, sans-serif",
  "Trebuchet MS"  = "'Trebuchet MS', Verdana, sans-serif")

# ---- animated brand (two fish, alternating; swims across) ----
fish_svg <- '
<div class="swim-lane"><div class="swim">
  <svg class="fish fish-rb" width="60" height="32" viewBox="0 0 64 32">
    <g class="tail"><path d="M14,16 L2,7 L5,16 L2,25 Z" fill="#6aa0c8"/></g>
    <ellipse cx="36" cy="16" rx="22" ry="8.5" fill="#8fb98e"/>
    <path d="M14,17.5 C26,22 50,22 58,16.6 L58,17.4 C50,23 26,23 14,19 Z" fill="#d9e6dd"/>
    <rect x="16" y="14.6" width="40" height="2.6" rx="1.3" fill="#e8608a" opacity="0.9"/>
    <path d="M30,8 L42,8 L36,2 Z" fill="#7faa7e"/><path d="M30,25 L40,25 L35,30 Z" fill="#7faa7e"/>
    <circle cx="24" cy="11" r="1" fill="#3f5d3e"/><circle cx="31" cy="10" r="1" fill="#3f5d3e"/>
    <circle cx="38" cy="11" r="1" fill="#3f5d3e"/><circle cx="45" cy="12" r="1" fill="#3f5d3e"/>
    <circle cx="54" cy="13.5" r="1.8" fill="#143"/><circle cx="54.6" cy="13" r="0.6" fill="#fff"/>
  </svg>
  <svg class="fish fish-st" width="60" height="32" viewBox="0 0 64 32">
    <defs><linearGradient id="sg" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#9fb2bd"/><stop offset="0.5" stop-color="#d7e1e6"/>
      <stop offset="1" stop-color="#aebcc4"/></linearGradient></defs>
    <g class="tail"><path d="M14,16 L2,7 L5,16 L2,25 Z" fill="#9aacb6"/></g>
    <ellipse cx="36" cy="16" rx="22" ry="8.5" fill="url(#sg)"/>
    <rect x="16" y="15" width="40" height="1.8" rx="0.9" fill="#b9c6cd" opacity="0.8"/>
    <path d="M30,8 L42,8 L36,2.5 Z" fill="#9fb2bd"/><path d="M30,25 L40,25 L35,30 Z" fill="#9fb2bd"/>
    <circle cx="24" cy="12" r="0.8" fill="#6b7b84"/><circle cx="34" cy="11.5" r="0.8" fill="#6b7b84"/>
    <circle cx="44" cy="12" r="0.8" fill="#6b7b84"/>
    <circle cx="54" cy="13.5" r="1.8" fill="#14323f"/><circle cx="54.6" cy="13" r="0.6" fill="#fff"/>
  </svg>
  <span class="bubble"></span><span class="bubble b2"></span><span class="bubble b3"></span>
</div></div>'
brand_html <- tags$span(
  class = "brand",
  HTML(fish_svg),
  tags$span(class = "brand-title", HTML("Where&#39;s <span class=\"q\">O&nbsp;Mykiss</span>")),
  tags$span(class = "beta", "Beta"))

google_fonts <- "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;700&family=Inter:wght@400;600&family=Rubik&family=DM+Sans&family=Work+Sans&family=Montserrat&family=Oswald&family=Archivo&display=swap"

# ---- theme preview cards (gradient, bg, surface, primary, text) ----
theme_prev <- list(
  classic  = c("linear-gradient(90deg,#0e2d49,#1aa37d)","#f6f9fb","#dde7ee","#1f5f9e","#0e2d49"),
  viridis  = c("linear-gradient(90deg,#3b528b,#21918c 55%,#5ec962)","#f3f9f6","#d6ebe2","#21918c","#1c3b44"),
  dark     = c("linear-gradient(90deg,#0b0e12,#13343b)","#15181c","#262b31","#3b82f6","#e6edf3"),
  magma    = c("linear-gradient(90deg,#0b0414,#3b0f70 55%,#b73779)","#140b1f","#2a1540","#de4968","#f3e6ef"),
  graphite = c("linear-gradient(90deg,#141416,#4a1f1f)","#1b1b1d","#2c2c30","#e03a3a","#e4e4e6"),
  forest   = c("linear-gradient(90deg,#0a140d,#1f6b43)","#0f1a12","#1b2b20","#2f9e5e","#d7e6d8"),
  sand     = c("linear-gradient(90deg,#6b5a33,#b5801f)","#f5efe2","#e6dcc5","#b5801f","#3a2f1e"),
  highcon  = c("linear-gradient(90deg,#000,#3a3a00)","#000000","#1a1a1a","#ffd400","#ffffff"))
theme_card <- function(key) {
  p <- theme_prev[[key]]; lab <- names(theme_choices)[theme_choices == key]
  HTML(sprintf(paste0('<div class="tcard"><div class="hd" style="background:%s"></div>',
    '<div class="sw"><i style="background:%s"></i><i style="background:%s"></i>',
    '<i style="background:%s"></i><i style="background:%s"></i></div>',
    '<div class="nm">%s</div></div>'),
    p[1], p[2], p[3], p[4], p[5], lab))
}
theme_names_ui <- lapply(unname(theme_choices), theme_card)

# ---- live clocks (top-right UTC + world-clock popover) ----
clock_html <- HTML(paste0(
  '<div class="clockbox"><div class="clk-head"><span class="clk-label">UTC</span>&nbsp;',
  '<span class="clk" data-tz="UTC" data-full="1">-------  --:--:--</span><span class="byline"> · Macus Ong</span></div>',
  '<div class="worldclocks">',
  '<div class="row"><span>Vancouver</span><span data-tz="America/Vancouver">--</span></div>',
  '<div class="row"><span>Kuala Lumpur</span><span data-tz="Asia/Kuala_Lumpur">--</span></div>',
  '<div class="row"><span>London</span><span data-tz="Europe/London">--</span></div>',
  '<div class="row"><span>Sydney</span><span data-tz="Australia/Sydney">--</span></div>',
  '<div class="row"><span>Johannesburg</span><span data-tz="Africa/Johannesburg">--</span></div>',
  '</div></div>'))
clock_js <- HTML(paste0(
  "(function(){function tick(){var now=new Date();",
  "document.querySelectorAll('[data-tz]').forEach(function(el){try{",
  "if(el.hasAttribute('data-full')){",
  "el.textContent=new Intl.DateTimeFormat('en-CA',{timeZone:el.dataset.tz,year:'numeric',month:'2-digit',",
  "day:'2-digit',hour:'2-digit',minute:'2-digit',second:'2-digit',hour12:false}).format(now).replace(', ',' ');",
  "}else{",
  "el.textContent=new Intl.DateTimeFormat('en-GB',{timeZone:el.dataset.tz,",
  "hour:'2-digit',minute:'2-digit',second:'2-digit',hour12:false}).format(now);}",
  "}catch(e){}});}",
  "setInterval(tick,1000);tick();setTimeout(tick,400);})();"))

# ---- animated seabird banner for the Outmigration & predation tab ----
seabird_html <- paste0(
  '<div class="seabanner"><div class="waterline"></div>',
  '<svg class="gull g1" viewBox="0 0 40 22"><g><path class="wl" d="M20,12 Q11,3 2,12" stroke="#33434f" stroke-width="2.4" fill="none" stroke-linecap="round"/><path class="wr" d="M20,12 Q29,3 38,12" stroke="#33434f" stroke-width="2.4" fill="none" stroke-linecap="round"/></g></svg>',
  '<svg class="gull g2" viewBox="0 0 40 22"><g><path class="wl" d="M20,12 Q11,3 2,12" stroke="#3b4a56" stroke-width="2.6" fill="none" stroke-linecap="round"/><path class="wr" d="M20,12 Q29,3 38,12" stroke="#3b4a56" stroke-width="2.6" fill="none" stroke-linecap="round"/></g></svg>',
  '<svg class="gull g3" viewBox="0 0 40 22"><g><path class="wl" d="M20,12 Q11,3 2,12" stroke="#4a5864" stroke-width="2.6" fill="none" stroke-linecap="round"/><path class="wr" d="M20,12 Q29,3 38,12" stroke="#4a5864" stroke-width="2.6" fill="none" stroke-linecap="round"/></g></svg>',
  '<svg class="diver" viewBox="0 0 44 22"><g><path d="M22,12 Q12,2 2,12" stroke="#2b3a46" stroke-width="2.6" fill="none" stroke-linecap="round"/><path d="M22,12 Q32,2 42,12" stroke="#2b3a46" stroke-width="2.6" fill="none" stroke-linecap="round"/><path d="M20,12 L24,12 L23,15 Z" fill="#e0a21a"/></g></svg>',
  '<svg class="prey" width="26" height="14" viewBox="0 0 34 16"><ellipse cx="19" cy="8" rx="12" ry="4.5" fill="#9fb9c9"/><path d="M8,8 L1,4 L3,8 L1,12 Z" fill="#9fb9c9"/><circle cx="27" cy="7" r="1" fill="#143"/></svg>',
  '<div class="capt">Smolts outmigrating past Columbia bird colonies</div></div>')

# ---- rotating "notable fish" showcase (Stories tab) ----
notable <- list(
  c("3DD.003D53EC37","nb-green","Returned","Vaseux Creek → Wells adult ladder (May 2024) → home to spawn in 2025. The only confirmed round trip."),
  c("3DD.0077CDB9CD","nb-red","Predation","Outmigrated past Rocky Reach in 2020; tag recovered at East Sand Island — eaten by birds in the estuary."),
  c("3DD.0077CFF969","nb-red","Predation","Left from Vaseux (2020); recovered at Badger Island in the McNary pool, 2021."),
  c("3DD.003D53E384","nb-red","Predation","From Inkaneep (2023); recovered at Little Miller Island, 2024."),
  c("3DD.0077CE1DF3","nb-amber","In-basin","Recovered in its natal Shingle Creek the same year it was tagged — never left the Okanagan."))
notable_html <- HTML(paste0('<div class="nb-carousel" id="nbCarousel">',
  paste0(vapply(seq_along(notable), function(i) {
    x <- notable[[i]]
    sprintf('<div class="nb-slide%s"><div class="nb-top"><span class="nb-tag">%s</span><span class="nb-badge %s">%s</span></div><div class="nb-line">%s</div></div>',
            if (i == 1) " active" else "", x[1], x[2], x[3], x[4])
  }, character(1)), collapse = ""), '</div>'))
carousel_js <- HTML(paste0(
  "(function(){var i=0;function rot(){var s=document.querySelectorAll('#nbCarousel .nb-slide');",
  "if(!s.length)return;s[i].classList.remove('active');i=(i+1)%s.length;s[i].classList.add('active');}",
  "setInterval(rot,5000);})();"))

# =====================================================================
# UI
# =====================================================================
ui <- page_navbar(
  id = "mainnav",
  title = brand_html,
  window_title = "Where's O Mykiss",
  theme = themes[["classic"]],
  fillable = FALSE,
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = ""),
      tags$link(rel = "stylesheet", href = google_fonts),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    tags$script(clock_js), tags$script(carousel_js)
  ),

  sidebar = sidebar(
    width = 320,
    navset_pill(
      nav_panel(
        "Filters",
        selectInput("f_loc", "Release location (stream)", choices = locs,
                    selected = locs, multiple = TRUE),
        sliderInput("f_mark", "Mark year", min = my_min, max = my_max,
                    value = c(my_min, my_max), step = 1, sep = ""),
        sliderInput("f_rel", "Release year", min = ry_min, max = ry_max,
                    value = c(ry_min, ry_max), step = 1, sep = ""),
        actionButton("f_reset", "Reset filters", class = "btn-sm btn-outline-secondary"),
        tags$div(class = "filt-count text-muted small mt-2", textOutput("f_count"))
      ),
      nav_panel(
        "Settings",
        tags$label(class = "form-label fw-semibold mt-1", "Theme"),
        tags$div(class = "theme-picker",
          radioButtons("theme", NULL, choiceNames = theme_names_ui,
                       choiceValues = unname(theme_choices), selected = "classic")),
        selectInput("font", "Interface font", choices = names(font_stacks),
                    selected = "Space Grotesk"),
        tags$div(class = "font-preview", id = "fontPreview",
                 "The quick brown fox jumps over the lazy dog 0123456789")
      )
    )
  ),

  nav_panel(
    "Overview", icon = bs_icon("house"),
    layout_column_wrap(
      width = 1/5, heights_equal = "row", fill = FALSE,
      value_box("Fish tagged", textOutput("kpi_fish"),
                showcase = bs_icon("tag"), theme = value_box_theme(bg = "#1f5f9e", fg = "white")),
      value_box("Events tracked", textOutput("kpi_events"),
                showcase = bs_icon("broadcast"), theme = value_box_theme(bg = "#1aa37d", fg = "white")),
      value_box("Left the Okanagan", textOutput("kpi_left"),
                showcase = bs_icon("box-arrow-right"), theme = value_box_theme(bg = "#e08a2e", fg = "white")),
      value_box("Predation deaths", textOutput("kpi_pred"),
                showcase = bs_icon("exclamation-triangle"), theme = value_box_theme(bg = pal$red, fg = "white")),
      value_box("Confirmed return", textOutput("kpi_ret"),
                showcase = bs_icon("house-heart"), theme = value_box_theme(bg = pal$green, fg = "white"))
    ),
    card(card_header("Correction note"), markdown(
      "An earlier draft (interrogation detections only) mislabelled outmigrating smolts as having
      *returned to Canada* and over-called them *steelhead*. Adding **recapture** and **recovery**
      events fixed this: fish that go downstream and are never seen again have **left**, and several
      were recovered dead on Columbia predation islands. Individual fish are **not** labelled
      steelhead vs rainbow — detections cannot resolve that.")),
    card(card_header("Fate of the selected fish"),
         plotlyOutput("plot_fate", height = "330px"), DTOutput("tbl_fate")),
    tags$div(class = "provenance", textOutput("prov_overview"))
  ),

  nav_panel(
    "Stories", icon = bs_icon("book"),
    layout_column_wrap(
      width = 1/2,
      card(class = "story-card story-win",
        card_header("The one that came home"),
        markdown("Tagged as a juvenile in **Vaseux Creek** (Sept 2022), this fish went to sea, climbed the
          **Wells Dam adult ladder in May 2024** on its way back, passed **Zosel Dam** in spring 2025, and
          returned to **Vaseux Creek** in April–May 2025 to spawn — natal homing, confirmed by
          detections alone. The one fish in the whole 2019+ cohort that completed the full round trip."),
        actionButton("trace_return", "Trace this fish in Fish explorer →", class = "btn-sm btn-success")),
      card(class = "story-card story-loss",
        card_header("One that didn't make it"),
        markdown("Tagged in **Vaseux Creek** (Sept 2019), this smolt outmigrated past **Rocky Reach Dam**
          in June 2020. Its tag was **recovered at East Sand Island** in the Columbia estuary in 2021 —
          eaten by birds. It never returned: the common outcome, and one of five confirmed lost to predation."),
        actionButton("trace_loss", "Trace this fish in Fish explorer →", class = "btn-sm btn-danger"))
    ),
    card(card_header("More notable fish"), notable_html,
         card_footer(class = "text-muted", "Auto-rotating · find any fish in Fish explorer."))
  ),

  nav_panel(
    "Tributary timing", icon = bs_icon("calendar-event"),
    card(card_header("First detection in each tributary (antenna observations)"),
         checkboxGroupInput("tim_creeks", NULL,
           choices = setNames(creek_codes, creek_names[creek_codes]),
           selected = creek_codes, inline = TRUE),
         plotlyOutput("plot_timing", height = "460px"),
         card_footer(class = "text-muted",
           "Each point is one fish's first antenna detection at that creek, by day of year."))
  ),

  nav_panel(
    "Presence", icon = bs_icon("grid-3x3"),
    card(card_header("Unique fish detected by site and year (antenna observations)"),
         plotlyOutput("plot_heat", height = "520px"))
  ),

  nav_panel(
    "Cohorts", icon = bs_icon("bar-chart-steps"),
    card(card_header("What happened to each release-year cohort?",
           radioButtons("coh_mode", NULL, c("Percent of cohort" = "fill", "Number of fish" = "stack"),
                        inline = TRUE)),
         plotlyOutput("plot_cohort", height = "420px")),
    card(card_header("Cohort summary"), DTOutput("tbl_cohort"),
         card_footer(class = "text-muted",
           "Later cohorts have had less time to complete a return (steelhead can spend 1\u20134 years at sea), so low return counts for recent years are expected. Counts respond to the left-panel filters.")),
    tags$div(class = "provenance", textOutput("prov_cohort"))
  ),

  nav_panel(
    "Corridor map", icon = bs_icon("map"),
    card(card_header("Where tagged fish were detected, from the Okanagan to the Pacific"),
         plotlyOutput("plot_map", height = "600px"),
         card_footer(class = "text-muted",
           "Circle size = number of fish detected or recovered at that site (marks excluded). Site coordinates are approximate, for orientation only. The line is an indicative downstream route, not a tracked path.")),
    card(card_header("Detections by site"), DTOutput("tbl_map")),
    tags$div(class = "provenance", textOutput("prov_map"))
  ),

  nav_panel(
    "Outmigration & predation", icon = bs_icon("box-arrow-right"),
    tags$div(class = "seabanner-wrap", HTML(seabird_html)),
    card(card_header("What do these numbers mean?"), uiOutput("wnm")),
    layout_column_wrap(
      width = 1/2,
      card(card_header("Outmigration timing (first downstream detection)"),
           plotlyOutput("plot_out", height = "330px")),
      card(card_header("Tag / carcass recoveries"), DTOutput("tbl_rec"))
    ),
    card(card_header("What the recoveries mean"), markdown(
      "Five of the fish that left were recovered dead on Columbia River **bird-predation islands**
      (East Sand, Badger, Little Miller) — direct evidence of **avian predation of outmigrating
      smolts**. Combined with near-zero detected returns, this underlines how little of the
      population survives the seaward journey.")),
    tags$div(class = "provenance", textOutput("prov_pred"))
  ),

  nav_panel(
    "Estuary predation", icon = bs_icon("crosshair"),
    layout_column_wrap(
      width = 1/4, fill = FALSE,
      value_box("Tags on bird colonies", textOutput("ep_n"), showcase = bs_icon("tag"),
                theme = value_box_theme(bg = pal$red, fg = "white")),
      value_box("Median days, release to recovery", textOutput("ep_days"), showcase = bs_icon("hourglass-split"),
                theme = value_box_theme(bg = "#e08a2e", fg = "white")),
      value_box("Seen leaving first", textOutput("ep_seen"), showcase = bs_icon("eye"),
                theme = value_box_theme(bg = "#1f5f9e", fg = "white")),
      value_box("Colonies involved", textOutput("ep_isl"), showcase = bs_icon("geo-alt"),
                theme = value_box_theme(bg = "#1aa37d", fg = "white"))
    ),
    card(card_header("Journey of each fish recovered on a bird colony"),
         plotlyOutput("plot_ep", height = "380px")),
    layout_column_wrap(
      width = 1/2,
      card(card_header("Recoveries by colony"), plotlyOutput("plot_ep_isl", height = "300px")),
      card(card_header("Fish detail"), DTOutput("tbl_ep"))
    ),
    card(card_header("How to read this"), markdown(
      "Colonies are scanned for tags after the nesting season, so the **recovery date is when the colony
       was surveyed, not the day the fish was eaten**. The true predation date lies between the last
       downstream detection and the survey. Recovered tags are a **minimum**: most predated tags are never
       found, so these five fish are a floor for avian predation, not a rate.")),
    tags$div(class = "provenance", textOutput("prov_ep"))
  ),

  nav_panel(
    "Fish explorer", icon = bs_icon("search"),
    layout_sidebar(
      sidebar = sidebar(
        selectInput("fx_fate", "Filter by fate", choices = c("All", fs$fate), selected = "All"),
        selectizeInput("fx_tag", "Tag code", choices = NULL),
        helpText("Pick any fish to trace its full event history.")
      ),
      layout_column_wrap(
        width = 1/3, fill = FALSE,
        value_box("Release", textOutput("fx_release"), showcase = bs_icon("geo-alt")),
        value_box("Fate", textOutput("fx_fate_txt"), showcase = bs_icon("signpost-split")),
        value_box("Events", textOutput("fx_nevents"), showcase = bs_icon("list-ol"))
      ),
      card(card_header("Event timeline"), plotlyOutput("fx_timeline", height = "320px")),
      card(card_header("Every event for this fish"), DTOutput("fx_events"))
    )
  ),

  nav_panel(
    "Data", icon = bs_icon("table"),
    card(card_header("Per-fish table",
                     downloadButton("dl_perfish", "Download CSV", class = "btn-sm")),
         DTOutput("tbl_perfish"))
  ),

  nav_panel(
    "Guide", icon = bs_icon("mortarboard"),
    card(card_header("How to use this dashboard"), markdown(
      "**Left panel — Filters** subset the whole dashboard by release location, mark year and release
       year. **Settings** let you pick a colour theme and interface font.

       **Overview** — value boxes and the fate bar for the fish currently selected.
       **Tributary timing** — when fish were first detected in each creek.
       **Presence** — a site x year heatmap; darker = more fish.
       **Cohorts** — outcomes for each release year.
       **Corridor map** — where fish were detected, Okanagan to ocean.
       **Estuary predation** — fish recovered on bird colonies.
       **Outmigration & predation** — downstream timing and the recoveries that reveal fate.
       **Fish explorer** — pick a tag and trace its whole journey (opens on the one confirmed return).
       **Data** — the full per-fish table, searchable and downloadable.")),
    card(card_header("Reading the fates"), DTOutput("tbl_guide")),
    card(card_header("The big caveat"), markdown(
      "Steelhead and rainbow trout are the **same species** (*O. mykiss*). PIT detections show
       *where and when* a fish was, never *which life history* it is."))
  ),

  nav_spacer(),
  nav_item(clock_html),
  nav_item(tags$span(class = "vchip", paste0("v", APP_VERSION))),
  nav_item(tags$a(bs_icon("github"), href = "https://github.com/MacusOng/steelhead-tracker",
                  target = "_blank", class = "ghlink"))
)

# =====================================================================
# SERVER
# =====================================================================
server <- function(input, output, session) {
  cur_cols <- reactive({
    tk <- input$theme
    if (is.null(tk) || !(tk %in% names(plot_theme_cols))) tk <- "classic"
    plot_theme_cols[[tk]]
  })


  # ---- global filters ----
  pf_f <- reactive({
    req(input$f_loc)
    pf |> filter(release_site %in% input$f_loc,
                 mark_year    >= input$f_mark[1], mark_year    <= input$f_mark[2],
                 release_year >= input$f_rel[1],  release_year <= input$f_rel[2])
  })
  tags_f <- reactive(pf_f()$tag_code)
  ev_f   <- reactive(ev |> filter(tag_code %in% tags_f()))

  output$f_count <- renderText(paste0(nrow(pf_f()), " of ", nrow(pf), " fish selected"))
  observeEvent(input$f_reset, {
    updateSelectInput(session, "f_loc", selected = locs)
    updateSliderInput(session, "f_mark", value = c(my_min, my_max))
    updateSliderInput(session, "f_rel",  value = c(ry_min, ry_max))
  })

  # ---- provenance footer (filters + version + generated time) ----
  prov_text <- reactive({
    loc <- if (length(input$f_loc) == length(locs)) "all locations"
           else paste(input$f_loc, collapse = ", ")
    gen <- format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%d %H:%M:%S")
    paste0("Filters: ", loc,
           " | Mark ", input$f_mark[1], "–", input$f_mark[2],
           " | Release ", input$f_rel[1], "–", input$f_rel[2],
           "  ·  v", APP_VERSION, "  ·  Generated ", gen, " UTC")
  })
  filt_line <- reactive({
    loc <- if (length(input$f_loc) == length(locs)) "all locations" else paste(input$f_loc, collapse = ", ")
    paste0("Filters: ", loc, " | Mark ", input$f_mark[1], "\u2013", input$f_mark[2],
           " | Release ", input$f_rel[1], "\u2013", input$f_rel[2])
  })
  output$prov_overview <- renderText(prov_text())
  output$prov_pred     <- renderText(prov_text())

  # ---- plain-language "what do the numbers mean" ----
  output$wnm <- renderUI({
    d <- pf_f(); total <- nrow(d)
    left <- sum(d$left_basin == "Y", na.rm = TRUE)
    pred <- sum(d$fate == "recovered - predation island")
    ret  <- sum(startsWith(d$fate, "confirmed return"))
    noret <- max(0, left - pred - ret)
    if (left == 0)
      return(HTML(paste0("<p>Of the <b>", format(total, big.mark = ","),
        "</b> fish in your current selection, none were detected leaving the Okanagan toward the ocean, ",
        "so there is nothing to summarise here yet. Widen the filters to include more fish.</p>")))
    pc <- function(n) round(100 * n / left)
    para <- sprintf(paste0(
      "<p>You've selected <b>%s</b> tagged fish. <b>%d</b> were detected leaving the Okanagan toward ",
      "the ocean. Of those that left: <b>%d (%d%%)</b> were recovered dead on Columbia bird-predation ",
      "islands — confirmed eaten by seabirds; <b>%d (%d%%)</b> were never seen again (fate unknown); ",
      "and <b>%d (%d%%)</b> made the full journey home to spawn.</p>"),
      format(total, big.mark = ","), left, pred, pc(pred), noret, pc(noret), ret, pc(ret))
    seg <- function(w, col) sprintf('<div class="seg" style="width:%d%%;background:%s">%s</div>',
                                    w, col, if (w >= 8) paste0(w, "%") else "")
    bar <- paste0('<p class="wnm-sub"><b>Of the fish that left the Okanagan:</b></p><div class="flbar">',
      seg(pc(pred), "#9E2B25"), seg(pc(noret), "#c98a5a"), seg(pc(ret), "#1E6B3A"),
      '</div><div class="leg">',
      '<span><i style="background:#9E2B25"></i>Eaten by birds (', pred, ')</span>',
      '<span><i style="background:#c98a5a"></i>Never seen again (', noret, ')</span>',
      '<span><i style="background:#1E6B3A"></i>Returned home (', ret, ')</span></div>')
    HTML(paste0(para, bar))
  })

  # ---- theme & font ----
  observeEvent(input$theme, {
    session$setCurrentTheme(themes[[input$theme]])
    runjs(sprintf("document.documentElement.setAttribute('data-theme','%s')", input$theme))
  }, ignoreInit = TRUE)
  observeEvent(input$font, {
    runjs(sprintf("document.documentElement.style.setProperty('--app-font', \"%s\")",
                  font_stacks[[input$font]]))
  })

  # ---- KPIs ----
  output$kpi_fish   <- renderText(format(nrow(pf_f()), big.mark = ","))
  output$kpi_events <- renderText(format(nrow(ev_f()), big.mark = ","))
  output$kpi_left   <- renderText(sum(pf_f()$left_basin == "Y", na.rm = TRUE))
  output$kpi_pred   <- renderText(sum(pf_f()$fate == "recovered - predation island"))
  output$kpi_ret    <- renderText(sum(startsWith(pf_f()$fate, "confirmed return")))

  # ---- fate ----
  fate_tbl <- reactive({
    pf_f() |> count(fate, name = "n_fish") |>
      right_join(tibble(fate = fate_order), by = "fate") |>
      mutate(n_fish = coalesce(n_fish, 0L), definition = fate_def[fate]) |>
      arrange(match(fate, fate_order))
  })
  output$plot_fate <- renderPlotly({
    col <- cur_cols()
    d <- fate_tbl() |> mutate(fate = factor(fate, levels = rev(fate_order)))
    p <- ggplot(d, aes(fate, n_fish, fill = fate, text = paste0(fate, ": ", n_fish))) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = fate_cols, guide = "none") +
      scale_y_continuous(labels = comma) + labs(x = NULL, y = "Number of fish") + theme_okn(col)
    ggplotly(p, tooltip = "text") |> layout(showlegend = FALSE) |> stamp_fig(col, filt_line())
  })
  output$tbl_fate <- renderDT(
    datatable(fate_tbl(), rownames = FALSE, options = list(dom = "t", pageLength = 10),
              colnames = c("Fate", "Fish", "Definition")))
  output$tbl_guide <- renderDT(
    datatable(fs, rownames = FALSE, options = list(dom = "t", pageLength = 10),
              colnames = c("Fate", "Fish", "Definition")))

  # ---- tributary timing ----
  output$plot_timing <- renderPlotly({
    col <- cur_cols()
    req(input$tim_creeks)
    d <- ev_f() |> filter(event_type == "Observation", site_code %in% input$tim_creeks) |>
      group_by(tag_code, site_code) |> summarise(doy = min(doy), .groups = "drop") |>
      mutate(creek = creek_names[site_code])
    validate(need(nrow(d) > 0, "No detections for this selection."))
    creek_pal <- c("Inkaneep Creek" = pal$blue, "Vaseux Creek" = pal$orange,
                   "Shingle Creek" = pal$aqua, "Shuttleworth Creek" = "#8a5cd0")
    p <- ggplot(d, aes(doy, creek, color = creek, text = paste0(creek, " · day ", doy))) +
      geom_jitter(height = 0.18, alpha = 0.5, size = 1.6) +
      geom_boxplot(aes(fill = creek), alpha = 0.15, outlier.shape = NA, color = "grey40") +
      scale_color_manual(values = creek_pal, guide = "none") +
      scale_fill_manual(values = creek_pal, guide = "none") +
      scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                         labels = month.abb, limits = c(1, 366)) +
      labs(x = "Day of year (first detection)", y = NULL) + theme_okn(col)
    ggplotly(p, tooltip = "text") |> stamp_fig(col, filt_line())
  })

  # ---- presence ----
  output$plot_heat <- renderPlotly({
    col <- cur_cols()
    d <- ev_f() |> filter(event_type == "Observation") |>
      group_by(site_name, event_year) |> summarise(fish = n_distinct(tag_code), .groups = "drop")
    validate(need(nrow(d) > 0, "No detections for this selection."))
    ord <- d |> group_by(site_name) |> summarise(t = sum(fish)) |> arrange(t) |> pull(site_name)
    d <- d |> mutate(site_name = factor(site_name, levels = ord))
    p <- ggplot(d, aes(factor(event_year), site_name, fill = fish,
                       text = paste0(site_name, " · ", event_year, ": ", fish, " fish"))) +
      geom_tile(color = "white") + geom_text(aes(label = fish), size = 3, color = col$fg) +
      scale_fill_viridis_c(option = "magma", direction = -1, name = "Fish") +
      labs(x = NULL, y = NULL) + theme_okn(col) + theme(legend.position = "right")
    ggplotly(p, tooltip = "text") |> stamp_fig(col, filt_line())
  })

  # ---- outmigration ----
  output$plot_out <- renderPlotly({
    col <- cur_cols()
    d <- ev_f() |> filter(site_code %in% down_sites) |>
      group_by(tag_code) |> summarise(dt = min(event_date), .groups = "drop") |>
      mutate(month = factor(format(dt, "%b"), levels = month.abb)) |>
      count(month, .drop = FALSE) |> filter(as.integer(month) %in% 3:8)
    validate(need(sum(d$n) > 0, "No outmigration detections for this selection."))
    p <- ggplot(d, aes(month, n, text = paste0(month, ": ", n))) +
      geom_col(fill = pal$blue) + geom_text(aes(label = n), vjust = -0.4, size = 4, color = col$fg) +
      labs(x = NULL, y = "Fish") + theme_okn(col)
    ggplotly(p, tooltip = "text") |> stamp_fig(col, filt_line())
  })
  output$tbl_rec <- renderDT(
    datatable(rec |> filter(tag_code %in% tags_f()), rownames = FALSE,
              options = list(dom = "t", pageLength = 10, scrollX = TRUE)))

  output$prov_cohort <- renderText(prov_text())
  output$prov_map    <- renderText(prov_text())
  output$prov_ep     <- renderText(prov_text())

  # ---- cohorts ----
  output$plot_cohort <- renderPlotly({
    col <- cur_cols()
    d <- pf_f() |> count(release_year, fate)
    validate(need(nrow(d) > 0, "No fish for this selection."))
    d <- d |> mutate(fate = factor(fate, levels = rev(fate_order)))
    ylab <- if (!identical(input$coh_mode, "stack")) "Share of cohort" else "Number of fish"
    p <- ggplot(d, aes(factor(release_year), n, fill = fate,
                       text = paste0(release_year, " \u00b7 ", fate, ": ", n))) +
      geom_col(position = if (is.null(input$coh_mode)) "fill" else input$coh_mode) +
      scale_fill_manual(values = fate_cols, name = NULL, breaks = fate_order) +
      labs(x = "Release year", y = ylab) + theme_okn(col)
    if (!identical(input$coh_mode, "stack")) p <- p + scale_y_continuous(labels = percent)
    ggplotly(p, tooltip = "text") |> stamp_fig(col, filt_line())
  })
  output$tbl_cohort <- renderDT({
    d <- pf_f() |> group_by(`Release year` = release_year) |>
      summarise(Tagged = n(),
                Left = sum(left_basin == "Y", na.rm = TRUE),
                `Left (%)` = round(100 * Left / Tagged, 1),
                `Eaten by birds` = sum(fate == "recovered - predation island"),
                Returned = sum(startsWith(fate, "confirmed return")),
                `Returned (%)` = round(100 * Returned / Tagged, 2), .groups = "drop")
    datatable(d, rownames = FALSE, options = list(dom = "t", pageLength = 12))
  })

  # ---- corridor map ----
  site_xy <- tibble::tribble(
    ~site_name,                          ~lat,   ~lon,     ~grp,
    "Inkaneep Creek",                    49.16, -119.56, "Okanagan (in-basin)",
    "Vaseux Creek",                      49.29, -119.53, "Okanagan (in-basin)",
    "Shuttleworth Creek",                49.34, -119.55, "Okanagan (in-basin)",
    "Shatford Creek",                    49.40, -119.57, "Okanagan (in-basin)",
    "Shingle Creek",                     49.43, -119.60, "Okanagan (in-basin)",
    "Okanagan R. @ VDS-3",               49.25, -119.54, "Okanagan (in-basin)",
    "McIntyre Dam",                      49.20, -119.55, "Okanagan (in-basin)",
    "Skaha Dam ladder",                  49.34, -119.57, "Okanagan (in-basin)",
    "Penticton Channel",                 49.48, -119.58, "Okanagan (in-basin)",
    "Penticton Dam",                     49.47, -119.59, "Okanagan (in-basin)",
    "Zosel Dam",                         48.98, -119.46, "Okanagan (in-basin)",
    "Wells Dam adult ladder",            47.95, -119.86, "Return corridor",
    "Rocky Reach Dam (juv)",             47.53, -120.30, "Columbia downstream",
    "Bonneville Corner Collector",       45.64, -121.94, "Columbia downstream",
    "Bonneville Dam (juv)",              45.64, -121.94, "Columbia downstream",
    "Columbia estuary rkm62",            46.20, -123.68, "Columbia downstream",
    "Estuary towed array",               46.25, -123.90, "Columbia downstream",
    "Badger Island (predation)",         46.18, -119.20, "Bird colony (predation)",
    "Little Miller Island (predation)",  45.65, -121.00, "Bird colony (predation)",
    "East Sand Island (predation)",      46.26, -123.98, "Bird colony (predation)")
  grp_cols <- c("Okanagan (in-basin)" = "#2a78d6", "Return corridor" = "#1E6B3A",
                "Columbia downstream" = "#eb6834", "Bird colony (predation)" = "#9E2B25")
  map_tbl <- reactive({
    ev_f() |> filter(event_type != "Mark") |>
      group_by(site_name) |> summarise(fish = n_distinct(tag_code), .groups = "drop") |>
      inner_join(site_xy, by = "site_name")
  })
  output$plot_map <- renderPlotly({
    col <- cur_cols()
    d <- map_tbl(); validate(need(nrow(d) > 0, "No detections for this selection."))
    dark <- input$theme %in% c("dark", "magma", "graphite", "forest", "highcon")
    route <- site_xy[match(c("Vaseux Creek", "Wells Dam adult ladder", "Rocky Reach Dam (juv)",
                             "Badger Island (predation)", "Little Miller Island (predation)",
                             "Bonneville Dam (juv)", "East Sand Island (predation)"), site_xy$site_name), ]
    p <- plot_ly() |>
      add_trace(type = "scattermapbox", mode = "lines", lat = route$lat, lon = route$lon,
                line = list(color = if (dark) "#8fb3d1" else "#4a6a85", width = 2),
                hoverinfo = "none", showlegend = FALSE)
    mx <- max(d$fish)
    for (g in names(grp_cols)) {
      dg <- d[d$grp == g, ]
      if (!nrow(dg)) next
      p <- p |> add_trace(type = "scattermapbox", mode = "markers", name = g,
        lat = dg$lat, lon = dg$lon,
        marker = list(size = 9 + 30 * sqrt(dg$fish / mx), color = grp_cols[[g]], opacity = 0.8),
        text = paste0(dg$site_name, ": ", dg$fish, " fish"), hoverinfo = "text")
    }
    p |> layout(mapbox = list(style = if (dark) "carto-darkmatter" else "carto-positron",
                              center = list(lon = -120.9, lat = 46.9), zoom = 4.6)) |>
      stamp_fig(col, filt_line())
  })
  output$tbl_map <- renderDT({
    d <- map_tbl() |> arrange(desc(fish)) |> select(Site = site_name, Group = grp, Fish = fish)
    datatable(d, rownames = FALSE, options = list(dom = "tp", pageLength = 10))
  })

  # ---- estuary predation deep-dive ----
  ep_df <- reactive({
    r <- rec |> filter(zone == "Columbia predation island", tag_code %in% tags_f())
    if (!nrow(r)) return(r |> mutate(mark_date = as.Date(character()), down_date = as.Date(character()),
                                      island = character(), days = numeric(), label = character()))
    mk <- ev |> filter(event_type == "Mark") |> group_by(tag_code) |>
      summarise(mark_date = min(event_date), .groups = "drop")
    dn <- ev |> filter(site_code %in% down_sites) |> group_by(tag_code) |>
      summarise(down_date = min(event_date), .groups = "drop")
    r |> left_join(mk, by = "tag_code") |> left_join(dn, by = "tag_code") |>
      mutate(recovery_date = as.Date(recovery_date),
             island = sub(" \\(predation\\)", "", recovery_site),
             days = as.numeric(recovery_date - mark_date),
             label = paste0(substr(tag_code, 5, 14), " \u00b7 ", release_site, " ", release_year)) |>
      arrange(mark_date)
  })
  output$ep_n    <- renderText(nrow(ep_df()))
  output$ep_days <- renderText(if (nrow(ep_df())) format(round(median(ep_df()$days, na.rm = TRUE)), big.mark = ",") else "\u2013")
  output$ep_seen <- renderText(paste0(sum(!is.na(ep_df()$down_date)), " of ", nrow(ep_df())))
  output$ep_isl  <- renderText(n_distinct(ep_df()$island))
  output$plot_ep <- renderPlotly({
    col <- cur_cols(); d <- ep_df()
    validate(need(nrow(d) > 0, "No bird-colony recoveries for this selection."))
    d <- d |> mutate(label = factor(label, levels = rev(unique(label))))
    pts <- bind_rows(
      d |> transmute(label, date = mark_date, stage = "Tagged & released"),
      d |> filter(!is.na(down_date)) |> transmute(label, date = down_date, stage = "First downstream detection"),
      d |> transmute(label, date = recovery_date, stage = paste0("Recovered on colony")))
    p <- ggplot() +
      geom_segment(data = d, aes(x = mark_date, xend = recovery_date, y = label, yend = label),
                   color = col$grid, linewidth = 1.2) +
      geom_point(data = pts, aes(date, label, color = stage,
                 text = paste0(label, "<br>", stage, ": ", date)), size = 3.4) +
      scale_color_manual(values = c("Tagged & released" = pal$grey,
                                    "First downstream detection" = pal$blue,
                                    "Recovered on colony" = pal$red), name = NULL) +
      labs(x = NULL, y = NULL) + theme_okn(col)
    ggplotly(p, tooltip = "text") |> stamp_fig(col, filt_line())
  })
  output$plot_ep_isl <- renderPlotly({
    col <- cur_cols(); d <- ep_df()
    validate(need(nrow(d) > 0, "No bird-colony recoveries for this selection."))
    d <- d |> count(island)
    p <- ggplot(d, aes(reorder(island, n), n, text = paste0(island, ": ", n))) +
      geom_col(fill = pal$red) + coord_flip() +
      scale_y_continuous(breaks = scales::breaks_pretty()) +
      labs(x = NULL, y = "Tags recovered") + theme_okn(col)
    ggplotly(p, tooltip = "text") |> stamp_fig(col, filt_line())
  })
  output$tbl_ep <- renderDT({
    d <- ep_df() |> transmute(Tag = tag_code, Colony = island, `Release` = paste(release_site, release_year),
                              `Recovered` = recovery_date, `Days at large` = days)
    datatable(d, rownames = FALSE, options = list(dom = "t", scrollX = TRUE, pageLength = 10))
  })

  # ---- fish explorer ----
  observe({
    base <- pf_f()
    tags <- if (input$fx_fate == "All") base$tag_code else base$tag_code[base$fate == input$fx_fate]
    cur  <- isolate(input$fx_tag)
    sel  <- if (!is.null(cur) && cur %in% tags) cur
            else if ("3DD.003D53EC37" %in% tags) "3DD.003D53EC37" else tags[1]
    updateSelectizeInput(session, "fx_tag", choices = tags, selected = sel, server = TRUE)
  })

  trace_to <- function(tag) {
    updateSelectInput(session, "fx_fate", selected = "All")
    updateSelectizeInput(session, "fx_tag", choices = pf$tag_code, selected = tag, server = TRUE)
    nav_select("mainnav", "Fish explorer")
  }
  observeEvent(input$trace_return, trace_to("3DD.003D53EC37"))
  observeEvent(input$trace_loss,   trace_to("3DD.0077CDB9CD"))
  fx_row <- reactive({ req(input$fx_tag); pf[pf$tag_code == input$fx_tag, ][1, ] })
  fx_ev  <- reactive({ req(input$fx_tag); ev[ev$tag_code == input$fx_tag, ] |> arrange(event_date) })
  output$fx_release  <- renderText(paste0(fx_row()$release_site, " (", fx_row()$release_year, ")"))
  output$fx_fate_txt <- renderText(fx_row()$fate)
  output$fx_nevents  <- renderText(nrow(fx_ev()))
  output$fx_timeline <- renderPlotly({
    col <- cur_cols()
    d <- fx_ev(); validate(need(nrow(d) > 0, "No events."))
    d <- d |> mutate(site_name = factor(site_name, levels = unique(site_name)))
    p <- ggplot(d, aes(event_date, site_name, color = event_type,
                       text = paste0(event_type, " · ", site_name, " · ", event_date))) +
      geom_line(aes(group = 1), color = "grey70") + geom_point(size = 3) +
      scale_color_manual(values = c(Mark = pal$grey, Observation = pal$blue,
                                    Recapture = pal$orange, Recovery = pal$red), name = NULL) +
      labs(x = NULL, y = NULL) + theme_okn(col)
    ggplotly(p, tooltip = "text") |> stamp_fig(col, paste0("Fish: ", input$fx_tag))
  })
  output$fx_events <- renderDT(
    datatable(fx_ev() |> select(event_date, event_type, site_name, zone), rownames = FALSE,
              options = list(dom = "tp", pageLength = 15),
              colnames = c("Date", "Event", "Site", "Zone")))

  # ---- data tab ----
  output$tbl_perfish <- renderDT(
    datatable(pf_f(), rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE)))
  output$dl_perfish <- downloadHandler(
    filename = function() "okanagan_steelhead_per_fish.csv",
    content  = function(file) write_csv(pf_f(), file))
}

shinyApp(ui, server)
