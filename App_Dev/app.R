library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(viridisLite)

# =====================================================================
# Let's Follow the Steelhead  —  Okanagan O. mykiss PIT detection explorer
# Part of the CSS FTT dashboard family (sibling of sockeye-tracker).
# Data: PTAGIS complete tag history, all event types, 19 Aug 2026.
# Cohort = fish released 2019+. Dates are as recorded (UTC).
# =====================================================================
APP_VERSION <- "0.1.0"

# ---- load data ----
pf   <- read_csv("data/per_fish.csv",         show_col_types = FALSE)
ev   <- read_csv("data/events_clean.csv",     show_col_types = FALSE) |>
          mutate(event_date = as.Date(event_date),
                 doy = as.integer(format(event_date, "%j")))
fs   <- read_csv("data/fate_summary.csv",     show_col_types = FALSE)
rec  <- read_csv("data/recoveries.csv",       show_col_types = FALSE)
cret <- read_csv("data/confirmed_return.csv", show_col_types = FALSE) |>
          mutate(event_date = as.Date(event_date))

# ---- palette & lookups (full names only; no site codes in the UI) ----
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
yr_max <- max(pf$release_year, na.rm = TRUE)

theme_okn <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(color = pal$navy, face = "bold"),
          legend.position = "bottom")
}

# =====================================================================
# UI
# =====================================================================
ui <- page_navbar(
  title = tags$span(bs_icon("water"), " Let's Follow the Steelhead"),
  window_title = "Let's Follow the Steelhead",
  theme = bs_theme(version = 5, primary = pal$navy),
  fillable = FALSE,

  nav_panel(
    "Overview", icon = bs_icon("house"),
    layout_column_wrap(
      width = 1/5, heights_equal = "row", fill = FALSE,
      value_box("Fish tagged (2019+)", textOutput("kpi_fish"),
                showcase = bs_icon("tag"), theme = "primary"),
      value_box("Events tracked", textOutput("kpi_events"),
                showcase = bs_icon("broadcast"), theme = "secondary"),
      value_box("Left the Okanagan", textOutput("kpi_left"),
                showcase = bs_icon("box-arrow-right")),
      value_box("Predation deaths", textOutput("kpi_pred"),
                showcase = bs_icon("exclamation-triangle"),
                theme = value_box_theme(bg = pal$red, fg = "white")),
      value_box("Confirmed return", textOutput("kpi_ret"),
                showcase = bs_icon("house-heart"),
                theme = value_box_theme(bg = pal$green, fg = "white"))
    ),
    card(card_header("Correction note"), markdown(
      "An earlier draft (interrogation detections only) mislabelled outmigrating smolts as having
      *returned to Canada* and over-called them *steelhead*. Adding **recapture** and **recovery**
      events fixed this: fish that go downstream and are never seen again have **left**, and several
      were recovered dead on Columbia predation islands. Individual fish are **not** labelled
      steelhead vs rainbow — detections cannot resolve that.")),
    card(card_header("Fate of the tagged fish"),
         plotlyOutput("plot_fate", height = "330px"), DTOutput("tbl_fate"))
  ),

  nav_panel(
    "Tributary timing", icon = bs_icon("calendar-event"),
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("tim_creeks", "Tributaries",
          choices = setNames(creek_codes, creek_names[creek_codes]), selected = creek_codes),
        sliderInput("tim_years", "Observation year",
          min = 2019, max = yr_max, value = c(2019, yr_max), step = 1, sep = "")
      ),
      card(card_header("First detection in each tributary (antenna observations)"),
           plotlyOutput("plot_timing", height = "460px"),
           card_footer(class = "text-muted",
             "Each point is one fish's first antenna detection at that creek, by day of year."))
    )
  ),

  nav_panel(
    "Presence", icon = bs_icon("grid-3x3"),
    card(card_header("Unique fish detected by site and year (antenna observations)"),
         plotlyOutput("plot_heat", height = "520px"))
  ),

  nav_panel(
    "Outmigration & predation", icon = bs_icon("box-arrow-right"),
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
      population survives the seaward journey."))
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

  # ---------------- Guide / tutorial (mandatory walk-through) ----------------
  nav_panel(
    "Guide", icon = bs_icon("mortarboard"),
    card(card_header("How to use this dashboard"), markdown(
      "**1. Overview** — the headline. Five value boxes, then the *fate bar*: of every tagged fish,
       how many were never re-detected, stayed in-basin, left the Okanagan, died to predation, or
       came home.

       **2. Tributary timing** — tick creeks and drag the year slider; the plot shows when fish were
       first detected in each creek.

       **3. Presence** — a site × year heatmap; darker = more fish. Hover any cell.

       **4. Outmigration & predation** — when smolts headed downstream, and the recoveries that reveal
       their fate (mostly bird predation on the Columbia).

       **5. Fish explorer** — the heart of it. Filter by fate or pick a tag, and the app draws that
       one fish's whole journey. It opens on `3DD.003D53EC37`, the single confirmed return.

       **6. Data** — the full per-fish table, searchable and downloadable.")),
    card(card_header("Reading the fates"), DTOutput("tbl_guide")),
    card(card_header("The big caveat"), markdown(
      "Steelhead and rainbow trout are the **same species** (*O. mykiss*). PIT detections show
       *where and when* a fish was, never *which life history* it is. No fish here is labelled
       \"steelhead.\" Telling anadromous from resident needs scale ageing, otolith/fin-ray
       microchemistry, or genetics."))
  ),

  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", paste0("v", APP_VERSION))),
  nav_item(tags$a(bs_icon("github"), href = "https://github.com/MacusOng/steelhead-tracker",
                  target = "_blank"))
)

# =====================================================================
# SERVER
# =====================================================================
server <- function(input, output, session) {
  output$kpi_fish   <- renderText(format(nrow(pf), big.mark = ","))
  output$kpi_events <- renderText(format(nrow(ev), big.mark = ","))
  output$kpi_left   <- renderText(sum(pf$left_basin == "Y", na.rm = TRUE))
  output$kpi_pred   <- renderText(fs$n_fish[fs$fate == "recovered - predation island"])
  output$kpi_ret    <- renderText(fs$n_fish[fs$fate == "confirmed return (adult, natal)"])

  output$plot_fate <- renderPlotly({
    d <- fs |> mutate(fate = factor(fate, levels = rev(fs$fate)))
    p <- ggplot(d, aes(fate, n_fish, fill = fate, text = paste0(fate, ": ", n_fish))) +
      geom_col() + coord_flip() +
      scale_fill_manual(values = fate_cols, guide = "none") +
      scale_y_continuous(labels = comma) + labs(x = NULL, y = "Number of fish") + theme_okn()
    ggplotly(p, tooltip = "text") |> layout(showlegend = FALSE)
  })
  output$tbl_fate <- renderDT(
    datatable(fs, rownames = FALSE, options = list(dom = "t", pageLength = 10),
              colnames = c("Fate", "Fish", "Definition")))
  output$tbl_guide <- renderDT(
    datatable(fs, rownames = FALSE, options = list(dom = "t", pageLength = 10),
              colnames = c("Fate", "Fish", "Definition")))

  timing <- reactive({
    ev |> filter(event_type == "Observation", site_code %in% input$tim_creeks,
                 event_year >= input$tim_years[1], event_year <= input$tim_years[2]) |>
      group_by(tag_code, site_code) |> summarise(doy = min(doy), .groups = "drop") |>
      mutate(creek = creek_names[site_code])
  })
  output$plot_timing <- renderPlotly({
    d <- timing(); validate(need(nrow(d) > 0, "No detections for this selection."))
    creek_pal <- c("Inkaneep Creek" = pal$blue, "Vaseux Creek" = pal$orange,
                   "Shingle Creek" = pal$aqua, "Shuttleworth Creek" = "#8a5cd0")
    p <- ggplot(d, aes(doy, creek, color = creek, text = paste0(creek, " · day ", doy))) +
      geom_jitter(height = 0.18, alpha = 0.5, size = 1.6) +
      geom_boxplot(aes(fill = creek), alpha = 0.15, outlier.shape = NA, color = "grey40") +
      scale_color_manual(values = creek_pal, guide = "none") +
      scale_fill_manual(values = creek_pal, guide = "none") +
      scale_x_continuous(breaks = c(1,32,60,91,121,152,182,213,244,274,305,335),
                         labels = month.abb, limits = c(1, 366)) +
      labs(x = "Day of year (first detection)", y = NULL) + theme_okn()
    ggplotly(p, tooltip = "text")
  })

  output$plot_heat <- renderPlotly({
    d <- ev |> filter(event_type == "Observation") |>
      group_by(site_name, event_year) |> summarise(fish = n_distinct(tag_code), .groups = "drop")
    ord <- d |> group_by(site_name) |> summarise(t = sum(fish)) |> arrange(t) |> pull(site_name)
    d <- d |> mutate(site_name = factor(site_name, levels = ord))
    p <- ggplot(d, aes(factor(event_year), site_name, fill = fish,
                       text = paste0(site_name, " · ", event_year, ": ", fish, " fish"))) +
      geom_tile(color = "white") + geom_text(aes(label = fish), size = 3, color = "grey30") +
      scale_fill_viridis_c(option = "magma", direction = -1, name = "Fish") +
      labs(x = NULL, y = NULL) + theme_okn() + theme(legend.position = "right")
    ggplotly(p, tooltip = "text")
  })

  output$plot_out <- renderPlotly({
    d <- ev |> filter(site_code %in% down_sites) |>
      group_by(tag_code) |> summarise(dt = min(event_date), .groups = "drop") |>
      mutate(month = factor(format(dt, "%b"), levels = month.abb)) |>
      count(month, .drop = FALSE) |> filter(as.integer(month) %in% 3:8)
    p <- ggplot(d, aes(month, n, text = paste0(month, ": ", n))) +
      geom_col(fill = pal$blue) + geom_text(aes(label = n), vjust = -0.4, size = 4, color = pal$navy) +
      labs(x = NULL, y = "Fish") + theme_okn()
    ggplotly(p, tooltip = "text")
  })
  output$tbl_rec <- renderDT(
    datatable(rec, rownames = FALSE, options = list(dom = "t", pageLength = 10, scrollX = TRUE)))

  observe({
    tags <- if (input$fx_fate == "All") pf$tag_code else pf$tag_code[pf$fate == input$fx_fate]
    sel  <- if ("3DD.003D53EC37" %in% tags) "3DD.003D53EC37" else tags[1]
    updateSelectizeInput(session, "fx_tag", choices = tags, selected = sel, server = TRUE)
  })
  fx_row <- reactive({ req(input$fx_tag); pf[pf$tag_code == input$fx_tag, ][1, ] })
  fx_ev  <- reactive({ req(input$fx_tag); ev[ev$tag_code == input$fx_tag, ] |> arrange(event_date) })
  output$fx_release  <- renderText(paste0(fx_row()$release_site, " (", fx_row()$release_year, ")"))
  output$fx_fate_txt <- renderText(fx_row()$fate)
  output$fx_nevents  <- renderText(nrow(fx_ev()))
  output$fx_timeline <- renderPlotly({
    d <- fx_ev(); validate(need(nrow(d) > 0, "No events."))
    d <- d |> mutate(site_name = factor(site_name, levels = unique(site_name)))
    p <- ggplot(d, aes(event_date, site_name, color = event_type,
                       text = paste0(event_type, " · ", site_name, " · ", event_date))) +
      geom_line(aes(group = 1), color = "grey70") + geom_point(size = 3) +
      scale_color_manual(values = c(Mark = pal$grey, Observation = pal$blue,
                                    Recapture = pal$orange, Recovery = pal$red), name = NULL) +
      labs(x = NULL, y = NULL) + theme_okn()
    ggplotly(p, tooltip = "text")
  })
  output$fx_events <- renderDT(
    datatable(fx_ev() |> select(event_date, event_type, site_name, zone), rownames = FALSE,
              options = list(dom = "tp", pageLength = 15),
              colnames = c("Date", "Event", "Site", "Zone")))

  output$tbl_perfish <- renderDT(
    datatable(pf, rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE)))
  output$dl_perfish <- downloadHandler(
    filename = function() "okanagan_steelhead_per_fish.csv",
    content  = function(file) write_csv(pf, file))
}

shinyApp(ui, server)
