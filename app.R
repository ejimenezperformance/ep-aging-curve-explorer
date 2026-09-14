

# =============================================================================
# EP Aging Curve Explorer
# Emerson Performance — R / Shiny portfolio piece
#
# WHAT THIS DOES
#   Builds a population-level hitter aging curve using the classic "delta
#   method" (year-over-year change among players who logged qualifying
#   playing time in both seasons of a pair), then overlays any individual
#   player's own career trajectory against that population baseline.
#
#   This mirrors the EP methodology used across the Python repos:
#     - CONFIRMED vs. INTERPRETATION labeling on every output
#     - explicit assumptions/limitations stated in-app, not buried in a README
#     - a reusable, parameterized pipeline rather than a one-off script
#
# DATA
#   Lahman::Batting + Lahman::People (public-domain historical MLB data,
#   Sean Lahman's database, distributed via the "Lahman" CRAN package).
#
# HOW TO RUN
#   install.packages(c("shiny", "Lahman", "dplyr", "tidyr", "ggplot2", "DT"))
#   shiny::runApp("app.R")
#
# METHOD NOTE (delta method for aging curves)
#   For a given metric M and two consecutive seasons (age a, age a+1) for the
#   same player, the delta is M(a+1) - M(a). Averaging that delta across all
#   qualifying players at each age transition, then taking a cumulative sum
#   from an arbitrary anchor age, produces a survivorship-bias-resistant
#   population aging curve (Tango, 2006 delta method; standard sabermetric
#   technique, not proprietary to any org or vendor).
# =============================================================================

library(shiny)
library(Lahman)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)

# ---- EP brand -----------------------------------------------------------
ep_navy  <- "#0B1B33"
ep_gold  <- "#D4A53A"
ep_off   <- "#F5F3EC"
ep_gray  <- "#5B6472"

ep_theme <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = ep_off, color = NA),
      panel.background = element_rect(fill = ep_off, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E3DDC9", linewidth = 0.3),
      plot.title    = element_text(color = ep_navy, face = "bold", size = 15),
      plot.subtitle = element_text(color = ep_gray, size = 10),
      axis.title    = element_text(color = ep_navy, size = 10),
      axis.text     = element_text(color = ep_gray),
      legend.position = "bottom",
      legend.title  = element_text(color = ep_navy)
    )
}

# ---- Data prep ------------------------------------------------------------
# Season-level rate stats, joined to birth year for an approximate age
# (year - birthYear; ignores exact birthdate-vs-season-start, which is noted
# in-app as a stated limitation rather than silently absorbed into the model).
batting_seasons <- Batting %>%
  filter(!is.na(AB), AB > 0) %>%
  group_by(playerID, yearID) %>%
  summarise(
    PA  = sum(AB + coalesce(BB, 0) + coalesce(HBP, 0) + coalesce(SH, 0) + coalesce(SF, 0), na.rm = TRUE),
    AB  = sum(AB, na.rm = TRUE),
    H   = sum(H,  na.rm = TRUE),
    X2B = sum(X2B, na.rm = TRUE),
    X3B = sum(X3B, na.rm = TRUE),
    HR  = sum(HR,  na.rm = TRUE),
    BB  = sum(coalesce(BB, 0), na.rm = TRUE),
    SO  = sum(coalesce(SO, 0), na.rm = TRUE),
    HBP = sum(coalesce(HBP, 0), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    OBP = (H + BB + HBP) / pmax(AB + BB + HBP, 1),
    SLG = (H + X2B + 2 * X3B + 3 * HR) / pmax(AB, 1),
    OPS = OBP + SLG,
    ISO = SLG - (H / pmax(AB, 1)),
    `K%`  = SO / pmax(PA, 1),
    `BB%` = BB / pmax(PA, 1)
  ) %>%
  inner_join(People %>% select(playerID, birthYear, nameFirst, nameLast), by = "playerID") %>%
  mutate(
    age = yearID - birthYear,
    player_name = paste(nameFirst, nameLast)
  ) %>%
  filter(!is.na(age), age >= 18, age <= 45)

metric_choices <- c("OPS", "SLG", "OBP", "ISO", "K%", "BB%")

# ---- Delta-method population aging curve -----------------------------------
build_aging_curve <- function(df, metric, min_pa, year_lo, year_hi) {
  d <- df %>%
    filter(PA >= min_pa, yearID >= year_lo, yearID <= year_hi) %>%
    select(playerID, yearID, age, value = all_of(metric)) %>%
    arrange(playerID, yearID)

  paired <- d %>%
    group_by(playerID) %>%
    mutate(next_age = lead(age), next_value = lead(value), next_year = lead(yearID)) %>%
    ungroup() %>%
    filter(!is.na(next_age), next_age == age + 1, next_year == yearID + 1) %>%
    mutate(delta = next_value - value, age_pair = age)

  deltas <- paired %>%
    group_by(age_pair) %>%
    summarise(mean_delta = mean(delta, na.rm = TRUE), n_players = n(), .groups = "drop") %>%
    arrange(age_pair)

  if (nrow(deltas) == 0) return(tibble(age = numeric(), curve = numeric(), n_players = integer()))

  anchor <- deltas$age_pair[which.max(deltas$n_players)]
  ages_present <- sort(unique(c(deltas$age_pair, deltas$age_pair + 1)))

  curve <- tibble(age = ages_present, curve = NA_real_)
  curve$curve[curve$age == anchor] <- 0

  # walk forward from anchor
  for (a in ages_present[ages_present > anchor]) {
    prev_val <- curve$curve[curve$age == a - 1]
    step <- deltas$mean_delta[deltas$age_pair == a - 1]
    curve$curve[curve$age == a] <- if (length(step) == 1 && !is.na(prev_val)) prev_val + step else NA_real_
  }
  # walk backward from anchor
  for (a in rev(ages_present[ages_present < anchor])) {
    next_val <- curve$curve[curve$age == a + 1]
    step <- deltas$mean_delta[deltas$age_pair == a]
    curve$curve[curve$age == a] <- if (length(step) == 1 && !is.na(next_val)) next_val - step else NA_real_
  }

  curve %>%
    left_join(deltas %>% transmute(age = age_pair, n_players), by = "age") %>%
    filter(!is.na(curve))
}

# ---- UI ----------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(sprintf("
    body { background:%s; font-family:-apple-system,Segoe UI,Roboto,sans-serif; }
    .well { background:#FFFFFF; border:none; border-top:3px solid %s; }
    h2.ep-title { color:%s; font-weight:700; }
    .ep-sub { color:%s; font-size:13px; margin-bottom:18px; }
    .ep-kicker { color:%s; letter-spacing:2px; font-size:11px; font-weight:700; text-transform:uppercase; }
    .ep-note { background:%s; border-left:4px solid %s; padding:10px 14px; font-size:12.5px; color:%s; margin-top:14px;}
  ", ep_off, ep_gold, ep_navy, ep_gray, ep_gold, ep_off, ep_gold, ep_navy)))),

  div(style = "padding:18px 6px;",
    div(class = "ep-kicker", "Emerson Performance"),
    h2(class = "ep-title", "Aging Curve Explorer"),
    div(class = "ep-sub", "Population-level hitter aging curves (delta method) with individual player overlay — Lahman database.")
  ),

  sidebarLayout(
    sidebarPanel(
      selectInput("metric", "Metric", choices = metric_choices, selected = "OPS"),
      sliderInput("min_pa", "Minimum PA per qualifying season", min = 100, max = 500, value = 300, step = 25),
      sliderInput("year_range", "Seasons included", min = 1970, max = 2023, value = c(1990, 2023), sep = ""),
      selectizeInput("player", "Overlay a player (optional)", choices = NULL,
                      options = list(placeholder = "Start typing a last name...", maxOptions = 20)),
      div(class = "ep-note",
          strong("Method note (CONFIRMED): "),
          "population curve built with the delta method — mean year-over-year change among players qualifying in both consecutive seasons, cumulative-summed from the best-supported age pair.",
          br(), br(),
          strong("Limitation (stated, not hidden): "),
          "age is computed as season year minus birth year, not exact age at midseason. This under/over-states age by up to ~1 year depending on birth month."
      )
    ),
    mainPanel(
      plotOutput("agingPlot", height = "420px"),
      br(),
      DTOutput("deltaTable")
    )
  )
)

# ---- Server ----------------------------------------------------------------
server <- function(input, output, session) {

  # Exclude pitchers so the search only surfaces position players. This is a
  # simplification (it also drops rare two-way players like Ohtani) — stated
  # here rather than silently baked in.
  pitcher_ids <- unique(Pitching$playerID)

  updateSelectizeInput(session, "player",
    choices = batting_seasons %>%
      filter(!playerID %in% pitcher_ids) %>%
      distinct(playerID, player_name) %>% arrange(player_name) %>%
      { setNames(.$playerID, .$player_name) },
    server = TRUE)

  curve_data <- reactive({
    build_aging_curve(batting_seasons, input$metric, input$min_pa, input$year_range[1], input$year_range[2])
  })

  player_data <- reactive({
    req(input$player)
    batting_seasons %>%
      filter(playerID == input$player, PA >= 100) %>%
      transmute(age, value = .data[[input$metric]]) %>%
      arrange(age)
  })

  output$agingPlot <- renderPlot({
    cd <- curve_data()
    validate(need(nrow(cd) > 0, "No qualifying player-seasons for this filter combination — widen the PA or year range."))

    p <- ggplot(cd, aes(x = age, y = curve)) +
      geom_ribbon(aes(ymin = curve - 0.01, ymax = curve + 0.01), fill = ep_gold, alpha = 0.12) +
      geom_line(color = ep_navy, linewidth = 1.1) +
      geom_point(color = ep_navy, size = 1.6) +
      labs(
        title = paste("Population aging curve —", input$metric),
        subtitle = sprintf("Delta method · min %d PA/season · %d–%d", input$min_pa, input$year_range[1], input$year_range[2]),
        x = "Age", y = paste(input$metric, "(relative to anchor age)")
      ) + ep_theme()

    if (!is.null(input$player) && input$player != "") {
      pd <- player_data()
      if (nrow(pd) > 0) {
        anchor_age <- cd$age[which.max(cd$n_players)]
        anchor_val <- pd$value[pd$age == anchor_age]
        offset <- if (length(anchor_val) == 1) anchor_val else mean(pd$value, na.rm = TRUE)
        pd$centered <- pd$value - offset
        p <- p + geom_line(data = pd, aes(x = age, y = centered), color = ep_gold, linewidth = 1.1, inherit.aes = FALSE) +
                 geom_point(data = pd, aes(x = age, y = centered), color = ep_gold, size = 2, inherit.aes = FALSE)
      }
    }
    p
  })

  output$deltaTable <- renderDT({
    cd <- curve_data() %>% transmute(Age = age, `Curve value` = round(curve, 4), `Players (n)` = n_players)
    datatable(cd, options = list(pageLength = 8, dom = "tp"), rownames = FALSE)
  })
}

shinyApp(ui, server)
