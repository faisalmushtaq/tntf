# app.R -- Tuesday Night Football analytics dashboard -------------------------
#
# Run with: shiny::runApp()
#
# This is the public, read-only app: it is deployed as a static shinylive
# site, so nothing viewers do can change the shared data. New team sheets
# are added by committing to data-raw/team_sheets_raw.txt on GitHub, which
# rebuilds and redeploys the site automatically.

source("global.R")

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- page_navbar(
  title = tags$span(tags$strong("TNF"), " Analytics"),
  theme = tnf_theme,
  fillable = FALSE,
  header = tags$head(
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1, maximum-scale=1"),
    tags$link(rel = "stylesheet", href = "styles.css")
  ),

  # -- Dashboard --------------------------------------------------------------
  nav_panel(
    "Dashboard", icon = icon("futbol"),
    layout_columns(
      fill = FALSE,
      value_box("Matches", textOutput("vb_matches"), showcase = icon("calendar"),
                theme = "primary"),
      value_box("Players", textOutput("vb_players"), showcase = icon("users"),
                theme = "secondary"),
      value_box("Goals / match", textOutput("vb_goals"), showcase = icon("bullseye")),
      value_box("Bibs win rate", textOutput("vb_bibs"), showcase = icon("shirt"),
                theme = "warning"),
      value_box("Usual format", textOutput("vb_format"), showcase = icon("user-check"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Every result"), plotOutput("dash_results", height = 340)),
      card(card_header("Match format"), plotOutput("dash_format", height = 340))
    ),
    card(card_header("Highlighted Stats"),
         uiOutput("dash_facts"))
  ),

  # -- Matches ----------------------------------------------------------------
  nav_panel(
    "Matches", icon = icon("table-list"),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("All matches"), DTOutput("match_table")),
      card(card_header("Match detail"),
           p(class = "text-muted", "Select a match on the left."),
           uiOutput("match_detail"))
    )
  ),

  # -- Players ----------------------------------------------------------------
  nav_panel(
    "Players", icon = icon("user"),
    layout_sidebar(
      sidebar = sidebar(
        selectizeInput("player", "Search a player", choices = NULL,
                       options = list(placeholder = "Type a name..."))
      ),
      uiOutput("player_boxes"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Results timeline"),
             plotOutput("player_timeline", height = 260)),
        card(card_header("Appearances by month"),
             plotOutput("player_months", height = 260))
      ),
      card(card_header("All players, every metric"),
           DTOutput("player_stats_table"))
    )
  ),

  # -- Compare ----------------------------------------------------------------
  nav_panel(
    "Compare", icon = icon("scale-balanced"),
    layout_columns(
      fill = FALSE, col_widths = c(4, 4, 4),
      selectizeInput("cmp_a", "Player A", choices = NULL),
      selectizeInput("cmp_b", "Player B", choices = NULL),
      div()
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Side by side"), tableOutput("cmp_table")),
      card(card_header("As teammates and as rivals"), uiOutput("cmp_h2h"))
    )
  ),

  # -- Partnerships -----------------------------------------------------------
  nav_panel(
    "Partnerships", icon = icon("people-group"),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("pair_min", "Minimum matches together", 2, 12, 5, step = 1),
        selectInput("combo_size", "Combination size",
                    c("Pairs" = 2, "Trios" = 3, "Quartets" = 4, "Quintets" = 5))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Best partnerships"),
             plotOutput("pair_plot", height = 380)),
        card(card_header("Who plays with whom"),
             plotOutput("cooc_plot", height = 420))
      ),
      card(card_header("Combination records"), DTOutput("combo_table"))
    )
  ),

  # -- Head-to-head ------------------------------------------------------------
  nav_panel(
    "Head-to-head", icon = icon("hand-fist"),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("h2h_min", "Minimum meetings", 2, 12, 4, step = 1),
        p(class = "text-muted small",
          "Win rate = how often the row player's team beats the column player's team when they are on opposite sides.")
      ),
      card(card_header("Who beats whom"),
           plotOutput("h2h_plot", height = 520)),
      card(card_header("All head-to-head records"), DTOutput("h2h_table"))
    )
  ),

  # -- Network -----------------------------------------------------------------
  nav_panel(
    "Network", icon = icon("circle-nodes"),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("net_min", "Minimum matches together (edges)", 1, 8, 3,
                    step = 1),
        p(class = "text-muted small",
          "Each circle is a player (bigger = more appearances). Lines join players who share teams: thicker = more matches together, green = they win together, red = they lose together. Pinch to zoom, tap a circle for details."),
        tags$details(
          tags$summary("What do the measures mean?"),
          tags$ul(class = "small text-muted",
            tags$li(tags$b("Degree"), " (how many different teammates)"),
            tags$li(tags$b("Strength"), " (total matches shared with teammates)"),
            tags$li(tags$b("Betweenness"), " (a 'bridge' score: how often the player links otherwise separate groups)"),
            tags$li(tags$b("Closeness"), " (how few steps it takes to reach everyone else)"),
            tags$li(tags$b("PageRank"), " (overall influence: playing often with other well-connected players — Google's famous algorithm)"),
            tags$li(tags$b("Community"), " (a cluster of players who tend to appear together)")
          )
        )
      ),
      card(card_header("Interactive network"),
           plotlyOutput("net_plot", height = "65vh")),
      card(card_header("Centrality measures (who matters in the network)"),
           DTOutput("net_table"))
    )
  ),

  # -- Trends ------------------------------------------------------------------
  nav_panel(
    "Trends", icon = icon("chart-line"),
    layout_columns(
      col_widths = c(6, 6, 12),
      card(card_header("Momentum (rolling win %)"),
           plotOutput("trend_rolling", height = 320)),
      card(card_header("Bib dominance"),
           plotOutput("trend_dominance", height = 320)),
      card(card_header("Cumulative appearances"),
           plotOutput("trend_cumapps", height = 380))
    )
  ),

  nav_panel(
    "About", icon = icon("circle-info"),
    card(
      card_header("About this platform"),
      markdown("
**Tuesday Night Football Analytics** ingests messy WhatsApp-style team
sheets, standardises player names through an alias table, stores everything
in a tidy relational database and serves statistics and network analysis
through this dashboard.

**This app is read-only.** It runs entirely in your browser, so nothing you
click can change the shared record. New team sheets are added by the
maintainer through the GitHub repository; the site rebuilds and updates
automatically within a couple of minutes.

Key assumptions:

* the number in brackets after a team name is that team's **goals**;
* nights are categorised as 5-, 7- or 8-a-side from the number of players
  listed;
* ambiguous first names are resolved by elimination within a match;
* win percentages carry 95% confidence intervals (a range the true rate
  plausibly lies in) — small samples are flagged.

Glossary of network measures:

* **Degree** — how many different teammates a player has had.
* **Strength** — total matches shared with all teammates combined.
* **Betweenness** — how often a player is the 'bridge' connecting groups
  that otherwise wouldn't mix.
* **Closeness** — how few steps it takes to reach every other player
  through shared matches.
* **PageRank** — overall influence: playing often alongside other
  well-connected players (the algorithm Google made famous).
* **Community** — a cluster of players the algorithm groups together
  because they so often share a team.
")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  db <- reactiveVal(initial_db)

  # keep player pickers in sync with the database
  observe({
    nms <- db()$players |> filter(!is_guest) |> pull(player_name)
    updateSelectizeInput(session, "player", choices = nms, selected = isolate(
      if (isTruthy(input$player) && input$player %in% nms) input$player else nms[1]))
    updateSelectizeInput(session, "cmp_a", choices = nms,
                         selected = isolate(input$cmp_a %||% nms[1]))
    updateSelectizeInput(session, "cmp_b", choices = nms,
                         selected = isolate(input$cmp_b %||% nms[2]))
  })

  # -- dashboard ---------------------------------------------------------------
  output$vb_matches <- renderText(as.character(match_summary(db())$total_matches))
  output$vb_players <- renderText(as.character(match_summary(db())$total_players))
  output$vb_goals <- renderText(sprintf("%.1f", match_summary(db())$goals_per_match))
  output$vb_bibs <- renderText({
    tr <- team_record(db()); fmt_pct(tr$win_pct[tr$team == "Bibs"])
  })
  output$vb_format <- renderText({
    f <- match_format(db()$matches$attendance)
    names(sort(table(f), decreasing = TRUE))[1]
  })
  output$dash_results <- renderPlot(plot_results_timeline(db()))
  output$dash_format <- renderPlot(plot_format_over_time(db()))
  output$dash_facts <- renderUI({
    tags$ul(lapply(generate_discoveries(db()), tags$li))
  })

  # -- matches ----------------------------------------------------------------
  output$match_table <- renderDT({
    match_results_table(db()) |>
      mutate(format = as.character(match_format(attendance)),
             date = format(date, "%d %b %Y")) |>
      select(date, score, winner, margin, format, note) |>
      datatable(selection = "single", rownames = FALSE,
                options = list(pageLength = 25, dom = "ft", scrollX = TRUE))
  })
  output$match_detail <- renderUI({
    idx <- input$match_table_rows_selected
    req(idx)
    m <- db()$matches |> arrange(date) |> slice(idx)
    sheets <- db()$appearances |>
      filter(match_id == m$match_id) |>
      inner_join(db()$players, by = "player_id") |>
      arrange(team, player_name)
    ev <- db()$events |> filter(match_id == m$match_id)
    tagList(
      h4(sprintf("%s — Bibs %d : %d Non-Bibs", format(m$date, "%d %B %Y"),
                 m$bibs_goals, m$nonbibs_goals)),
      p(class = "text-muted", as.character(match_format(m$attendance))),
      if (!is.na(m$note)) p(tags$em(paste("Note:", m$note))),
      layout_columns(
        col_widths = c(6, 6),
        tagList(h5("Bibs"),
                tags$ul(lapply(sheets$player_name[sheets$team == "Bibs"], tags$li))),
        tagList(h5("Non-Bibs"),
                tags$ul(lapply(sheets$player_name[sheets$team == "Non-Bibs"], tags$li)))
      ),
      if (nrow(ev) > 0) tagList(h5("Events"),
                                tags$ul(lapply(ev$detail, function(d)
                                  tags$li(paste(ev$event_type[ev$detail == d][1], "—", d)))))
    )
  })

  # -- players ----------------------------------------------------------------
  output$player_boxes <- renderUI({
    req(input$player)
    s <- player_stats(db()) |> filter(player_name == input$player)
    req(nrow(s) == 1)
    layout_columns(
      fill = FALSE,
      value_box("Appearances", s$appearances, theme = "primary"),
      value_box("Record", sprintf("%dW-%dD-%dL", s$wins, s$draws, s$losses)),
      value_box("Win %", paste0(fmt_pct(s$win_pct), if (s$small_sample) " *"),
                p(paste0("95% CI ", fmt_pct(s$lower), "-", fmt_pct(s$upper))),
                theme = "warning"),
      value_box("Goal difference", sprintf("%+d", s$goal_diff)),
      value_box("Current streak", s$current_streak,
                p(paste0("Best win run: ", s$longest_win_streak)))
    )
  })
  output$player_timeline <- renderPlot({
    req(input$player)
    d <- player_match_results(db()) |> filter(player_name == input$player,
                                              !is.na(outcome))
    ggplot(d, aes(date, 1, colour = outcome)) +
      geom_point(size = 6) +
      scale_colour_manual(values = c(W = "#2E8B57", D = "#8C8C8C",
                                     L = "#C7361F")) +
      labs(x = NULL, y = NULL) + theme_tnf() +
      theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
            axis.line.y = element_blank(), panel.grid = element_blank())
  })
  output$player_months <- renderPlot({
    req(input$player)
    player_appearances_by_month(db()) |>
      filter(player_name == input$player) |>
      ggplot(aes(month, appearances)) +
      geom_col(fill = "#F2A900", width = 20) +
      labs(x = NULL, y = "Appearances") + theme_tnf()
  })
  output$player_stats_table <- renderDT({
    player_stats(db()) |>
      filter(!is_guest) |>
      transmute(Player = player_name, Apps = appearances, W = wins, D = draws,
                L = losses, `Win %` = round(100 * win_pct),
                `CI low` = round(100 * lower), `CI high` = round(100 * upper),
                GF = goals_for, GA = goals_against, GD = goal_diff,
                `Best run` = longest_win_streak, `Worst run` = longest_losing_streak,
                Streak = current_streak,
                `Consistency %` = round(100 * attendance_consistency),
                First = format(first_appearance, "%d %b %y"),
                Latest = format(last_appearance, "%d %b %y")) |>
      datatable(rownames = FALSE,
                options = list(pageLength = 35, dom = "ft", scrollX = TRUE))
  })

  # -- compare ----------------------------------------------------------------
  output$cmp_table <- renderTable({
    req(input$cmp_a, input$cmp_b, input$cmp_a != input$cmp_b)
    s <- player_stats(db()) |> filter(player_name %in% c(input$cmp_a, input$cmp_b))
    req(nrow(s) == 2)
    s <- s[match(c(input$cmp_a, input$cmp_b), s$player_name), ]
    tibble(
      Metric = c("Appearances", "Wins", "Draws", "Losses", "Win %",
                 "Goals for", "Goals against", "Goal difference",
                 "Longest win streak", "Longest losing streak",
                 "Current streak", "Attendance consistency",
                 "First appearance", "Latest appearance"),
      !!input$cmp_a := c(s$appearances[1], s$wins[1], s$draws[1], s$losses[1],
                         fmt_pct(s$win_pct[1]), s$goals_for[1], s$goals_against[1],
                         s$goal_diff[1], s$longest_win_streak[1],
                         s$longest_losing_streak[1], s$current_streak[1],
                         fmt_pct(s$attendance_consistency[1]),
                         format(s$first_appearance[1], "%d %b %y"),
                         format(s$last_appearance[1], "%d %b %y")),
      !!input$cmp_b := c(s$appearances[2], s$wins[2], s$draws[2], s$losses[2],
                         fmt_pct(s$win_pct[2]), s$goals_for[2], s$goals_against[2],
                         s$goal_diff[2], s$longest_win_streak[2],
                         s$longest_losing_streak[2], s$current_streak[2],
                         fmt_pct(s$attendance_consistency[2]),
                         format(s$first_appearance[2], "%d %b %y"),
                         format(s$last_appearance[2], "%d %b %y"))
    )
  }, striped = TRUE, width = "100%")

  output$cmp_h2h <- renderUI({
    req(input$cmp_a, input$cmp_b, input$cmp_a != input$cmp_b)
    together <- pair_stats(db()) |>
      filter((player_a == input$cmp_a & player_b == input$cmp_b) |
             (player_a == input$cmp_b & player_b == input$cmp_a))
    against <- head_to_head(db()) |>
      filter(player == input$cmp_a, opponent == input$cmp_b)
    tagList(
      h5("Same team"),
      if (nrow(together) == 1)
        p(sprintf("Teammates %d times: %dW-%dD-%dL (%s win rate, GD %+d).",
                  together$together, together$wins, together$draws,
                  together$losses, fmt_pct(together$win_pct),
                  together$goal_diff))
      else p("Never on the same team."),
      h5("Opposite teams"),
      if (nrow(against) == 1)
        p(sprintf("When %s faces %s, %s's team wins %s of %d meetings (%dW-%dD-%dL).",
                  input$cmp_a, input$cmp_b, input$cmp_a,
                  fmt_pct(against$win_pct), against$meetings, against$wins,
                  against$draws, against$losses))
      else p("Never faced each other.")
    )
  })

  # -- partnerships ------------------------------------------------------------
  output$pair_plot <- renderPlot(plot_partnerships(db(), input$pair_min))
  output$cooc_plot <- renderPlot(plot_co_occurrence(db()))
  output$combo_table <- renderDT({
    size <- as.integer(input$combo_size)
    d <- if (size == 2) {
      pair_stats(db(), input$pair_min) |>
        mutate(combo = paste(player_a, "+", player_b)) |>
        select(combo, together, wins, draws, losses, win_pct)
    } else {
      combo_stats(db(), size, max(2, input$pair_min - 2))
    }
    d |>
      mutate(win_pct = round(100 * win_pct)) |>
      rename(`Win %` = win_pct) |>
      datatable(rownames = FALSE,
                options = list(pageLength = 15, dom = "ft", scrollX = TRUE))
  })

  # -- head-to-head -------------------------------------------------------------
  output$h2h_plot <- renderPlot(plot_h2h_heatmap(db(), min_meetings = input$h2h_min))
  output$h2h_table <- renderDT({
    head_to_head(db(), input$h2h_min) |>
      mutate(win_pct = round(100 * win_pct),
             ci = paste0(round(100 * lower), "-", round(100 * upper), "%")) |>
      transmute(Player = player, Opponent = opponent, Meetings = meetings,
                W = wins, D = draws, L = losses, `Win %` = win_pct,
                `95% CI (plausible range)` = ci,
                `GD (goal difference)` = goal_diff) |>
      datatable(rownames = FALSE,
                options = list(pageLength = 15, dom = "ft", scrollX = TRUE))
  })

  # -- network ------------------------------------------------------------------
  output$net_plot <- renderPlotly(plotly_network(db(), input$net_min))
  output$net_table <- renderDT({
    network_metrics(db(), min_together = input$net_min) |>
      transmute(
        Player = name,
        Apps = appearances,
        `Degree (teammates)` = degree,
        `Strength (shared matches)` = strength,
        `Betweenness (bridge score)` = round(betweenness, 1),
        `Closeness (steps to everyone)` = round(closeness, 3),
        `PageRank (influence)` = round(pagerank, 4),
        `Community (cluster)` = community
      ) |>
      datatable(rownames = FALSE,
                options = list(pageLength = 15, dom = "ft", scrollX = TRUE))
  })

  # -- trends -------------------------------------------------------------------
  output$trend_rolling <- renderPlot(plot_rolling_winpct(db()))
  output$trend_dominance <- renderPlot(plot_bib_dominance(db()))
  output$trend_cumapps <- renderPlot(plot_cumulative_appearances(db()))
}

shinyApp(ui, server)
