# app.R -- Tuesday Night Football analytics dashboard -------------------------
#
# Run with: shiny::runApp()
# See README.md for the full feature tour.

source("global.R")

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- page_navbar(
  title = tags$span(tags$strong("TNF"), " Analytics"),
  theme = tnf_theme,
  fillable = FALSE,
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

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
      value_box("Avg attendance", textOutput("vb_att"), showcase = icon("user-check"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Every result"), plotOutput("dash_results", height = 340)),
      card(card_header("Attendance"), plotOutput("dash_attendance", height = 340))
    ),
    card(card_header("Automatically discovered facts"),
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
                       options = list(placeholder = "Type a name...")),
        hr(),
        downloadButton("dl_player_stats", "Download player table (csv)",
                       class = "btn-sm")
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
                    c("Pairs" = 2, "Trios" = 3, "Quartets" = 4, "Quintets" = 5)),
        downloadButton("dl_pairs", "Download pairs (csv)", class = "btn-sm")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Best partnerships"),
             plotOutput("pair_plot", height = 380)),
        card(card_header("Who plays with whom"),
             plotOutput("cooc_plot", height = 380))
      ),
      card(card_header("Combination records"), DTOutput("combo_table"))
    )
  ),

  # -- Head-to-head ------------------------------------------------------------
  nav_panel(
    "Head-to-head", icon = icon("hand-fist"),
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("h2h_min", "Minimum meetings", 2, 12, 4, step = 1)
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
          "Node size = appearances; hover for details. Edge colour: green wins together, red loses together.")
      ),
      card(card_header("Interactive network"),
           plotlyOutput("net_plot", height = 560)),
      card(card_header("Centrality measures"), DTOutput("net_table"))
    )
  ),

  # -- Trends ------------------------------------------------------------------
  nav_panel(
    "Trends", icon = icon("chart-line"),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Momentum (rolling win %)"),
           plotOutput("trend_rolling", height = 320)),
      card(card_header("Bib dominance"),
           plotOutput("trend_dominance", height = 320)),
      card(card_header("Cumulative appearances"),
           plotOutput("trend_cumapps", height = 340)),
      card(card_header("The Tuesday calendar"),
           plotOutput("trend_calendar", height = 340))
    )
  ),

  # -- Data & upload -----------------------------------------------------------
  nav_panel(
    "Data", icon = icon("database"),
    navset_card_tab(
      nav_panel(
        "Upload team sheets",
        p("Paste a new team sheet below (same messy format as ever), or upload a text file. ",
          "Preview the parse, then commit it to the database."),
        layout_columns(
          col_widths = c(6, 6),
          textAreaInput("upload_text", NULL, rows = 12,
                        placeholder = "14/7/26\n\nBibs (5)\n- Faisal\n- Suki\n...\n\nNon-Bibs (4)\n- Lee\n..."),
          fileInput("upload_file", "...or upload a .txt file", accept = ".txt")
        ),
        actionButton("preview_btn", "Preview parse", class = "btn-primary"),
        actionButton("commit_btn", "Commit to database", class = "btn-warning"),
        br(), br(),
        uiOutput("upload_summary"),
        DTOutput("upload_issues")
      ),
      nav_panel(
        "Alias editor",
        p("Aliases map messy raw names onto canonical players. Add a new alias and save; the database rebuilds automatically."),
        layout_columns(
          fill = FALSE, col_widths = c(3, 3, 2, 2, 2),
          textInput("alias_new", "Alias (as written)"),
          textInput("alias_canonical", "Canonical player"),
          checkboxInput("alias_ambiguous", "Ambiguous?", FALSE),
          textInput("alias_note", "Note"),
          actionButton("alias_add", "Add & rebuild", class = "btn-primary",
                       style = "margin-top: 32px;")
        ),
        DTOutput("alias_table")
      ),
      nav_panel(
        "Parse issues",
        p("Everything the parser and alias resolver want a human to check."),
        DTOutput("issues_table")
      ),
      nav_panel(
        "Downloads",
        p("Download any table as csv, or regenerate the full PDF report."),
        selectInput("dl_table_pick", "Table",
                    c("matches", "teams", "players", "appearances", "events",
                      "player_aliases", "parse_issues")),
        downloadButton("dl_table", "Download table (csv)"),
        hr(),
        selectInput("dl_fig_pick", "Figure", character(0)),
        downloadButton("dl_figure", "Download figure (png)"),
        p(class = "text-muted small",
          "Every figure from the analytics suite, rendered at print quality.")
      )
    )
  ),

  nav_panel(
    "About", icon = icon("circle-info"),
    card(
      card_header("About this platform"),
      markdown("
**Tuesday Night Football Analytics** ingests messy WhatsApp-style team
sheets, standardises player names through an alias table, stores everything
in a tidy relational database and serves statistics, network analysis and a
PDF report.

Key assumptions:

* the number in brackets after a team name is that team's **goals**;
* dates are day/month/year (two-digit years = 20xx);
* ambiguous first names are resolved by elimination within a match and
  logged in the parse issues;
* win percentages carry 95% Wilson confidence intervals; small samples are
  flagged.

Source: the project README and `docs/assumptions.md` in the repository.
")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  db <- reactiveVal(initial_db)
  upload_preview <- reactiveVal(NULL)

  # keep player pickers in sync with the database
  observe({
    nms <- db()$players |> filter(!is_guest) |> pull(player_name)
    updateSelectizeInput(session, "player", choices = nms, selected = isolate(
      if (isTruthy(input$player) && input$player %in% nms) input$player else nms[1]))
    updateSelectizeInput(session, "cmp_a", choices = nms,
                         selected = isolate(input$cmp_a %||% nms[1]))
    updateSelectizeInput(session, "cmp_b", choices = nms,
                         selected = isolate(input$cmp_b %||% nms[2]))
    updateSelectInput(session, "dl_fig_pick", choices = names(all_figures(db())))
  })

  # -- dashboard ---------------------------------------------------------------
  output$vb_matches <- renderText(as.character(match_summary(db())$total_matches))
  output$vb_players <- renderText(as.character(match_summary(db())$total_players))
  output$vb_goals <- renderText(sprintf("%.1f", match_summary(db())$goals_per_match))
  output$vb_bibs <- renderText({
    tr <- team_record(db()); fmt_pct(tr$win_pct[tr$team == "Bibs"])
  })
  output$vb_att <- renderText(sprintf("%.1f", match_summary(db())$mean_attendance))
  output$dash_results <- renderPlot(plot_results_timeline(db()))
  output$dash_attendance <- renderPlot(plot_attendance_over_time(db()))
  output$dash_facts <- renderUI({
    tags$ul(lapply(generate_discoveries(db()), tags$li))
  })

  # -- matches ----------------------------------------------------------------
  output$match_table <- renderDT({
    match_results_table(db()) |>
      mutate(date = format(date, "%d %b %Y")) |>
      datatable(selection = "single", rownames = FALSE,
                options = list(pageLength = 25, dom = "ft"))
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
      datatable(rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = 35, dom = "Bft", scrollX = TRUE,
                               buttons = c("copy", "csv")))
  })
  output$dl_player_stats <- downloadHandler(
    filename = function() "tnf_player_stats.csv",
    content = function(file) readr::write_csv(player_stats(db()), file)
  )

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
      datatable(rownames = FALSE, options = list(pageLength = 15, dom = "ft"))
  })
  output$dl_pairs <- downloadHandler(
    filename = function() "tnf_pairs.csv",
    content = function(file) readr::write_csv(pair_stats(db()), file)
  )

  # -- head-to-head -------------------------------------------------------------
  output$h2h_plot <- renderPlot(plot_h2h_heatmap(db(), min_meetings = input$h2h_min))
  output$h2h_table <- renderDT({
    head_to_head(db(), input$h2h_min) |>
      mutate(win_pct = round(100 * win_pct),
             ci = paste0(round(100 * lower), "-", round(100 * upper), "%")) |>
      select(player, opponent, meetings, wins, draws, losses, win_pct, ci,
             goal_diff) |>
      datatable(rownames = FALSE, options = list(pageLength = 15, dom = "ft"))
  })

  # -- network ------------------------------------------------------------------
  output$net_plot <- renderPlotly(plotly_network(db(), input$net_min))
  output$net_table <- renderDT({
    network_metrics(db(), min_together = input$net_min) |>
      mutate(across(c(betweenness, closeness, pagerank, eigen, win_pct),
                    ~ round(.x, 3))) |>
      datatable(rownames = FALSE, options = list(pageLength = 15, dom = "ft",
                                                 scrollX = TRUE))
  })

  # -- trends -------------------------------------------------------------------
  output$trend_rolling <- renderPlot(plot_rolling_winpct(db()))
  output$trend_dominance <- renderPlot(plot_bib_dominance(db()))
  output$trend_cumapps <- renderPlot(plot_cumulative_appearances(db()))
  output$trend_calendar <- renderPlot(plot_calendar_heatmap(db()))

  # -- data & upload ------------------------------------------------------------
  upload_text <- reactive({
    if (isTruthy(input$upload_file)) {
      readr::read_file(input$upload_file$datapath)
    } else {
      input$upload_text
    }
  })

  observeEvent(input$preview_btn, {
    txt <- upload_text()
    if (!isTruthy(txt) || !nzchar(trimws(txt))) {
      showNotification("Nothing to parse - paste a team sheet first.",
                       type = "warning")
      return()
    }
    parsed <- parse_team_sheets(txt)
    aliases <- load_aliases(ALIAS_PATH)
    preview_db <- build_database_from_parsed(parsed, aliases)
    upload_preview(list(parsed = parsed, db = preview_db, text = txt))
  })

  output$upload_summary <- renderUI({
    pv <- upload_preview()
    req(pv)
    m <- pv$db$matches
    tagList(
      h5("Parse preview"),
      p(sprintf("%d match(es), %d appearance(s), %d issue(s).",
                nrow(m), nrow(pv$db$appearances), nrow(pv$db$parse_issues))),
      if (nrow(m) > 0) tableOutput("upload_matches")
    )
  })
  output$upload_matches <- renderTable({
    pv <- upload_preview(); req(pv)
    pv$db$matches |>
      transmute(date = format(date, "%d %b %Y"),
                score = paste0(bibs_goals, "-", nonbibs_goals),
                winner = result, attendance)
  })
  output$upload_issues <- renderDT({
    pv <- upload_preview(); req(pv)
    pv$db$parse_issues |>
      datatable(rownames = FALSE, options = list(pageLength = 10, dom = "t"))
  })

  observeEvent(input$commit_btn, {
    pv <- upload_preview()
    if (is.null(pv)) {
      showNotification("Preview the parse before committing.", type = "warning")
      return()
    }
    existing <- readr::read_file(RAW_PATH)
    readr::write_file(paste(existing, pv$text, sep = "\n\n"), RAW_PATH)
    new_db <- build_database(RAW_PATH, ALIAS_PATH)
    save_database(new_db, "data")
    db(new_db)
    upload_preview(NULL)
    updateTextAreaInput(session, "upload_text", value = "")
    showNotification("Team sheet committed - all statistics updated.",
                     type = "message")
  })

  # -- alias editor -------------------------------------------------------------
  output$alias_table <- renderDT({
    db()$player_aliases |>
      datatable(rownames = FALSE, options = list(pageLength = 15, dom = "ft"))
  })
  observeEvent(input$alias_add, {
    if (!isTruthy(input$alias_new) || !isTruthy(input$alias_canonical)) {
      showNotification("Alias and canonical name are both required.",
                       type = "warning")
      return()
    }
    aliases <- readr::read_csv(ALIAS_PATH, show_col_types = FALSE) |>
      bind_rows(tibble(alias = input$alias_new,
                       canonical = input$alias_canonical,
                       ambiguous = isTRUE(input$alias_ambiguous),
                       note = ifelse(nzchar(input$alias_note),
                                     input$alias_note, NA_character_)))
    readr::write_csv(aliases, ALIAS_PATH, na = "")
    new_db <- build_database(RAW_PATH, ALIAS_PATH)
    save_database(new_db, "data")
    db(new_db)
    showNotification("Alias added and database rebuilt.", type = "message")
  })

  # -- issues -------------------------------------------------------------------
  output$issues_table <- renderDT({
    db()$parse_issues |>
      datatable(rownames = FALSE, options = list(pageLength = 20, dom = "ft"))
  })

  # -- downloads ----------------------------------------------------------------
  output$dl_table <- downloadHandler(
    filename = function() paste0("tnf_", input$dl_table_pick, ".csv"),
    content = function(file) readr::write_csv(db()[[input$dl_table_pick]], file)
  )
  output$dl_figure <- downloadHandler(
    filename = function() paste0("tnf_", input$dl_fig_pick, ".png"),
    content = function(file) {
      figs <- all_figures(db())
      ggsave(file, figs[[input$dl_fig_pick]], width = 10, height = 7,
             dpi = 300, bg = "white")
    }
  )
}

shinyApp(ui, server)
