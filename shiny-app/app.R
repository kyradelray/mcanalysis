# MCAnalysis Shiny App
# Interactive menstrual cycle analysis tool

library(shiny)
library(bslib)
library(DT)

# Increase max upload size to 500MB
options(shiny.maxRequestSize = 500 * 1024^2)

# Install mcanalysis from GitHub if not available
if (!requireNamespace("mcanalysis", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  remotes::install_github("kyradelray/mcanalysis", subdir = "r-package/mcanalysis")
}

# Load the mcanalysis package
library(mcanalysis)

# Custom CSS for professional styling - Navy Blue Theme
custom_css <- "
/* Color Palette */
:root {
  --primary-navy: #1e3a5f;
  --secondary-navy: #2c5282;
  --primary-dark: #152a45;
  --accent-blue: #3182ce;
  --light-navy: #ebf4ff;
}

/* Global styles */
body {
  background-color: #f8f9fa;
}

/* Header styling */
.navbar, .bslib-page-sidebar > .navbar {
  background: linear-gradient(135deg, var(--primary-navy) 0%, var(--secondary-navy) 100%) !important;
  box-shadow: 0 2px 8px rgba(30, 58, 95, 0.3);
}

/* Sidebar styling */
.sidebar {
  background-color: #ffffff;
  border-right: 1px solid #e9ecef;
}

/* Card improvements */
.card {
  border: none;
  border-radius: 12px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
  transition: box-shadow 0.2s ease;
}

.card:hover {
  box-shadow: 0 4px 16px rgba(0,0,0,0.1);
}

.card-header {
  background-color: #ffffff;
  border-bottom: 1px solid #f0f0f0;
  font-weight: 600;
  padding: 1rem 1.25rem;
  border-radius: 12px 12px 0 0 !important;
}

.card-body {
  padding: 1.25rem;
}

/* Sidebar cards */
.sidebar .card {
  margin-bottom: 1rem;
  background-color: #fafbfc;
}

.sidebar .card-header {
  background: linear-gradient(135deg, var(--primary-navy) 0%, var(--secondary-navy) 100%);
  color: white;
  font-size: 0.9rem;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

/* Button styling */
.btn-primary {
  background: linear-gradient(135deg, var(--primary-navy) 0%, var(--secondary-navy) 100%);
  border: none;
  border-radius: 8px;
  font-weight: 600;
  letter-spacing: 0.3px;
  transition: transform 0.2s ease, box-shadow 0.2s ease;
}

.btn-primary:hover {
  transform: translateY(-1px);
  box-shadow: 0 4px 12px rgba(30, 58, 95, 0.4);
  background: linear-gradient(135deg, var(--primary-dark) 0%, var(--accent-blue) 100%);
}

/* File input styling */
.form-control, .shiny-input-container input[type='text'] {
  border-radius: 8px;
  border: 1px solid #e0e0e0;
  transition: border-color 0.2s ease, box-shadow 0.2s ease;
}

.form-control:focus {
  border-color: var(--primary-navy);
  box-shadow: 0 0 0 3px rgba(30, 58, 95, 0.15);
}

/* Tab styling */
.nav-tabs .nav-link {
  border-radius: 8px 8px 0 0;
  font-weight: 500;
  color: #6c757d;
  transition: color 0.2s ease;
}

.nav-tabs .nav-link.active {
  color: var(--primary-navy);
  border-bottom: 2px solid var(--primary-navy);
}

.nav-tabs .nav-link:hover {
  color: var(--secondary-navy);
}

/* DataTable styling */
.dataTables_wrapper {
  font-size: 0.9rem;
}

table.dataTable thead th {
  background-color: var(--light-navy);
  font-weight: 600;
  text-transform: uppercase;
  font-size: 0.75rem;
  letter-spacing: 0.5px;
  color: var(--primary-navy);
}

/* Plot container */
.shiny-plot-output {
  border-radius: 8px;
  overflow: hidden;
}

/* Info button */
.info-btn {
  background: rgba(255,255,255,0.2);
  border: 1px solid rgba(255,255,255,0.3);
  border-radius: 50%;
  width: 32px;
  height: 32px;
  padding: 0;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  transition: background 0.2s ease;
}

.info-btn:hover {
  background: rgba(255,255,255,0.3);
}

/* Header title */
.app-title {
  font-weight: 700;
  letter-spacing: -0.5px;
  font-size: 1.4rem;
  color: white !important;
}

.header-subtitle {
  color: rgba(255,255,255,0.85) !important;
  font-size: 1.1rem;
}

/* Oxford logo in header */
.oxford-logo {
  height: 36px;
  margin-right: 12px;
  filter: brightness(0) invert(1);
}

/* Footer */
.app-footer {
  background: linear-gradient(to bottom, #ffffff, #f8f9fa);
  border-top: 1px solid #e9ecef;
  padding: 2rem 1rem;
  text-align: center;
  color: #495057;
  font-size: 0.9rem;
  margin-top: 2rem;
}

.app-footer a {
  color: var(--primary-navy);
  text-decoration: none;
  font-weight: 500;
}

.app-footer a:hover {
  text-decoration: underline;
  color: var(--secondary-navy);
}

.footer-logo {
  height: 50px;
  margin-bottom: 1rem;
}

.footer-citation {
  background-color: var(--light-navy);
  border-left: 4px solid var(--primary-navy);
  padding: 1rem 1.5rem;
  margin: 1.5rem auto;
  max-width: 600px;
  text-align: left;
  border-radius: 0 8px 8px 0;
  font-size: 0.85rem;
}

.footer-citation code {
  background-color: #ffffff;
  padding: 0.2rem 0.4rem;
  border-radius: 4px;
  font-size: 0.8rem;
}

/* Tooltip styling */
.tooltip-inner {
  max-width: 300px;
  text-align: left;
  padding: 0.75rem 1rem;
  border-radius: 8px;
  background: linear-gradient(135deg, var(--primary-navy) 0%, var(--secondary-navy) 100%);
}

/* Select input */
.selectize-input {
  border-radius: 8px !important;
}

/* Radio buttons */
.shiny-input-radiogroup label {
  font-weight: 500;
}

/* Progress/loading */
.shiny-notification {
  border-radius: 8px;
}

/* Credit section */
.credit-section {
  margin-top: 1rem;
  padding-top: 1rem;
  border-top: 1px solid #e9ecef;
}

.developer-credit {
  font-weight: 500;
  color: var(--primary-navy);
}

"

# UI -------------------------------------------------------------------------
ui <- page_sidebar(
  title = div(
    class = "d-flex align-items-center",
    tags$img(
      src = "oxford-logo-white.png",
      class = "oxford-logo",
      alt = "University of Oxford",
      onerror = "this.style.display='none'"
    ),
    span(class = "app-title", "MCAnalysis"),
    span(class = "header-subtitle", style = "margin-left: 8px;", "| Menstrual Cycle Analysis"),
    actionButton("info_btn", "", icon = icon("circle-info"),
                 class = "info-btn ms-3", style = "color: white;")
  ),
  theme = bs_theme(
    version = 5,
    primary = "#1e3a5f",
    secondary = "#2c5282",
    success = "#28a745",
    info = "#3182ce",
    warning = "#ffc107",
    danger = "#dc3545",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    font_scale = 0.95,
    `enable-rounded` = TRUE
  ) |> bs_add_rules(custom_css),
  fillable = FALSE,

  # Sidebar with inputs
  sidebar = sidebar(
    width = 360,
    bg = "#ffffff",

    # File upload mode
    card(
      card_header(icon("cloud-upload"), " Data Upload"),
      radioButtons("upload_mode", "Upload mode:",
                   choices = c("Separate files" = "separate",
                               "Single combined file" = "combined"),
                   selected = "separate"),
      conditionalPanel(
        condition = "input.upload_mode == 'combined'",
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          div(style = "flex: 1;",
            fileInput("combined_file", "Combined Data (CSV)",
                      accept = ".csv",
                      placeholder = "All data in one file")
          ),
          tooltip(
            span(icon("circle-question"), style = "color: #6c757d; cursor: help;"),
            "A single CSV file containing user IDs, period dates, observation dates, and outcome values all in one file.",
            placement = "right"
          )
        )
      ),
      conditionalPanel(
        condition = "input.upload_mode == 'separate'",
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          div(style = "flex: 1;",
            fileInput("periods_file", "Menstruation Dates (CSV)",
                      accept = ".csv")
          ),
          tooltip(
            span(icon("circle-question"), style = "color: #6c757d; cursor: help;"),
            "CSV with user IDs and their menstruation start dates. Each row = one period start date for one user.",
            placement = "right"
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          div(style = "flex: 1;",
            fileInput("outcomes_file", "Outcome Data (CSV)",
                      accept = ".csv")
          ),
          tooltip(
            span(icon("circle-question"), style = "color: #6c757d; cursor: help;"),
            "CSV with user IDs, observation dates, and the outcome variable you want to analyse (e.g., mood, sleep, HRV).",
            placement = "right"
          )
        ),
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          div(style = "flex: 1;",
            fileInput("confounders_file", "Confounders (CSV, optional)",
                      accept = ".csv")
          ),
          tooltip(
            span(icon("circle-question"), style = "color: #6c757d; cursor: help;"),
            "Optional CSV with user IDs, dates, and variables that might affect the outcome (e.g., weekday, season, stress).",
            placement = "right"
          )
        )
      )
    ),

    # Column mapping
    card(
      card_header(icon("columns"), " Column Mapping"),
      p(style = "font-size: 0.85rem; color: #6c757d; margin-bottom: 1rem;",
        "Match these to the column names in your uploaded CSV files."),
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        div(style = "flex: 1;", textInput("id_col", "User ID column", value = "id")),
        tooltip(
          span(icon("circle-question"), style = "color: #6c757d; cursor: help; margin-top: 1.5rem;"),
          "Unique identifier for each participant (e.g., 'user_id', 'participant_id', 'id')",
          placement = "right"
        )
      ),
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        div(style = "flex: 1;", textInput("date_col", "Menstruation date column", value = "period_date")),
        tooltip(
          span(icon("circle-question"), style = "color: #6c757d; cursor: help; margin-top: 1.5rem;"),
          "Column containing the start date of each menstrual period (e.g., 'period_date', 'cycle_start')",
          placement = "right"
        )
      ),
      div(
        style = "display: flex; align-items: center; gap: 8px;",
        div(style = "flex: 1;", textInput("outcome_date_col", "Observation date column", value = "date")),
        tooltip(
          span(icon("circle-question"), style = "color: #6c757d; cursor: help; margin-top: 1.5rem;"),
          "Column containing the date each outcome was recorded (e.g., 'date', 'observation_date', 'timestamp')",
          placement = "right"
        )
      ),
      conditionalPanel(
        condition = "input.upload_mode == 'combined'",
        uiOutput("outcome_selector")
      ),
      conditionalPanel(
        condition = "input.upload_mode == 'separate'",
        div(
          style = "display: flex; align-items: center; gap: 8px;",
          div(style = "flex: 1;", textInput("outcome_col", "Outcome column", value = "outcome")),
          tooltip(
            span(icon("circle-question"), style = "color: #6c757d; cursor: help; margin-top: 1.5rem;"),
            "Column containing the outcome variable to analyse (e.g., 'mood', 'sleep_hours', 'pain_level')",
            placement = "right"
          )
        )
      ),
      actionButton("run_analysis", "Run Analysis",
                   class = "btn-primary btn-lg w-100 mt-3",
                   icon = icon("play"))
    ),

  ),

  # Main panel with results
  navset_card_tab(
    id = "results_tabs",
    # Main Results tab (first)
    nav_panel(
      title = "Cycle Effect",
      icon = icon("chart-line"),
      card(
        card_header(
          div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            textOutput("plot_title", inline = TRUE),
            tooltip(
              span(icon("circle-question"), style = "cursor: help; color: #6c757d;"),
              "Y-axis shows how much the outcome varies from each individual's personal average. A value of +2% means the outcome is 2% higher than that person's typical value. This accounts for individual differences, so we can see the cycle effect across all participants.",
              placement = "left"
            )
          )
        ),
        card_body(
          plotOutput("main_plot", height = "600px")
        ),
        card_footer(
          downloadButton("download_report", "Download Report", class = "btn-primary")
        )
      )
    ),

    # Data Preview tab
    nav_panel(
      title = "Data Preview",
      icon = icon("table"),
      card(
        card_header(icon("table"), " Uploaded Data"),
        DTOutput("data_preview")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("list"), " Available Columns"),
          verbatimTextOutput("column_list")
        ),
        card(
          card_header(icon("circle-info"), " Data Info"),
          verbatimTextOutput("data_info")
        )
      )
    ),

    # EDA tab
    nav_panel(
      title = "Exploratory Analysis",
      icon = icon("chart-bar"),
      layout_columns(
        col_widths = c(12),
        card(
          card_header(icon("clipboard-list"), " Data Summary"),
          DTOutput("data_summary")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("chart-bar"), " Outcome Distribution"),
          plotOutput("plot_outcome", height = "400px")
        ),
        card(
          card_header(icon("calendar-days"), " Cycle Lengths"),
          plotOutput("plot_cycles", height = "400px")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("users"), " Observations per User"),
          plotOutput("plot_obs_per_user", height = "400px")
        ),
        card(
          card_header(icon("layer-group"), " Confounder Distributions"),
          plotOutput("plot_confounders", height = "400px")
        )
      )
    ),

    # Confounders tab
    nav_panel(
      title = "Confounders & Effect Modifiers",
      icon = icon("sliders"),

      # Confounders Section
      card(
        card_header(
          div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            span(icon("chart-line"), " Confounder Effects on Cycle-Outcome Relationship"),
            tooltip(
              span(icon("circle-question"), style = "cursor: help; color: #6c757d;"),
              "Each bar shows how much the confounder shifts the outcome (in % from individual mean). Error bars show 95% confidence intervals. Bars not crossing zero indicate significant effects.",
              placement = "left"
            )
          )
        ),
        card_body(
          plotOutput("confounder_plot", height = "400px")
        )
      ),
      card(
        card_header(icon("info-circle"), " Understanding Confounders"),
        card_body(
          div(
            style = "background-color: #f8f9fa; padding: 1.25rem; border-radius: 8px; font-size: 0.9rem; color: #495057;",

            tags$h6(style = "color: #1e3a5f; margin-bottom: 0.75rem;", icon("question-circle"), " Why adjust for confounders?"),
            tags$p(
              "Confounders are external factors that could explain changes in your outcome ",
              tags$em("independently"), " of the menstrual cycle. Without adjusting for them, you might mistakenly attribute ",
              "their effects to the cycle itself."
            ),

            tags$h6(style = "color: #1e3a5f; margin-top: 1.25rem; margin-bottom: 0.75rem;", icon("list"), " Common confounders"),
            tags$ul(style = "margin-bottom: 1rem;",
              tags$li(tags$strong("Day of week:"), " Sleep and activity often differ on weekends vs weekdays"),
              tags$li(tags$strong("Season/month:"), " Mood, energy, and physiology vary with seasons"),
              tags$li(tags$strong("Time trends:"), " Outcomes may drift over time due to lifestyle changes"),
              tags$li(tags$strong("Age:"), " Baseline levels of many outcomes change with age")
            ),

            tags$h6(style = "color: #1e3a5f; margin-top: 1.25rem; margin-bottom: 0.75rem;", icon("chart-bar"), " Reading the results"),
            tags$p(tags$strong("The coefficient"), " tells you how much the outcome shifts (in % from each person's mean) when the confounder increases by 1 unit:"),
            tags$ul(
              tags$li(tags$span(style = "color: #27ae60;", "Positive (+):"), " Higher confounder value → higher outcome"),
              tags$li(tags$span(style = "color: #e74c3c;", "Negative (−):"), " Higher confounder value → lower outcome"),
              tags$li(tags$strong("95% CI not crossing zero:"), " The effect is statistically significant (p < 0.05)")
            ),

            tags$h6(style = "color: #1e3a5f; margin-top: 1.25rem; margin-bottom: 0.75rem;", icon("lightbulb"), " Example interpretation"),
            tags$div(
              style = "background-color: #ffffff; border-left: 3px solid #1e3a5f; padding: 0.75rem 1rem; margin-top: 0.5rem;",
              tags$p(style = "margin-bottom: 0.5rem;",
                tags$code("weekend"), " coefficient = ", tags$strong("+3.2"), ", p = 0.002"
              ),
              tags$p(style = "margin-bottom: 0;",
                "→ On weekends, the outcome is ", tags$strong("3.2% higher"), " than on weekdays, ",
                "after accounting for the menstrual cycle effect. This is statistically significant."
              )
            ),

            tags$h6(style = "color: #1e3a5f; margin-top: 1.25rem; margin-bottom: 0.75rem;", icon("exclamation-triangle"), " Important note"),
            tags$p(style = "margin-bottom: 0;",
              "The main cycle effect plot shows the ", tags$strong("unadjusted"), " relationship. ",
              "Use the 'Adjusted Cycle Effect Model' section below to see how the curve changes when controlling for confounders."
            )
          )
        )
      ),
      card(
        card_header(icon("table"), " Confounder Analysis Table"),
        DTOutput("confounder_table")
      ),

      # Adjusted Model Section
      card(
        card_header(
          div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            span(icon("sliders"), " Adjusted Cycle Effect Model"),
            tooltip(
              span(icon("circle-question"), style = "cursor: help; color: #6c757d;"),
              "Select confounders to include in the GAM model. The plot updates automatically to show the cycle effect after controlling for selected confounders.",
              placement = "left"
            )
          )
        ),
        card_body(
          uiOutput("adjusted_model_selector"),
          plotOutput("adjusted_model_plot", height = "450px")
        )
      ),

      # Effect Modifiers Section
      tags$hr(style = "margin: 2rem 0; border-top: 2px solid #1e3a5f;"),
      tags$h4(style = "color: #1e3a5f; margin-bottom: 1rem;", icon("code-branch"), " Effect Modifiers"),

      card(
        card_header(icon("info-circle"), " Understanding Effect Modifiers"),
        card_body(
          div(
            style = "background-color: #fff8e6; padding: 1.25rem; border-radius: 8px; font-size: 0.9rem; color: #495057; border-left: 4px solid #f0ad4e;",

            tags$h6(style = "color: #8a6d3b; margin-bottom: 0.75rem;", icon("question-circle"), " What are effect modifiers?"),
            tags$p(
              "Effect modifiers are variables that ", tags$strong("change the strength or direction"), " of the cycle effect itself. ",
              "Unlike confounders (which shift the outcome level), effect modifiers create ", tags$em("different cycle patterns"), " for different groups."
            ),

            tags$h6(style = "color: #8a6d3b; margin-top: 1.25rem; margin-bottom: 0.75rem;", icon("not-equal"), " Confounders vs Effect Modifiers"),
            tags$div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; margin-bottom: 0;",
              tags$div(
                style = "background-color: #ffffff; padding: 1rem; border-radius: 8px; border: 1px solid #e0e0e0;",
                tags$strong(style = "color: #1e3a5f;", "Confounder"),
                tags$p(style = "margin-bottom: 0; margin-top: 0.5rem; font-size: 0.85rem;",
                  "Shifts the ", tags$em("level"), " of your outcome"
                )
              ),
              tags$div(
                style = "background-color: #ffffff; padding: 1rem; border-radius: 8px; border: 1px solid #f0ad4e;",
                tags$strong(style = "color: #8a6d3b;", "Effect Modifier"),
                tags$p(style = "margin-bottom: 0; margin-top: 0.5rem; font-size: 0.85rem;",
                  "Changes the ", tags$em("cycle pattern"), " itself"
                )
              )
            )
          )
        )
      ),

      card(
        card_header(
          div(
            style = "display: flex; align-items: center; justify-content: space-between;",
            span(icon("chart-line"), " Effect Modifier Analysis"),
            tooltip(
              span(icon("circle-question"), style = "cursor: help; color: #6c757d;"),
              "Select a variable to see if the cycle effect differs across groups. This fits separate cycle curves for each level of the effect modifier.",
              placement = "left"
            )
          )
        ),
        card_body(
          uiOutput("effect_modifier_selector"),
          plotOutput("effect_modifier_plot", height = "450px")
        )
      ),

      card(
        card_header(icon("table"), " Effect Modifier Results"),
        DTOutput("effect_modifier_table")
      )
    ),

    # Results tab
    nav_panel(
      title = "Results",
      icon = icon("table"),
      card(
        card_header(icon("file-lines"), " Analysis Summary"),
        verbatimTextOutput("summary_text")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("chart-area"), " GAM Model Results"),
          DTOutput("gam_table")
        ),
        card(
          card_header(icon("arrows-left-right"), " Phase Models"),
          DTOutput("phase_table")
        )
      )
    ),

  ),

  # Footer
  div(
    class = "app-footer",
    # Oxford Logo
    tags$img(
      src = "oxford-logo.png",
      class = "footer-logo",
      alt = "University of Oxford",
      onerror = "this.style.display='none'"
    ),
    # Developer Credit
    div(
      class = "credit-section",
      div(
        class = "developer-credit",
        "Developed by ",
        tags$strong("Kyra Delray")
      ),
      div(
        style = "color: #6c757d; margin-top: 0.25rem;",
        "Department of Statistics, University of Oxford"
      )
    ),
    # Citation Box
    div(
      class = "footer-citation",
      div(style = "font-weight: 600; margin-bottom: 0.5rem; color: #1e3a5f;",
          icon("bookmark"), " If you use this tool, please cite:"),
      div(
        "Delray, K. (", format(Sys.Date(), "%Y"), "). ",
        tags$em("MCAnalysis: Menstrual Cycle Analysis Tool."),
        " GitHub repository: ",
        tags$a(href = "https://github.com/KyraDelray/mcanalysis",
               target = "_blank",
               "https://github.com/KyraDelray/mcanalysis")
      )
    ),
    # Version and Links
    div(
      style = "margin-top: 1.5rem;",
      tags$a(href = "https://github.com/KyraDelray/mcanalysis", target = "_blank",
             icon("github"), " View on GitHub"),
      span(" | ", style = "color: #dee2e6;"),
      span(paste0("Version 1.0.0 | ", format(Sys.Date(), "%Y")))
    )
  )
)

# Server ---------------------------------------------------------------------
server <- function(input, output, session) {

  # Info modal
  observeEvent(input$info_btn, {
    showModal(modalDialog(
      title = div(
        style = "display: flex; align-items: center;",
        icon("chart-line", style = "color: #1e3a5f; margin-right: 10px; font-size: 1.5rem;"),
        span("Welcome to MCAnalysis", style = "font-weight: 600;")
      ),
      div(
        style = "padding: 1rem 0;",
        h5(icon("info-circle", style = "color: #1e3a5f;"), " About", style = "font-weight: 600; margin-bottom: 1rem;"),
        p("MCAnalysis estimates menstrual cycle effects on health outcomes in digital health data.
           It uses Generalized Additive Models (GAMs) with a Fourier basis to capture cyclical patterns,
           expressing outcomes as percentage change from each individual's mean to account for
           between-person variability.",
          style = "color: #6c757d;"),
        hr(style = "margin: 1.5rem 0;"),
        h5(icon("cogs", style = "color: #1e3a5f;"), " How It Works", style = "font-weight: 600; margin-bottom: 1rem;"),

        tags$div(style = "margin-bottom: 1rem;",
          tags$strong("Preprocessing", style = "color: #1e3a5f;"),
          tags$ul(style = "color: #6c757d; margin-top: 0.5rem;",
            tags$li("Aligns observations to menstruation onset (Day 0)"),
            tags$li("Shifts cycle days to range from -14 to +13 (Day 0 = menstruation start)"),
            tags$li("Filters cycles by length (default 21-35 days)"),
            tags$li("Normalises outcomes as % difference from each individual's mean")
          )
        ),

        tags$div(style = "margin-bottom: 1rem;",
          tags$strong("Modelling", style = "color: #1e3a5f;"),
          tags$ul(style = "color: #6c757d; margin-top: 0.5rem;",
            tags$li("Fits a GAM with Fourier basis to capture cyclical patterns"),
            tags$li("Detects turning points (peaks and troughs) in the cycle effect"),
            tags$li("Fits linear models between turning points to estimate phase slopes")
          )
        ),

        tags$div(style = "margin-bottom: 0;",
          tags$strong("Visualisation", style = "color: #1e3a5f;"),
          tags$ul(style = "color: #6c757d; margin-top: 0.5rem;",
            tags$li("Displays the GAM curve with 95% confidence intervals"),
            tags$li("Marks turning points and phase transitions"),
            tags$li("Shows statistical summaries including p-values and deviance explained")
          )
        ),
        hr(style = "margin: 1.5rem 0;"),
        h5(icon("upload", style = "color: #1e3a5f;"), " Upload Modes", style = "font-weight: 600; margin-bottom: 1rem;"),
        tags$ul(
          style = "color: #6c757d;",
          tags$li(tags$strong("Separate files:"), " One CSV with menstruation dates, another with outcome observations"),
          tags$li(tags$strong("Single combined file:"), " All data in one CSV with user ID, dates, and outcome columns")
        ),
        hr(style = "margin: 1.5rem 0;"),
        h5(icon("list-ol", style = "color: #1e3a5f;"), " Getting Started", style = "font-weight: 600; margin-bottom: 1rem;"),
        tags$ol(
          style = "color: #6c757d;",
          tags$li("Upload your menstruation dates and outcome data"),
          tags$li("Adjust column names to match your CSV files"),
          tags$li("Click 'Run Analysis'"),
          tags$li("Explore results in the Cycle Effect and Results tabs")
        ),
        hr(style = "margin: 1.5rem 0;"),
        div(
          style = "text-align: center;",
          tags$a(href = "https://medium.com/@kyradelray/introducing-mcanalysis-a-tool-for-menstrual-cycle-effect-estimation-in-digital-health-data-897485b7eda5",
                 target = "_blank",
                 style = "color: #1e3a5f; font-weight: 500;",
                 icon("book-open"), " Read the full guide on Medium")
        )
      ),
      size = "l",
      easyClose = TRUE,
      footer = div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        div(
          style = "display: flex; align-items: center;",
          tags$img(
            src = "oxford-logo.png",
            style = "height: 24px; margin-right: 8px;",
            onerror = "this.style.display='none'"
          ),
          span("University of Oxford", style = "color: #1e3a5f; font-size: 0.85rem; font-weight: 500;")
        ),
        modalButton("Got it", icon = icon("check"))
      )
    ))
  })

  # Reactive values to store data and results
  rv <- reactiveValues(
    raw_data = NULL,
    periods = NULL,
    outcomes = NULL,
    confounders = NULL,
    results = NULL,
    eda_results = NULL,
    outcome_name = NULL
  )

  # Read combined file
  observe({
    req(input$combined_file)
    rv$raw_data <- read.csv(input$combined_file$datapath, stringsAsFactors = FALSE)
  })

  # Read separate files
  observe({
    req(input$periods_file)
    rv$periods <- read.csv(input$periods_file$datapath, stringsAsFactors = FALSE)
  })

  observe({
    req(input$outcomes_file)
    rv$outcomes <- read.csv(input$outcomes_file$datapath, stringsAsFactors = FALSE)
  })

  observe({
    req(input$confounders_file)
    rv$confounders <- read.csv(input$confounders_file$datapath, stringsAsFactors = FALSE)
  })

  # Dynamic outcome selector for combined mode
  output$outcome_selector <- renderUI({
    req(rv$raw_data)

    # Get numeric columns (potential outcomes)
    numeric_cols <- names(rv$raw_data)[sapply(rv$raw_data, is.numeric)]

    # Filter out ID columns and known non-outcome columns
    exclude_patterns <- c("id", "userid", "cycle", "len", "age", "height", "weight", "bmi")
    outcome_cols <- numeric_cols[!grepl(paste(exclude_patterns, collapse = "|"),
                                         numeric_cols, ignore.case = TRUE)]

    # Common health outcomes first
    priority_cols <- c("steps", "sleep_minutes", "rhr", "est_rhr", "daily_mean_hr",
                       "daily_median_sdnn", "Fatigue", "Happy", "Energetic", "Sad",
                       "Stressed", "Headache", "Bloating")
    priority_cols <- priority_cols[priority_cols %in% outcome_cols]
    other_cols <- setdiff(outcome_cols, priority_cols)
    ordered_cols <- c(priority_cols, other_cols)

    selectInput("outcome_col_select", "Select outcome variable:",
                choices = ordered_cols,
                selected = if (length(priority_cols) > 0) priority_cols[1] else ordered_cols[1])
  })

  # Data preview
  output$data_preview <- renderDT({
    if (input$upload_mode == "combined") {
      req(rv$raw_data)
      datatable(head(rv$raw_data, 100),
                options = list(pageLength = 10, scrollX = TRUE))
    } else {
      req(rv$periods)
      datatable(head(rv$periods, 100),
                options = list(pageLength = 10, scrollX = TRUE))
    }
  })

  output$column_list <- renderText({
    if (input$upload_mode == "combined" && !is.null(rv$raw_data)) {
      paste(names(rv$raw_data), collapse = "\n")
    } else if (!is.null(rv$periods)) {
      paste("Menstruation dates:", paste(names(rv$periods), collapse = ", "),
            "\n\nOutcomes:", paste(names(rv$outcomes), collapse = ", "))
    } else {
      "No data uploaded"
    }
  })

  output$data_info <- renderText({
    if (input$upload_mode == "combined" && !is.null(rv$raw_data)) {
      sprintf("Rows: %s\nColumns: %d\nUsers: %s",
              format(nrow(rv$raw_data), big.mark = ","),
              ncol(rv$raw_data),
              format(length(unique(rv$raw_data[[input$id_col]])), big.mark = ","))
    } else if (!is.null(rv$periods)) {
      sprintf("Menstruation records: %s\nOutcome records: %s",
              format(nrow(rv$periods), big.mark = ","),
              format(nrow(rv$outcomes), big.mark = ","))
    } else {
      "No data uploaded"
    }
  })

  # Run analysis
  observeEvent(input$run_analysis, {

    message("=== RUN ANALYSIS CLICKED ===")
    message("Upload mode: ", input$upload_mode)

    # Show progress
    withProgress(message = "Running analysis...", value = 0, {

      incProgress(0.1, detail = "Preparing data...")
      message("Preparing data...")

      id_col <- input$id_col
      date_col <- input$date_col
      outcome_date_col <- input$outcome_date_col

      # Prepare data based on upload mode
      if (input$upload_mode == "combined") {
        message("Combined mode - checking requirements...")
        message("raw_data exists: ", !is.null(rv$raw_data))
        message("outcome_col_select: ", input$outcome_col_select)

        req(rv$raw_data, input$outcome_col_select)

        outcome_col <- input$outcome_col_select
        rv$outcome_name <- outcome_col  # Store original name for display
        message("Selected outcome: ", outcome_col)

        # Check columns exist
        if (!id_col %in% names(rv$raw_data)) {
          showNotification(paste("Column", id_col, "not found"), type = "error")
          return()
        }
        if (!date_col %in% names(rv$raw_data)) {
          showNotification(paste("Column", date_col, "not found"), type = "error")
          return()
        }

        message("Extracting period dates...")
        # Extract period dates (unique period start dates per user)
        periods <- unique(rv$raw_data[, c(id_col, date_col)])
        names(periods) <- c("id", "period_date")
        periods <- periods[!is.na(periods$period_date), ]
        message("Period dates extracted: ", nrow(periods), " rows")

        message("Extracting outcomes...")
        # Extract outcomes
        outcomes <- rv$raw_data[, c(id_col, outcome_date_col, outcome_col)]
        names(outcomes) <- c("id", "date", "outcome")
        outcomes <- outcomes[!is.na(outcomes$outcome), ]
        message("Outcomes extracted: ", nrow(outcomes), " rows")

        rv$periods <- periods
        rv$outcomes <- outcomes

        # Use standardized column names
        id_col <- "id"
        date_col <- "period_date"
        outcome_col <- "outcome"
        outcome_date_col <- "date"

      } else {
        req(rv$periods, rv$outcomes)
        outcome_col <- input$outcome_col
        rv$outcome_name <- outcome_col  # Store original name for display

        # Check columns exist
        if (!id_col %in% names(rv$periods)) {
          showNotification(paste("Column", id_col, "not found in menstruation dates"), type = "error")
          return()
        }
        if (!date_col %in% names(rv$periods)) {
          showNotification(paste("Column", date_col, "not found in menstruation dates"), type = "error")
          return()
        }
        if (!id_col %in% names(rv$outcomes)) {
          showNotification(paste("Column", id_col, "not found in outcomes"), type = "error")
          return()
        }
        if (!outcome_col %in% names(rv$outcomes)) {
          showNotification(paste("Column", outcome_col, "not found in outcomes"), type = "error")
          return()
        }
      }

      # Run EDA
      incProgress(0.2, detail = "Running exploratory analysis...")

      tryCatch({
        rv$eda_results <- run_exploratory_analysis(
          period_dates = rv$periods,
          outcome_data = rv$outcomes,
          confounder_data = rv$confounders,
          id_col = id_col,
          date_col = date_col,
          outcome_col = outcome_col,
          outcome_date_col = outcome_date_col
        )
      }, error = function(e) {
        showNotification(paste("EDA error:", e$message), type = "warning")
      })

      # Run main analysis
      incProgress(0.5, detail = "Fitting GAM model...")

      tryCatch({
        rv$results <- mc_analysis(
          period_dates = rv$periods,
          outcome_data = rv$outcomes,
          confounder_data = rv$confounders,
          id_col = id_col,
          date_col = date_col,
          outcome_col = outcome_col,
          outcome_date_col = outcome_date_col,
          min_cycle_length = 21,
          max_cycle_length = 35,
          min_negative_obs = 5,
          min_positive_obs = 5,
          k = 2
        )

        incProgress(1, detail = "Complete!")
        showNotification("Analysis complete!", type = "message")

        # Switch to results tab
        updateNavlistPanel(session, "results_tabs", selected = "Cycle Effect")

      }, error = function(e) {
        showNotification(paste("Analysis error:", e$message), type = "error")
      })
    })
  })

  # EDA outputs
  output$data_summary <- renderDT({
    req(rv$eda_results)
    datatable(rv$eda_results$summary,
              options = list(pageLength = 20, dom = 't'),
              rownames = FALSE)
  })

  output$plot_outcome <- renderPlot({
    req(rv$eda_results)
    rv$eda_results$plot_outcome
  })

  output$plot_cycles <- renderPlot({
    req(rv$eda_results)
    rv$eda_results$plot_cycles
  })

  output$plot_obs_per_user <- renderPlot({
    req(rv$eda_results)
    rv$eda_results$plot_obs_per_user
  })

  output$plot_confounders <- renderPlot({
    req(rv$eda_results)
    if (!is.null(rv$eda_results$plot_confounders)) {
      rv$eda_results$plot_confounders
    }
  })

  # Main plot - always show full plot with all features
  output$main_plot <- renderPlot({
    req(rv$results)

    # Format outcome name for display (convert snake_case to Title Case)
    outcome_display <- gsub("_", " ", rv$outcome_name)
    outcome_display <- tools::toTitleCase(outcome_display)

    # Get the base plot without the package's built-in annotations
    # We'll build a custom unified legend
    gam_result <- rv$results$gam_result
    turning_points <- rv$results$turning_points
    phase_models <- rv$results$phase_models
    day_range <- c(-14, 13)

    pred_df <- gam_result$predictions
    plot_data <- pred_df[pred_df$cycle_day >= day_range[1] &
                          pred_df$cycle_day <= day_range[2], ]

    plot_data$predicted_pct <- plot_data$predicted - 100
    plot_data$ci_lower_pct <- plot_data$ci_lower - 100
    plot_data$ci_upper_pct <- plot_data$ci_upper - 100

    y_min <- min(plot_data$ci_lower_pct, na.rm = TRUE)
    y_max <- max(plot_data$ci_upper_pct, na.rm = TRUE)

    # Define phase colors
    phase_colors <- c("#E74C3C", "#27AE60", "#3498DB", "#9B59B6")

    # Build legend data for phases
    legend_items <- list()
    legend_items[["GAM Fit"]] <- "steelblue"
    legend_items[["95% CI"]] <- "steelblue"

    if (!is.null(turning_points) && length(turning_points) > 0) {
      legend_items[["Turning Points"]] <- "orange"
    }

    # Base plot
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = cycle_day))

    # Phase shading
    if (!is.null(phase_models) && nrow(phase_models) > 0) {
      phase_shade_colors <- c("#E8F4F8", "#FFF3CD", "#E8F8E8", "#F8E8F8")
      for (i in seq_len(nrow(phase_models))) {
        pm <- phase_models[i, ]
        fill_color <- phase_shade_colors[(i - 1) %% length(phase_shade_colors) + 1]
        if (pm$end_day <= pm$start_day) {
          p <- p +
            ggplot2::annotate("rect", xmin = pm$start_day, xmax = day_range[2],
              ymin = -Inf, ymax = Inf, alpha = 0.15, fill = fill_color) +
            ggplot2::annotate("rect", xmin = day_range[1], xmax = pm$end_day,
              ymin = -Inf, ymax = Inf, alpha = 0.15, fill = fill_color)
        } else {
          p <- p +
            ggplot2::annotate("rect", xmin = pm$start_day, xmax = pm$end_day,
              ymin = -Inf, ymax = Inf, alpha = 0.15, fill = fill_color)
        }
      }
    }

    # Confidence interval
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lower_pct, ymax = ci_upper_pct),
        alpha = 0.3, fill = "steelblue"
      )

    # GAM curve
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = predicted_pct),
        color = "steelblue", linewidth = 1.2
      )

    # Reference lines
    p <- p +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8)

    # Phase slope lines with colors for legend
    if (!is.null(phase_models) && nrow(phase_models) > 0 && !is.null(turning_points)) {
      for (i in seq_len(nrow(phase_models))) {
        pm <- phase_models[i, ]
        line_color <- phase_colors[(i - 1) %% length(phase_colors) + 1]

        start_idx <- which.min(abs(plot_data$cycle_day - pm$start_day))
        end_idx <- which.min(abs(plot_data$cycle_day - pm$end_day))
        start_y <- plot_data$predicted_pct[start_idx]
        end_y <- plot_data$predicted_pct[end_idx]

        if (pm$end_day <= pm$start_day) {
          total_span <- (day_range[2] - pm$start_day) + (pm$end_day - day_range[1])
          frac1 <- (day_range[2] - pm$start_day) / total_span
          y_at_edge <- start_y + frac1 * (end_y - start_y)
          p <- p +
            ggplot2::annotate("segment", x = pm$start_day, xend = day_range[2],
              y = start_y, yend = y_at_edge,
              color = line_color, linetype = "dashed", linewidth = 0.9, alpha = 0.9) +
            ggplot2::annotate("segment", x = day_range[1], xend = pm$end_day,
              y = y_at_edge, yend = end_y,
              color = line_color, linetype = "dashed", linewidth = 0.9, alpha = 0.9)
        } else {
          p <- p +
            ggplot2::annotate("segment", x = pm$start_day, xend = pm$end_day,
              y = start_y, yend = end_y,
              color = line_color, linetype = "dashed", linewidth = 0.9, alpha = 0.9)
        }
      }
    }

    # Turning points
    if (!is.null(turning_points)) {
      for (i in seq_along(turning_points)) {
        tp <- turning_points[i]
        if (tp >= day_range[1] && tp <= day_range[2]) {
          closest_idx <- which.min(abs(plot_data$cycle_day - tp))
          y_val <- plot_data$predicted_pct[closest_idx]
          p <- p +
            ggplot2::geom_vline(xintercept = tp, linetype = "dashed", color = "orange", alpha = 0.6) +
            ggplot2::geom_point(data = data.frame(x = tp, y = y_val),
              ggplot2::aes(x = x, y = y), color = "orange", size = 4) +
            ggplot2::annotate("label", x = tp, y = y_val,
              label = paste0("Day ", round(tp, 0)),
              size = 3.5, vjust = -0.8,
              label.padding = ggplot2::unit(0.2, "lines"),
              fill = "white", color = "gray30")
        }
      }
    }

    # Significance subtitle
    p_val <- gam_result$p_value
    sig_stars <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else "(ns)"
    sig_text <- paste0("Cycle Effect p = ", format(p_val, scientific = TRUE, digits = 2), " ", sig_stars,
                       "  |  Deviance Explained: ", round(gam_result$deviance_explained * 100, 1), "%")

    # Watermark in bottom right of plot area
    p <- p +
      ggplot2::annotate("text", x = day_range[2] - 0.5, y = y_min,
        label = "MCAnalysis", hjust = 1, vjust = 1.5,
        size = 3.5, fontface = "italic", color = "gray60", alpha = 0.7)

    # Build unified legend - all items in one box using linetype aesthetic
    dummy_pt <- data.frame(x = NA_real_, y = NA_real_)

    p <- p +
      ggplot2::geom_line(data = dummy_pt,
        ggplot2::aes(x = x, y = y, linetype = "GAM Fit"),
        colour = "steelblue", linewidth = 1.5, show.legend = TRUE, na.rm = TRUE) +
      ggplot2::geom_point(data = dummy_pt,
        ggplot2::aes(x = x, y = y, linetype = "95% CI"),
        colour = "steelblue", shape = 15, size = 6, alpha = 0.4,
        show.legend = TRUE, na.rm = TRUE) +
      ggplot2::geom_point(data = dummy_pt,
        ggplot2::aes(x = x, y = y, linetype = "Turning Points"),
        colour = "orange", shape = 16, size = 4,
        show.legend = TRUE, na.rm = TRUE)

    # Single linetype scale for unified legend box
    p <- p +
      ggplot2::scale_linetype_manual(
        name = NULL,
        values = c("GAM Fit" = "solid", "95% CI" = "blank", "Turning Points" = "blank"),
        guide = ggplot2::guide_legend(
          order = 1,
          nrow = 1,
          override.aes = list(
            colour = c("steelblue", "steelblue", "orange"),
            linetype = c("solid", "blank", "blank"),
            linewidth = c(1.5, 0, 0),
            shape = c(NA, 15, 16),
            size = c(0, 6, 4),
            alpha = c(1, 0.4, 1)
          )
        )
      )

    # Phase slopes as second legend using shape
    if (!is.null(phase_models) && nrow(phase_models) > 0) {
      for (i in seq_len(nrow(phase_models))) {
        pm <- phase_models[i, ]
        sig <- if (pm$p_value < 0.001) "***" else if (pm$p_value < 0.01) "**" else if (pm$p_value < 0.05) "*" else ""
        direction <- if (pm$slope > 0) "\u2191" else "\u2193"
        slope_label <- sprintf("Phase %d: %s %.2f%% / day %s", i, direction, abs(pm$slope), sig)
        line_color <- phase_colors[(i - 1) %% length(phase_colors) + 1]

        p <- p +
          ggplot2::geom_point(data = dummy_pt,
            ggplot2::aes_string(x = "x", y = "y", shape = shQuote(slope_label)),
            colour = line_color, size = 3, na.rm = TRUE)
      }

      # Build shape scale for phase slopes
      n_phases <- nrow(phase_models)
      phase_shapes <- setNames(rep(16, n_phases),
        sapply(seq_len(n_phases), function(i) {
          pm <- phase_models[i, ]
          sig <- if (pm$p_value < 0.001) "***" else if (pm$p_value < 0.01) "**" else if (pm$p_value < 0.05) "*" else ""
          direction <- if (pm$slope > 0) "\u2191" else "\u2193"
          sprintf("Phase %d: %s %.2f%% / day %s", i, direction, abs(pm$slope), sig)
        }))

      phase_label_colors <- setNames(
        phase_colors[seq_len(n_phases)],
        names(phase_shapes))

      p <- p +
        ggplot2::scale_shape_manual(
          name = "Phase Slopes",
          values = phase_shapes,
          guide = ggplot2::guide_legend(
            order = 2,
            nrow = 1,
            override.aes = list(colour = phase_label_colors, size = 3)
          )
        )
    }

    # Final formatting
    p <- p +
      ggplot2::scale_x_continuous(breaks = seq(day_range[1], day_range[2], by = 2)) +
      ggplot2::labs(
        title = paste("Menstrual Cycle Effect on", outcome_display),
        subtitle = sig_text,
        x = "Cycle Day (Day 0 = Period Start)",
        y = paste(outcome_display, "(% difference from individual mean)")
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = ggplot2::element_text(size = 11, color = "gray40", hjust = 0.5),
        axis.title = ggplot2::element_text(size = 12),
        axis.text = ggplot2::element_text(size = 11),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.text = ggplot2::element_text(size = 10),
        legend.title = ggplot2::element_text(size = 10, face = "bold"),
        legend.key.width = ggplot2::unit(2, "lines"),
        legend.key.height = ggplot2::unit(1, "lines"),
        legend.box = "horizontal",
        legend.box.spacing = ggplot2::unit(0.8, "lines"),
        legend.box.just = "center",
        legend.spacing.x = ggplot2::unit(0.5, "lines"),
        legend.background = ggplot2::element_rect(fill = "white", colour = "grey70", linewidth = 0.5),
        legend.margin = ggplot2::margin(8, 14, 8, 14)
      )

    p
  })

  # Dynamic plot title
  output$plot_title <- renderText({
    if (is.null(rv$outcome_name)) {
      "Menstrual Cycle Effect on Outcome"
    } else {
      outcome_display <- gsub("_", " ", rv$outcome_name)
      outcome_display <- tools::toTitleCase(outcome_display)
      paste("Menstrual Cycle Effect on", outcome_display)
    }
  })

  # Confounder plot
  output$confounder_plot <- renderPlot({
    req(rv$results)
    if (!is.null(rv$results$confounder_results) && nrow(rv$results$confounder_results) > 0) {
      mc_plot_confounders(rv$results)
    } else {
      # Show placeholder message
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
          label = "No confounders uploaded.\n\nUpload a confounders file in the sidebar\nto see confounder analysis results.",
          size = 5, color = "gray50", hjust = 0.5, vjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "#f8f9fa", color = NA)
        )
    }
  })

  # Statistics outputs
  output$summary_text <- renderText({
    req(rv$results)
    rv$results$summary
  })

  output$gam_table <- renderDT({
    req(rv$results)
    gam <- rv$results$gam_result
    df <- data.frame(
      Metric = c("Effective Degrees of Freedom", "Deviance Explained", "P-value", "Significance"),
      Value = c(
        round(gam$edf, 2),
        paste0(round(gam$deviance_explained * 100, 1), "%"),
        format(gam$p_value, scientific = TRUE, digits = 3),
        ifelse(gam$p_value < 0.001, "***",
               ifelse(gam$p_value < 0.01, "**",
                      ifelse(gam$p_value < 0.05, "*", "ns")))
      )
    )
    datatable(df, options = list(dom = 't'), rownames = FALSE)
  })

  output$phase_table <- renderDT({
    req(rv$results)
    if (!is.null(rv$results$phase_models) && nrow(rv$results$phase_models) > 0) {
      df <- rv$results$phase_models
      df$slope <- round(df$slope, 4)
      df$slope_se <- round(df$slope_se, 4)
      df$p_value <- format(df$p_value, scientific = TRUE, digits = 3)
      df$r_squared <- round(df$r_squared, 3)
      datatable(df, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
    }
  })

  output$confounder_table <- renderDT({
    req(rv$results)
    if (!is.null(rv$results$confounder_results) && nrow(rv$results$confounder_results) > 0) {
      df <- rv$results$confounder_results
      df$coefficient <- round(df$coefficient, 4)
      df$std_error <- round(df$std_error, 4)
      df$ci_lower <- round(df$ci_lower, 4)
      df$ci_upper <- round(df$ci_upper, 4)
      df$p_value <- format(df$p_value, scientific = TRUE, digits = 3)
      datatable(df, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)
    } else {
      datatable(
        data.frame(Message = "No confounders uploaded. Upload a confounders file to see analysis results."),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })

  # Adjusted Model selector
  output$adjusted_model_selector <- renderUI({
    if (is.null(rv$confounders)) {
      return(
        div(
          style = "padding: 1rem; background-color: #f8f9fa; border-radius: 8px; margin-bottom: 1rem;",
          icon("info-circle", style = "color: #6c757d;"),
          span(style = "color: #6c757d; margin-left: 0.5rem;",
            "Upload a confounders file to fit an adjusted model.")
        )
      )
    }

    # Get numeric columns from confounders (for adjustment)
    conf_cols <- names(rv$confounders)
    # Exclude id and date columns
    exclude_patterns <- c("id", "date", "time")
    adj_cols <- conf_cols[!grepl(paste(exclude_patterns, collapse = "|"), conf_cols, ignore.case = TRUE)]

    if (length(adj_cols) == 0) {
      return(
        div(
          style = "padding: 1rem; background-color: #fff3cd; border-radius: 8px; margin-bottom: 1rem;",
          icon("exclamation-triangle", style = "color: #856404;"),
          span(style = "color: #856404; margin-left: 0.5rem;",
            "No suitable variables found in confounders file for adjustment.")
        )
      )
    }

    div(
      style = "margin-bottom: 1rem;",
      checkboxGroupInput(
        "adjusted_model_vars",
        "Select confounders to include in adjusted model:",
        choices = adj_cols,
        selected = NULL,
        inline = TRUE
      )
    )
  })

  # Adjusted Model plot
  output$adjusted_model_plot <- renderPlot({
    req(rv$results)

    # Handle empty or NULL selection gracefully
    selected_vars <- input$adjusted_model_vars
    if (is.null(rv$confounders) || is.null(selected_vars) || length(selected_vars) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
            label = "Select confounders above to see the adjusted cycle effect curve.",
            size = 5, color = "gray50", hjust = 0.5, vjust = 0.5) +
          ggplot2::theme_void() +
          ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
      )
    }

    tryCatch({
      processed_data <- rv$results$processed_data
      conf_data <- rv$confounders
      user_id_col <- input$id_col

        # Find the ID column in confounders
        if (user_id_col %in% names(conf_data)) {
          conf_id_col <- user_id_col
        } else if ("id" %in% names(conf_data)) {
          conf_id_col <- "id"
        } else {
          conf_id_col <- names(conf_data)[1]
        }

        # Find the ID column in processed data
        if ("id" %in% names(processed_data)) {
          proc_id_col <- "id"
        } else if (user_id_col %in% names(processed_data)) {
          proc_id_col <- user_id_col
        } else {
          proc_id_col <- names(processed_data)[1]
        }

        # Prepare merge
        conf_subset <- conf_data[, c(conf_id_col, selected_vars), drop = FALSE]
        names(conf_subset)[1] <- "merge_id"

        # Aggregate confounders by ID (take mean for numeric, first for others)
        conf_agg <- stats::aggregate(. ~ merge_id, data = conf_subset, FUN = function(x) {
          if (is.numeric(x)) mean(x, na.rm = TRUE) else x[1]
        })

        processed_data$merge_id <- processed_data[[proc_id_col]]
        merged_data <- merge(processed_data, conf_agg, by = "merge_id", all.x = TRUE)

        # Remove rows with missing confounders
        complete_rows <- complete.cases(merged_data[, selected_vars, drop = FALSE])
        merged_data <- merged_data[complete_rows, ]

        if (nrow(merged_data) < 50) {
          return(
            ggplot2::ggplot() +
              ggplot2::annotate("text", x = 0.5, y = 0.5,
                label = "Not enough data after merging confounders.\nCheck that IDs match between files.",
                size = 5, color = "gray50", hjust = 0.5, vjust = 0.5) +
              ggplot2::theme_void() +
              ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
          )
        }

        # Build formula with confounders
        conf_terms <- paste(selected_vars, collapse = " + ")
        formula_adj <- as.formula(paste0("a ~ s(cycle_day, bs = 'cc', k = 10) + ", conf_terms))

        # Fit adjusted GAM
        gam_adj <- mgcv::gam(formula_adj, data = merged_data, method = "REML")

        # Generate predictions (setting confounders to their means)
        day_range <- seq(-14, 13, length.out = 100)
        pred_data <- data.frame(cycle_day = day_range)
        for (v in selected_vars) {
          pred_data[[v]] <- mean(merged_data[[v]], na.rm = TRUE)
        }

        pred_adj <- predict(gam_adj, newdata = pred_data, se.fit = TRUE)
        adj_df <- data.frame(
          cycle_day = day_range,
          predicted = pred_adj$fit - 100,
          ci_lower = pred_adj$fit - 1.96 * pred_adj$se.fit - 100,
          ci_upper = pred_adj$fit + 1.96 * pred_adj$se.fit - 100,
          model = "Adjusted"
        )

        # Get unadjusted predictions
        unadj_df <- rv$results$gam_result$predictions
        unadj_df$predicted <- unadj_df$predicted - 100
        unadj_df$ci_lower <- unadj_df$ci_lower - 100
        unadj_df$ci_upper <- unadj_df$ci_upper - 100
        unadj_df$model <- "Unadjusted"

        # Combine for plotting
        plot_df <- rbind(
          unadj_df[, c("cycle_day", "predicted", "ci_lower", "ci_upper", "model")],
          adj_df
        )

        # Get p-values for annotation
        gam_adj_summary <- summary(gam_adj)
        p_adj <- gam_adj_summary$s.table[1, "p-value"]
        p_unadj <- rv$results$gam_result$p_value

        # Create comparison plot
        p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = cycle_day, y = predicted, color = model, fill = model)) +
          ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
          ggplot2::geom_line(linewidth = 1.2) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
          ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", alpha = 0.6) +
          ggplot2::scale_color_manual(
            values = c("Unadjusted" = "steelblue", "Adjusted" = "#27AE60"),
            name = NULL
          ) +
          ggplot2::scale_fill_manual(
            values = c("Unadjusted" = "steelblue", "Adjusted" = "#27AE60"),
            name = NULL
          ) +
          ggplot2::scale_x_continuous(breaks = seq(-14, 12, by = 2)) +
          ggplot2::labs(
            title = "Cycle Effect: Unadjusted vs Adjusted for Confounders",
            subtitle = paste0(
              "Unadjusted p = ", format(p_unadj, scientific = TRUE, digits = 2),
              "  |  Adjusted p = ", format(p_adj, scientific = TRUE, digits = 2),
              "  |  Adjusting for: ", paste(selected_vars, collapse = ", ")
            ),
            x = "Cycle Day (Day 0 = Period Start)",
            y = "% Change from Individual Mean"
          ) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
            plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5),
            legend.position = "bottom",
            panel.grid.minor = ggplot2::element_blank()
          )

        p

      }, error = function(e) {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
            label = paste("Error fitting adjusted model:\n", e$message),
            size = 4, color = "red", hjust = 0.5, vjust = 0.5) +
          ggplot2::theme_void() +
          ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
      })
  })

  # Effect Modifier selector
  output$effect_modifier_selector <- renderUI({
    if (is.null(rv$confounders)) {
      return(
        div(
          style = "padding: 1rem; background-color: #f8f9fa; border-radius: 8px; margin-bottom: 1rem;",
          icon("info-circle", style = "color: #6c757d;"),
          span(style = "color: #6c757d; margin-left: 0.5rem;",
            "Upload a confounders file to select an effect modifier variable.")
        )
      )
    }

    # Get categorical or binary columns from confounders
    conf_cols <- names(rv$confounders)
    # Exclude id and date columns
    exclude_patterns <- c("id", "date", "time")
    modifier_cols <- conf_cols[!grepl(paste(exclude_patterns, collapse = "|"), conf_cols, ignore.case = TRUE)]

    if (length(modifier_cols) == 0) {
      return(
        div(
          style = "padding: 1rem; background-color: #fff3cd; border-radius: 8px; margin-bottom: 1rem;",
          icon("exclamation-triangle", style = "color: #856404;"),
          span(style = "color: #856404; margin-left: 0.5rem;",
            "No suitable effect modifier variables found in confounders file.")
        )
      )
    }

    selectInput("effect_modifier_var", "Select Effect Modifier Variable:",
      choices = c("-- Select a variable --" = "", modifier_cols),
      selected = "")
  })

  # Effect Modifier plot
  output$effect_modifier_plot <- renderPlot({
    req(rv$results)

    if (is.null(rv$confounders) || is.null(input$effect_modifier_var) || input$effect_modifier_var == "") {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
            label = "Select an effect modifier variable above\nto see stratified cycle effects.",
            size = 5, color = "gray50", hjust = 0.5, vjust = 0.5) +
          ggplot2::theme_void() +
          ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "#f8f9fa", color = NA)
          )
      )
    }

    # Get the processed data and effect modifier
    tryCatch({
      modifier_var <- input$effect_modifier_var
      processed_data <- rv$results$processed_data

      if (is.null(processed_data)) {
        stop("Processed data not available")
      }

      # Merge effect modifier with processed data
      conf_data <- rv$confounders
      # Use the user-specified ID column
      user_id_col <- input$id_col

      # Find the ID column in confounders (try user-specified, then common names)
      if (user_id_col %in% names(conf_data)) {
        conf_id_col <- user_id_col
      } else if ("id" %in% names(conf_data)) {
        conf_id_col <- "id"
      } else {
        conf_id_col <- names(conf_data)[1]
      }

      # Find the ID column in processed data
      if ("id" %in% names(processed_data)) {
        proc_id_col <- "id"
      } else if (user_id_col %in% names(processed_data)) {
        proc_id_col <- user_id_col
      } else {
        proc_id_col <- names(processed_data)[1]
      }

      # Create modifier groups (for continuous, split into tertiles)
      modifier_values <- conf_data[[modifier_var]]
      if (is.numeric(modifier_values) && length(unique(modifier_values)) > 5) {
        # Split into tertiles for continuous variables with numeric boundaries in labels
        breaks <- quantile(modifier_values, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
        labels <- c(
          paste0("Low (", round(breaks[1], 1), "-", round(breaks[2], 1), ")"),
          paste0("Medium (", round(breaks[2], 1), "-", round(breaks[3], 1), ")"),
          paste0("High (", round(breaks[3], 1), "-", round(breaks[4], 1), ")")
        )
        conf_data$modifier_group <- cut(modifier_values,
          breaks = breaks,
          labels = labels,
          include.lowest = TRUE)
      } else {
        conf_data$modifier_group <- as.factor(modifier_values)
      }

      # Merge with processed data - need to aggregate modifier by ID first
      conf_subset <- conf_data[, c(conf_id_col, "modifier_group")]
      names(conf_subset)[1] <- "merge_id"
      conf_agg <- conf_subset[!duplicated(conf_subset$merge_id), ]

      processed_data$merge_id <- processed_data[[proc_id_col]]
      merged_data <- merge(processed_data, conf_agg, by = "merge_id", all.x = TRUE)
      merged_data <- merged_data[!is.na(merged_data$modifier_group), ]

      if (nrow(merged_data) == 0) {
        stop("No matching data after merge")
      }

      # Fit GAM for each group and get predictions
      groups <- unique(merged_data$modifier_group)
      group_colors <- c("#E74C3C", "#27AE60", "#3498DB", "#9B59B6", "#F39C12")

      day_range <- c(-14, 13)
      pred_days <- seq(day_range[1], day_range[2], by = 0.5)

      all_preds <- data.frame()
      for (i in seq_along(groups)) {
        g <- groups[i]
        group_data <- merged_data[merged_data$modifier_group == g, ]

        if (nrow(group_data) < 50) next

        # Fit simple GAM
        gam_fit <- tryCatch({
          mgcv::gam(a ~ s(cycle_day, bs = "cc", k = 5),
            data = group_data, method = "REML")
        }, error = function(e) NULL)

        if (is.null(gam_fit)) next

        # Get predictions
        new_data <- data.frame(cycle_day = pred_days)
        preds <- predict(gam_fit, newdata = new_data, se.fit = TRUE)

        group_preds <- data.frame(
          cycle_day = pred_days,
          predicted = preds$fit - 100,
          ci_lower = (preds$fit - 1.96 * preds$se.fit) - 100,
          ci_upper = (preds$fit + 1.96 * preds$se.fit) - 100,
          group = as.character(g)
        )
        all_preds <- rbind(all_preds, group_preds)
      }

      if (nrow(all_preds) == 0) {
        stop("Could not fit models for any group")
      }

      # Create plot
      p <- ggplot2::ggplot(all_preds, ggplot2::aes(x = cycle_day, y = predicted, colour = group, fill = group)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, colour = NA) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8) +
        ggplot2::scale_colour_manual(values = group_colors[1:length(groups)]) +
        ggplot2::scale_fill_manual(values = group_colors[1:length(groups)]) +
        ggplot2::scale_x_continuous(breaks = seq(day_range[1], day_range[2], by = 2)) +
        ggplot2::labs(
          title = paste("Cycle Effect Stratified by", modifier_var),
          subtitle = "Separate GAM curves fitted for each group",
          x = "Cycle Day (Day 0 = Period Start)",
          y = "% Change from Individual Mean",
          colour = modifier_var,
          fill = modifier_var
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 11, color = "gray40", hjust = 0.5),
          legend.position = "bottom"
        )

      p

    }, error = function(e) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5,
          label = paste("Could not compute effect modifier analysis:\n", e$message),
          size = 4, color = "gray50", hjust = 0.5, vjust = 0.5) +
        ggplot2::theme_void() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(fill = "#f8f9fa", color = NA)
        )
    })
  })

  # Effect Modifier table
  output$effect_modifier_table <- renderDT({
    req(rv$results)

    if (is.null(rv$confounders) || is.null(input$effect_modifier_var) || input$effect_modifier_var == "") {
      return(
        datatable(
          data.frame(Message = "Select an effect modifier variable to see stratified results."),
          options = list(dom = 't'),
          rownames = FALSE
        )
      )
    }

    tryCatch({
      modifier_var <- input$effect_modifier_var
      processed_data <- rv$results$processed_data
      conf_data <- rv$confounders
      user_id_col <- input$id_col

      # Find the ID column in confounders
      if (user_id_col %in% names(conf_data)) {
        conf_id_col <- user_id_col
      } else if ("id" %in% names(conf_data)) {
        conf_id_col <- "id"
      } else {
        conf_id_col <- names(conf_data)[1]
      }

      # Find the ID column in processed data
      if ("id" %in% names(processed_data)) {
        proc_id_col <- "id"
      } else if (user_id_col %in% names(processed_data)) {
        proc_id_col <- user_id_col
      } else {
        proc_id_col <- names(processed_data)[1]
      }

      # Create modifier groups
      modifier_values <- conf_data[[modifier_var]]
      if (is.numeric(modifier_values) && length(unique(modifier_values)) > 5) {
        # Split into tertiles with numeric boundaries in labels
        breaks <- quantile(modifier_values, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
        labels <- c(
          paste0("Low (", round(breaks[1], 1), "-", round(breaks[2], 1), ")"),
          paste0("Medium (", round(breaks[2], 1), "-", round(breaks[3], 1), ")"),
          paste0("High (", round(breaks[3], 1), "-", round(breaks[4], 1), ")")
        )
        conf_data$modifier_group <- cut(modifier_values,
          breaks = breaks,
          labels = labels,
          include.lowest = TRUE)
      } else {
        conf_data$modifier_group <- as.factor(modifier_values)
      }

      # Merge - aggregate by ID first
      conf_subset <- conf_data[, c(conf_id_col, "modifier_group")]
      names(conf_subset)[1] <- "merge_id"
      conf_agg <- conf_subset[!duplicated(conf_subset$merge_id), ]

      processed_data$merge_id <- processed_data[[proc_id_col]]
      merged_data <- merge(processed_data, conf_agg, by = "merge_id", all.x = TRUE)
      merged_data <- merged_data[!is.na(merged_data$modifier_group), ]

      # Compute stats per group
      groups <- unique(merged_data$modifier_group)
      results <- data.frame()

      for (g in groups) {
        group_data <- merged_data[merged_data$modifier_group == g, ]
        n_obs <- nrow(group_data)
        n_users <- length(unique(group_data$merge_id))

        # Fit GAM and get p-value
        gam_fit <- tryCatch({
          mgcv::gam(a ~ s(cycle_day, bs = "cc", k = 5),
            data = group_data, method = "REML")
        }, error = function(e) NULL)

        if (!is.null(gam_fit)) {
          gam_summary <- summary(gam_fit)
          p_val <- gam_summary$s.table[1, "p-value"]
          dev_exp <- gam_summary$dev.expl * 100
        } else {
          p_val <- NA
          dev_exp <- NA
        }

        results <- rbind(results, data.frame(
          Group = as.character(g),
          N_Users = n_users,
          N_Observations = n_obs,
          Deviance_Explained = round(dev_exp, 2),
          P_Value = format(p_val, scientific = TRUE, digits = 3)
        ))
      }

      datatable(results, options = list(dom = 't', scrollX = TRUE), rownames = FALSE)

    }, error = function(e) {
      datatable(
        data.frame(Message = paste("Error:", e$message)),
        options = list(dom = 't'),
        rownames = FALSE
      )
    })
  })

  # Download report as PDF
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("mcanalysis_report_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Format outcome name for display
      outcome_display <- gsub("_", " ", rv$outcome_name)
      outcome_display <- tools::toTitleCase(outcome_display)

      # Build plot from scratch (same as main_plot)
      gam_result <- rv$results$gam_result
      turning_points <- rv$results$turning_points
      phase_models <- rv$results$phase_models
      day_range <- c(-14, 13)
      phase_colors <- c("#E74C3C", "#27AE60", "#3498DB", "#9B59B6")

      pred_df <- gam_result$predictions
      plot_data <- pred_df[pred_df$cycle_day >= day_range[1] &
                            pred_df$cycle_day <= day_range[2], ]
      plot_data$predicted_pct <- plot_data$predicted - 100
      plot_data$ci_lower_pct <- plot_data$ci_lower - 100
      plot_data$ci_upper_pct <- plot_data$ci_upper - 100
      y_min <- min(plot_data$ci_lower_pct, na.rm = TRUE)
      y_max <- max(plot_data$ci_upper_pct, na.rm = TRUE)

      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = cycle_day))

      # Phase shading
      if (!is.null(phase_models) && nrow(phase_models) > 0) {
        phase_shade_colors <- c("#E8F4F8", "#FFF3CD", "#E8F8E8", "#F8E8F8")
        for (i in seq_len(nrow(phase_models))) {
          pm <- phase_models[i, ]
          fill_color <- phase_shade_colors[(i - 1) %% length(phase_shade_colors) + 1]
          if (pm$end_day <= pm$start_day) {
            p <- p +
              ggplot2::annotate("rect", xmin = pm$start_day, xmax = day_range[2],
                ymin = -Inf, ymax = Inf, alpha = 0.15, fill = fill_color) +
              ggplot2::annotate("rect", xmin = day_range[1], xmax = pm$end_day,
                ymin = -Inf, ymax = Inf, alpha = 0.15, fill = fill_color)
          } else {
            p <- p +
              ggplot2::annotate("rect", xmin = pm$start_day, xmax = pm$end_day,
                ymin = -Inf, ymax = Inf, alpha = 0.15, fill = fill_color)
          }
        }
      }

      # CI and GAM curve
      p <- p +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = ci_lower_pct, ymax = ci_upper_pct),
          alpha = 0.3, fill = "steelblue") +
        ggplot2::geom_line(ggplot2::aes(y = predicted_pct), color = "steelblue", linewidth = 1.2) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dotted", color = "red", linewidth = 0.8)

      # Phase slope lines
      if (!is.null(phase_models) && nrow(phase_models) > 0 && !is.null(turning_points)) {
        for (i in seq_len(nrow(phase_models))) {
          pm <- phase_models[i, ]
          line_color <- phase_colors[(i - 1) %% length(phase_colors) + 1]
          start_idx <- which.min(abs(plot_data$cycle_day - pm$start_day))
          end_idx <- which.min(abs(plot_data$cycle_day - pm$end_day))
          start_y <- plot_data$predicted_pct[start_idx]
          end_y <- plot_data$predicted_pct[end_idx]
          if (pm$end_day <= pm$start_day) {
            total_span <- (day_range[2] - pm$start_day) + (pm$end_day - day_range[1])
            frac1 <- (day_range[2] - pm$start_day) / total_span
            y_at_edge <- start_y + frac1 * (end_y - start_y)
            p <- p +
              ggplot2::annotate("segment", x = pm$start_day, xend = day_range[2],
                y = start_y, yend = y_at_edge, color = line_color, linetype = "dashed", linewidth = 0.9, alpha = 0.9) +
              ggplot2::annotate("segment", x = day_range[1], xend = pm$end_day,
                y = y_at_edge, yend = end_y, color = line_color, linetype = "dashed", linewidth = 0.9, alpha = 0.9)
          } else {
            p <- p +
              ggplot2::annotate("segment", x = pm$start_day, xend = pm$end_day,
                y = start_y, yend = end_y, color = line_color, linetype = "dashed", linewidth = 0.9, alpha = 0.9)
          }
        }
      }

      # Turning points
      if (!is.null(turning_points)) {
        for (i in seq_along(turning_points)) {
          tp <- turning_points[i]
          if (tp >= day_range[1] && tp <= day_range[2]) {
            closest_idx <- which.min(abs(plot_data$cycle_day - tp))
            y_val <- plot_data$predicted_pct[closest_idx]
            p <- p +
              ggplot2::geom_vline(xintercept = tp, linetype = "dashed", color = "orange", alpha = 0.6) +
              ggplot2::geom_point(data = data.frame(x = tp, y = y_val),
                ggplot2::aes(x = x, y = y), color = "orange", size = 4) +
              ggplot2::annotate("label", x = tp, y = y_val, label = paste0("Day ", round(tp, 0)),
                size = 3.5, vjust = -0.8, fill = "white", color = "gray30")
          }
        }
      }

      # Watermark bottom right
      p <- p + ggplot2::annotate("text", x = day_range[2] - 0.5, y = y_min,
        label = "MCAnalysis", hjust = 1, vjust = 1.5, size = 3.5,
        fontface = "italic", color = "gray60", alpha = 0.7)

      # Subtitle
      p_val <- gam_result$p_value
      sig_stars <- if (p_val < 0.001) "***" else if (p_val < 0.01) "**" else if (p_val < 0.05) "*" else "(ns)"
      sig_text <- paste0("Cycle Effect p = ", format(p_val, scientific = TRUE, digits = 2), " ", sig_stars,
                         "  |  Deviance Explained: ", round(gam_result$deviance_explained * 100, 1), "%")

      # Build unified legend - all items in one box using linetype aesthetic
      dummy_pt <- data.frame(x = NA_real_, y = NA_real_)

      p <- p +
        ggplot2::geom_line(data = dummy_pt,
          ggplot2::aes(x = x, y = y, linetype = "GAM Fit"),
          colour = "steelblue", linewidth = 1.5, show.legend = TRUE, na.rm = TRUE) +
        ggplot2::geom_point(data = dummy_pt,
          ggplot2::aes(x = x, y = y, linetype = "95% CI"),
          colour = "steelblue", shape = 15, size = 6, alpha = 0.4,
          show.legend = TRUE, na.rm = TRUE) +
        ggplot2::geom_point(data = dummy_pt,
          ggplot2::aes(x = x, y = y, linetype = "Turning Points"),
          colour = "orange", shape = 16, size = 4,
          show.legend = TRUE, na.rm = TRUE)

      # Single linetype scale for unified legend box
      p <- p +
        ggplot2::scale_linetype_manual(
          name = NULL,
          values = c("GAM Fit" = "solid", "95% CI" = "blank", "Turning Points" = "blank"),
          guide = ggplot2::guide_legend(
            order = 1,
            nrow = 1,
            override.aes = list(
              colour = c("steelblue", "steelblue", "orange"),
              linetype = c("solid", "blank", "blank"),
              linewidth = c(1.5, 0, 0),
              shape = c(NA, 15, 16),
              size = c(0, 6, 4),
              alpha = c(1, 0.4, 1)
            )
          )
        )

      # Phase slopes as second legend using shape
      if (!is.null(phase_models) && nrow(phase_models) > 0) {
        for (i in seq_len(nrow(phase_models))) {
          pm <- phase_models[i, ]
          sig <- if (pm$p_value < 0.001) "***" else if (pm$p_value < 0.01) "**" else if (pm$p_value < 0.05) "*" else ""
          direction <- if (pm$slope > 0) "\u2191" else "\u2193"
          slope_label <- sprintf("Phase %d: %s %.2f%% / day %s", i, direction, abs(pm$slope), sig)
          line_color <- phase_colors[(i - 1) %% length(phase_colors) + 1]

          p <- p +
            ggplot2::geom_point(data = dummy_pt,
              ggplot2::aes_string(x = "x", y = "y", shape = shQuote(slope_label)),
              colour = line_color, size = 3, na.rm = TRUE)
        }

        n_phases <- nrow(phase_models)
        phase_shapes <- setNames(rep(16, n_phases),
          sapply(seq_len(n_phases), function(i) {
            pm <- phase_models[i, ]
            sig <- if (pm$p_value < 0.001) "***" else if (pm$p_value < 0.01) "**" else if (pm$p_value < 0.05) "*" else ""
            direction <- if (pm$slope > 0) "\u2191" else "\u2193"
            sprintf("Phase %d: %s %.2f%% / day %s", i, direction, abs(pm$slope), sig)
          }))

        phase_label_colors <- setNames(phase_colors[seq_len(n_phases)], names(phase_shapes))

        p <- p +
          ggplot2::scale_shape_manual(name = "Phase Slopes", values = phase_shapes,
            guide = ggplot2::guide_legend(order = 4, nrow = 1,
              override.aes = list(colour = phase_label_colors, size = 3)))
      }

      p <- p +
        ggplot2::scale_x_continuous(breaks = seq(day_range[1], day_range[2], by = 2)) +
        ggplot2::labs(
          title = paste("Menstrual Cycle Effect on", outcome_display),
          subtitle = sig_text,
          x = "Cycle Day (Day 0 = Period Start)",
          y = paste(outcome_display, "(% difference from individual mean)")
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          text = ggplot2::element_text(size = 12),
          plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
          plot.subtitle = ggplot2::element_text(size = 11, color = "gray40", hjust = 0.5),
          axis.title = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 11),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = "center",
          legend.text = ggplot2::element_text(size = 10),
          legend.title = ggplot2::element_text(size = 10, face = "bold"),
          legend.key.width = ggplot2::unit(2, "lines"),
          legend.key.height = ggplot2::unit(1, "lines"),
          legend.box = "horizontal",
          legend.box.spacing = ggplot2::unit(0.8, "lines"),
          legend.box.just = "center",
          legend.spacing.x = ggplot2::unit(0.5, "lines"),
          legend.background = ggplot2::element_rect(fill = "white", colour = "grey70", linewidth = 0.5),
          legend.margin = ggplot2::margin(8, 14, 8, 14)
        )

      # Create PDF with plot and summary
      pdf(file, width = 11, height = 14)

      # Page 1: Plot
      print(p)

      # Page 2: Summary text
      plot.new()
      par(mar = c(1, 1, 1, 1))

      # Split summary into lines and display
      summary_lines <- strsplit(rv$results$summary, "\n")[[1]]

      # Create text plot
      plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
           axes = FALSE, xlab = "", ylab = "")

      # Calculate line positions
      n_lines <- length(summary_lines)
      y_positions <- seq(0.95, 0.05, length.out = min(n_lines, 40))

      for (i in seq_along(summary_lines)) {
        if (i <= 40) {
          text(0.02, y_positions[i], summary_lines[i],
               adj = c(0, 0.5), family = "mono", cex = 0.7)
        }
      }

      dev.off()
    }
  )
}

# Run app --------------------------------------------------------------------
shinyApp(ui, server)
