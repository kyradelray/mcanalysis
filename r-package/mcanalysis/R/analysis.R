#' Menstrual Cycle Analysis
#'
#' Run complete menstrual cycle analysis pipeline.
#'
#' @param period_dates Data frame with user ID and period start dates
#' @param outcome_data Data frame with user ID, date, and outcome values
#' @param confounder_data Optional data frame with confounders
#' @param id_col Column name for user ID (default "id")
#' @param date_col Column name for period dates (default "period_date")
#' @param outcome_col Column name for outcome variable (default "outcome")
#' @param outcome_date_col Column name for outcome dates (default "date")
#' @param min_cycle_length Minimum cycle length to include (default 21)
#' @param max_cycle_length Maximum cycle length to include (default 35)
#' @param min_negative_obs Minimum observations with negative cycle days (default 5)
#' @param min_positive_obs Minimum observations with positive cycle days (default 5)
#' @param k Number of basis functions for GAM (default 10)
#' @param confounder_cols Specific confounder columns to analyze (default NULL uses all)
#'
#' @return List with analysis results
#' @export
#'
#' @examples
#' \dontrun{
#' # Load your data
#' periods <- read.csv("period_dates.csv")
#' outcomes <- read.csv("outcomes.csv")
#'
#' # Run analysis
#' results <- mc_analysis(periods, outcomes)
#'
#' # View results
#' print(results$summary)
#' plot(results)
#' }
mc_analysis <- function(period_dates,
                         outcome_data,
                         confounder_data = NULL,
                         id_col = "id",
                         date_col = "period_date",
                         outcome_col = "outcome",
                         outcome_date_col = "date",
                         min_cycle_length = 21,
                         max_cycle_length = 35,
                         min_negative_obs = 5,
                         min_positive_obs = 5,
                         k = 10,
                         confounder_cols = NULL) {

  # Step 1: Process periods
  message("Processing period dates...")
  df <- process_periods(
    period_dates,
    outcome_data,
    id_col = id_col,
    date_col = date_col,
    outcome_col = outcome_col,
    outcome_date_col = outcome_date_col
  )

  if (nrow(df) == 0) {
    stop("No data after processing. Check your input data.")
  }

  # Step 2: Filter cycles by length
  message("Filtering cycles by length...")
  df <- filter_cycles(df, min_cycle_length, max_cycle_length)

  # Step 3: Filter users by observation count
  message("Filtering users by observation count...")
  df <- filter_users(df, min_negative_obs, min_positive_obs)

  if (nrow(df) == 0) {
    stop("No users meet the observation criteria.")
  }

  # Step 4: Add dummy day 21
  message("Adding dummy day for cyclical analysis...")
  df <- add_dummy_day(df)

  # Step 5: Normalize outcome
  message("Normalizing outcome to percentage of individual mean...")
  df <- normalize_outcome(df, outcome_col = "outcome")

  # Step 6: Fit GAM
  message("Fitting GAM model...")
  gam_result <- fit_gam(df, outcome_col = "a", k = k, cyclic = TRUE)

  # Step 7: Find inflection points
  message("Finding inflection points...")
  inflection_points <- find_inflections(gam_result)

  # Step 8: Fit phase models from GAM predictions at turning points
  message("Fitting phase models...")
  phase_models <- fit_phase_models(df, inflection_points, gam_result = gam_result)

  # Step 9: Analyze confounders if provided
  confounder_results <- NULL
  if (!is.null(confounder_data)) {
    message("Analyzing confounders...")
    confounder_results <- analyze_confounders(
      df,
      confounder_data,
      id_col = "user_id",
      confounder_cols = confounder_cols
    )
  }

  # Compile results
  n_users <- length(unique(df$user_id))
  n_obs <- sum(!df$is_dummy)
  n_cycles <- sum(tapply(df$cycle_id, df$user_id, function(x) length(unique(x))))

  # Create summary text
  summary_text <- sprintf("
================================================================================
                     MENSTRUAL CYCLE ANALYSIS SUMMARY
================================================================================

DATA SUMMARY
------------
  Users analyzed: %d
  Total observations: %d
  Total cycles: %d

GAM MODEL RESULTS
-----------------
  Effective degrees of freedom: %.2f
  Deviance explained: %.1f%%
  P-value for cycle effect: %.2e %s

INFLECTION POINTS
-----------------
  Number found: %d
  Days: %s

PHASE MODELS (Daily Change)
---------------------------
",
    n_users, n_obs, n_cycles,
    gam_result$edf,
    gam_result$deviance_explained * 100,
    gam_result$p_value,
    if (gam_result$p_value < 0.001) "***" else if (gam_result$p_value < 0.01) "**" else if (gam_result$p_value < 0.05) "*" else "(not significant)",
    length(inflection_points),
    if (length(inflection_points) > 0) paste(round(inflection_points, 1), collapse = ", ") else "None"
  )

  # Add phase model details
  if (nrow(phase_models) > 0) {
    for (i in seq_len(nrow(phase_models))) {
      pm <- phase_models[i, ]
      sig <- if (pm$p_value < 0.001) "***" else if (pm$p_value < 0.01) "**" else if (pm$p_value < 0.05) "*" else ""
      summary_text <- paste0(summary_text, sprintf(
        "  Phase %d (Day %.0f to %.0f):\n    Slope: %.4f ± %.4f (p=%.3e) %s\n    R²: %.3f\n",
        pm$phase, pm$start_day, pm$end_day,
        pm$slope, pm$slope_se, pm$p_value, sig,
        pm$r_squared
      ))
    }
  }

  # Add confounder results
  if (!is.null(confounder_results) && nrow(confounder_results) > 0) {
    summary_text <- paste0(summary_text, "\nCONFOUNDER ANALYSIS\n-------------------\n")
    for (i in seq_len(nrow(confounder_results))) {
      cr <- confounder_results[i, ]
      sig <- if (cr$p_value < 0.001) "***" else if (cr$p_value < 0.01) "**" else if (cr$p_value < 0.05) "*" else ""
      summary_text <- paste0(summary_text, sprintf(
        "  %s:\n    Coefficient: %.4f [%.4f, %.4f]\n    P-value: %.3e %s\n",
        cr$confounder, cr$coefficient, cr$ci_lower, cr$ci_upper, cr$p_value, sig
      ))
    }
  }

  summary_text <- paste0(summary_text, "\n================================================================================\n")

  result <- list(
    processed_data = df,
    gam_result = gam_result,
    inflection_points = inflection_points,
    phase_models = phase_models,
    confounder_results = confounder_results,
    n_users = n_users,
    n_observations = n_obs,
    n_cycles = n_cycles,
    summary = summary_text
  )

  class(result) <- c("mcanalysis", "list")

  message("\nAnalysis complete!")
  message(sprintf("  Users: %d", n_users))
  message(sprintf("  Observations: %d", n_obs))
  message(sprintf("  Cycles: %d", n_cycles))
  message(sprintf("  Inflection points found: %d", length(inflection_points)))

  return(result)
}


#' Print MC Analysis Results
#'
#' @param x mcanalysis object
#' @param ... Additional arguments (ignored)
#'
#' @export
print.mcanalysis <- function(x, ...) {
  cat(x$summary)
}


#' Plot MC Analysis Results
#'
#' @param x mcanalysis object
#' @param ... Additional arguments passed to plot_cycle_effect()
#'
#' @export
plot.mcanalysis <- function(x, ...) {
  plot_cycle_effect(
    x$gam_result,
    inflection_points = x$inflection_points,
    phase_models = x$phase_models,
    raw_data = x$processed_data,
    ...
  )
}


#' @rdname mc_analysis
#' @export
mc_analyze <- mc_analysis


#' Plot Menstrual Cycle Analysis Results
#'
#' @param results mcanalysis object from mc_analyze()
#' @param ... Additional arguments passed to plot_cycle_effect()
#'
#' @return ggplot object
#' @export
mc_plot <- function(results, ...) {
  plot_cycle_effect(
    results$gam_result,
    inflection_points = results$inflection_points,
    phase_models = results$phase_models,
    raw_data = results$processed_data,
    ...
  )
}


#' Print Summary of Menstrual Cycle Analysis
#'
#' @param results mcanalysis object from mc_analyze()
#'
#' @export
mc_summary <- function(results) {
  cat(results$summary)
}


#' Plot Confounder Effects
#'
#' @param results mcanalysis object from mc_analyze()
#' @param title Plot title
#' @param ... Additional arguments
#'
#' @return ggplot object
#' @export
mc_plot_confounders <- function(results, title = "Confounder Effects", ...) {
  plot_confounder_effects(results$confounder_results, title = title, ...)
}
