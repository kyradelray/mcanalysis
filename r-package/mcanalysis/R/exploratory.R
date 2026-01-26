#' Exploratory Data Analysis Functions
#'
#' Functions for exploring menstrual cycle data before analysis.


#' Plot Outcome Distribution
#'
#' Plot histogram and boxplot of the outcome variable.
#'
#' @param outcome_data DataFrame with outcome values
#' @param outcome_col Name of outcome column
#' @param title Plot title
#' @param color Histogram fill color
#' @param bins Number of histogram bins
#'
#' @return ggplot2 object
#' @export
#'
#' @import ggplot2
#' @importFrom stats median sd
plot_outcome_distribution <- function(outcome_data,
                                       outcome_col = "outcome",
                                       title = "Outcome Distribution",
                                       color = "steelblue",
                                       bins = 50) {

  values <- outcome_data[[outcome_col]]
  values <- values[!is.na(values)]

  mean_val <- mean(values)
  median_val <- median(values)
  sd_val <- sd(values)

  # Create data frame for plotting
  df <- data.frame(value = values)

  # Histogram
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = bins, fill = color, color = "white", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = mean_val, color = "red", linetype = "dashed",
                        linewidth = 1) +
    ggplot2::geom_vline(xintercept = median_val, color = "orange", linetype = "dashed",
                        linewidth = 1) +
    ggplot2::labs(x = outcome_col, y = "Frequency",
                  subtitle = sprintf("N=%s | Mean=%.2f | Median=%.2f | SD=%.2f",
                                     format(length(values), big.mark=","),
                                     mean_val, median_val, sd_val)) +
    ggplot2::theme_minimal()

  # Boxplot
  p2 <- ggplot2::ggplot(df, ggplot2::aes(y = value, x = "")) +
    ggplot2::geom_boxplot(fill = color, alpha = 0.7) +
    ggplot2::labs(y = outcome_col, x = "") +
    ggplot2::theme_minimal()

  # Combine plots
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p <- p1 + p2 + patchwork::plot_layout(widths = c(2, 1)) +
      patchwork::plot_annotation(title = title,
                                  theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")))
  } else {
    p <- p1 + ggplot2::ggtitle(title)
  }

  return(p)
}


#' Plot Cycle Length Distribution
#'
#' Plot distribution of menstrual cycle lengths.
#'
#' @param period_dates DataFrame with period start dates
#' @param id_col Name of user ID column
#' @param date_col Name of date column
#' @param title Plot title
#' @param color Histogram fill color for valid cycles
#' @param valid_range Vector of c(min, max) valid cycle length
#'
#' @return List with ggplot2 object and summary data frame
#' @export
#'
#' @import ggplot2
#' @importFrom stats median sd
plot_cycle_lengths <- function(period_dates,
                                id_col = "user_id",
                                date_col = "period_date",
                                title = "Cycle Length Distribution",
                                color = "coral",
                                valid_range = c(21, 35)) {

  df <- period_dates
  df[[date_col]] <- as.Date(df[[date_col]])
  df <- df[order(df[[id_col]], df[[date_col]]), ]

  # Calculate cycle lengths
  df$cycle_length <- ave(as.numeric(df[[date_col]]), df[[id_col]],
                          FUN = function(x) c(NA, diff(x)))
  cycle_lengths <- df$cycle_length[!is.na(df$cycle_length)]

  # Mark valid/invalid
  is_valid <- cycle_lengths >= valid_range[1] & cycle_lengths <= valid_range[2]
  n_valid <- sum(is_valid)
  n_invalid <- sum(!is_valid)
  pct_valid <- 100 * n_valid / length(cycle_lengths)

  plot_df <- data.frame(
    cycle_length = cycle_lengths,
    valid = ifelse(is_valid, "Valid", "Outside range")
  )

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = cycle_length, fill = valid)) +
    ggplot2::geom_histogram(binwidth = 1, color = "white", alpha = 0.7) +
    ggplot2::scale_fill_manual(values = c("Valid" = color, "Outside range" = "lightgray")) +
    ggplot2::geom_vline(xintercept = mean(cycle_lengths), color = "red",
                        linetype = "dashed", linewidth = 1) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("N=%s cycles | Mean=%.1f days | Valid (%d-%d days): %.1f%%",
                         format(length(cycle_lengths), big.mark=","),
                         mean(cycle_lengths), valid_range[1], valid_range[2], pct_valid),
      x = "Cycle Length (days)",
      y = "Frequency",
      fill = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    ggplot2::xlim(10, 60)

  summary_df <- data.frame(
    metric = c("Total cycles", "Valid cycles", "Invalid cycles",
               "Percent valid", "Mean length", "Median length",
               "SD", "Min", "Max"),
    value = c(length(cycle_lengths), n_valid, n_invalid,
              pct_valid, mean(cycle_lengths), median(cycle_lengths),
              sd(cycle_lengths), min(cycle_lengths), max(cycle_lengths))
  )

  return(list(plot = p, summary = summary_df))
}


#' Plot Confounder Distributions
#'
#' Plot distributions of confounder variables.
#'
#' @param confounder_data DataFrame with confounder variables
#' @param id_col Name of user ID column (excluded from plots)
#' @param confounder_cols Vector of confounder column names. If NULL, uses all numeric columns.
#' @param title Plot title
#' @param color Histogram fill color
#'
#' @return ggplot2 object
#' @export
#'
#' @import ggplot2
plot_confounder_distributions <- function(confounder_data,
                                           id_col = "user_id",
                                           confounder_cols = NULL,
                                           title = "Confounder Distributions",
                                           color = "mediumpurple") {

  if (is.null(confounder_cols)) {
    confounder_cols <- names(confounder_data)[names(confounder_data) != id_col]
    confounder_cols <- confounder_cols[sapply(confounder_data[confounder_cols], is.numeric)]
  }

  if (length(confounder_cols) == 0) {
    message("No numeric confounder columns found")
    return(NULL)
  }

  # Reshape to long format
  plot_data <- tidyr::pivot_longer(
    confounder_data[, c(id_col, confounder_cols)],
    cols = all_of(confounder_cols),
    names_to = "confounder",
    values_to = "value"
  )

  # Calculate stats for labels
  stats <- plot_data %>%
    dplyr::group_by(confounder) %>%
    dplyr::summarise(
      mean_val = mean(value, na.rm = TRUE),
      sd_val = sd(value, na.rm = TRUE),
      n = sum(!is.na(value)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(label = sprintf("N=%s\nμ=%.1f\nσ=%.1f", format(n, big.mark=","), mean_val, sd_val))

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 30, fill = color, color = "white", alpha = 0.7) +
    ggplot2::geom_vline(data = stats, ggplot2::aes(xintercept = mean_val),
                        color = "red", linetype = "dashed") +
    ggplot2::facet_wrap(~confounder, scales = "free") +
    ggplot2::labs(title = title, x = "Value", y = "Frequency") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  return(p)
}


#' Plot Observations per User
#'
#' Plot distribution of number of observations per user.
#'
#' @param outcome_data DataFrame with outcome data
#' @param id_col Name of user ID column
#' @param title Plot title
#' @param color Histogram fill color
#'
#' @return List with ggplot2 object and summary data frame
#' @export
#'
#' @import ggplot2
#' @importFrom stats median sd
plot_observations_per_user <- function(outcome_data,
                                        id_col = "user_id",
                                        title = "Observations per User",
                                        color = "teal") {

  obs_per_user <- table(outcome_data[[id_col]])
  obs_df <- data.frame(n_obs = as.numeric(obs_per_user))

  mean_obs <- mean(obs_df$n_obs)
  median_obs <- median(obs_df$n_obs)

  p <- ggplot2::ggplot(obs_df, ggplot2::aes(x = n_obs)) +
    ggplot2::geom_histogram(bins = 50, fill = color, color = "white", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = mean_obs, color = "red", linetype = "dashed",
                        linewidth = 1) +
    ggplot2::geom_vline(xintercept = median_obs, color = "orange", linetype = "dashed",
                        linewidth = 1) +
    ggplot2::labs(
      title = title,
      subtitle = sprintf("N=%s users | Mean=%.1f obs | Median=%.1f obs",
                         format(length(obs_per_user), big.mark=","), mean_obs, median_obs),
      x = "Number of observations",
      y = "Number of users"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

  summary_df <- data.frame(
    metric = c("Total users", "Total observations", "Mean obs/user",
               "Median obs/user", "Min obs/user", "Max obs/user"),
    value = c(length(obs_per_user), sum(obs_per_user), mean_obs,
              median_obs, min(obs_per_user), max(obs_per_user))
  )

  return(list(plot = p, summary = summary_df))
}


#' Data Summary
#'
#' Generate a summary table of the input data.
#'
#' @param period_dates Period dates data
#' @param outcome_data Outcome data
#' @param confounder_data Confounder data (optional)
#' @param id_col User ID column name
#' @param date_col Period date column name
#' @param outcome_col Outcome column name
#' @param outcome_date_col Outcome date column name
#'
#' @return Data frame with summary statistics
#' @export
#'
#' @importFrom stats median sd
data_summary <- function(period_dates,
                          outcome_data,
                          confounder_data = NULL,
                          id_col = "user_id",
                          date_col = "period_date",
                          outcome_col = "outcome",
                          outcome_date_col = "date") {

  # Period data stats
  period_users <- length(unique(period_dates[[id_col]]))
  total_periods <- nrow(period_dates)

  # Calculate cycle lengths
  df <- period_dates
  df[[date_col]] <- as.Date(df[[date_col]])
  df <- df[order(df[[id_col]], df[[date_col]]), ]
  df$cycle_length <- ave(as.numeric(df[[date_col]]), df[[id_col]],
                          FUN = function(x) c(NA, diff(x)))
  cycle_lengths <- df$cycle_length[!is.na(df$cycle_length)]

  # Outcome data stats
  outcome_users <- length(unique(outcome_data[[id_col]]))
  total_outcomes <- nrow(outcome_data)
  outcome_values <- outcome_data[[outcome_col]]
  outcome_values <- outcome_values[!is.na(outcome_values)]

  # Users in common
  common_users <- intersect(unique(period_dates[[id_col]]),
                            unique(outcome_data[[id_col]]))

  rows <- list(
    c("PERIOD DATA", "", ""),
    c("  Users with period data", period_users, ""),
    c("  Total period records", total_periods, ""),
    c("  Periods per user", sprintf("%.1f", total_periods/period_users), "mean"),
    c("  Cycle length (days)", sprintf("%.1f ± %.1f", mean(cycle_lengths), sd(cycle_lengths)), "mean ± SD"),
    c("  Cycle length range", sprintf("%.0f - %.0f", min(cycle_lengths), max(cycle_lengths)), "min - max"),
    c("", "", ""),
    c("OUTCOME DATA", "", ""),
    c("  Users with outcome data", outcome_users, ""),
    c("  Total outcome records", total_outcomes, ""),
    c("  Observations per user", sprintf("%.1f", total_outcomes/outcome_users), "mean"),
    c("  Outcome value", sprintf("%.2f ± %.2f", mean(outcome_values), sd(outcome_values)), "mean ± SD"),
    c("  Outcome range", sprintf("%.2f - %.2f", min(outcome_values), max(outcome_values)), "min - max"),
    c("", "", ""),
    c("OVERLAP", "", ""),
    c("  Users in both datasets", length(common_users), "")
  )

  if (!is.null(confounder_data)) {
    conf_users <- length(unique(confounder_data[[id_col]]))
    conf_cols <- names(confounder_data)[names(confounder_data) != id_col]
    conf_cols <- conf_cols[sapply(confounder_data[conf_cols], is.numeric)]

    rows <- c(rows, list(
      c("", "", ""),
      c("CONFOUNDER DATA", "", ""),
      c("  Users with confounders", conf_users, ""),
      c("  Confounder variables", length(conf_cols), "")
    ))

    for (col in conf_cols) {
      values <- confounder_data[[col]]
      values <- values[!is.na(values)]
      if (length(values) > 0) {
        rows <- c(rows, list(
          c(sprintf("    %s", col), sprintf("%.2f ± %.2f", mean(values), sd(values)), "mean ± SD")
        ))
      }
    }
  }

  summary_df <- do.call(rbind, rows)
  summary_df <- as.data.frame(summary_df)
  names(summary_df) <- c("Metric", "Value", "Note")

  return(summary_df)
}


#' Run Exploratory Analysis
#'
#' Run complete exploratory data analysis and generate all plots.
#'
#' @param period_dates Period dates data
#' @param outcome_data Outcome data
#' @param confounder_data Confounder data (optional)
#' @param id_col User ID column name
#' @param date_col Period date column name
#' @param outcome_col Outcome column name
#' @param outcome_date_col Outcome date column name
#' @param confounder_cols Specific confounder columns to analyze
#' @param save_dir Directory to save figures (optional)
#'
#' @return List containing all plots and summary tables
#' @export
run_exploratory_analysis <- function(period_dates,
                                      outcome_data,
                                      confounder_data = NULL,
                                      id_col = "user_id",
                                      date_col = "period_date",
                                      outcome_col = "outcome",
                                      outcome_date_col = "date",
                                      confounder_cols = NULL,
                                      save_dir = NULL) {

  results <- list()

  cat(strrep("=", 60), "\n")
  cat("EXPLORATORY DATA ANALYSIS\n")
  cat(strrep("=", 60), "\n\n")

  # Data summary
  cat("DATA SUMMARY:\n")
  cat(strrep("-", 40), "\n")
  summary_df <- data_summary(period_dates, outcome_data, confounder_data,
                              id_col, date_col, outcome_col, outcome_date_col)
  for (i in seq_len(nrow(summary_df))) {
    if (summary_df$Metric[i] != "") {
      cat(sprintf("%s: %s\n", summary_df$Metric[i], summary_df$Value[i]))
    }
  }
  results$summary <- summary_df

  cat("\n", strrep("-", 40), "\n", sep = "")

  # Outcome distribution
  cat("Generating outcome distribution plot...\n")
  p_outcome <- plot_outcome_distribution(outcome_data, outcome_col = outcome_col,
                                          title = paste("Distribution of", outcome_col))
  results$plot_outcome <- p_outcome
  if (!is.null(save_dir)) {
    ggplot2::ggsave(file.path(save_dir, "eda_outcome_distribution.png"), p_outcome,
                    width = 10, height = 4, dpi = 150)
  }

  # Cycle lengths
  cat("Generating cycle length distribution plot...\n")
  cycle_result <- plot_cycle_lengths(period_dates, id_col = id_col, date_col = date_col)
  results$plot_cycles <- cycle_result$plot
  results$cycle_summary <- cycle_result$summary
  if (!is.null(save_dir)) {
    ggplot2::ggsave(file.path(save_dir, "eda_cycle_lengths.png"), cycle_result$plot,
                    width = 10, height = 4, dpi = 150)
  }

  # Observations per user
  cat("Generating observations per user plot...\n")
  obs_result <- plot_observations_per_user(outcome_data, id_col = id_col)
  results$plot_obs_per_user <- obs_result$plot
  results$obs_summary <- obs_result$summary
  if (!is.null(save_dir)) {
    ggplot2::ggsave(file.path(save_dir, "eda_obs_per_user.png"), obs_result$plot,
                    width = 10, height = 4, dpi = 150)
  }

  # Confounder distributions
  if (!is.null(confounder_data)) {
    cat("Generating confounder distribution plots...\n")
    p_conf <- plot_confounder_distributions(confounder_data, id_col = id_col,
                                             confounder_cols = confounder_cols)
    results$plot_confounders <- p_conf
    if (!is.null(save_dir) && !is.null(p_conf)) {
      ggplot2::ggsave(file.path(save_dir, "eda_confounders.png"), p_conf,
                      width = 10, height = 6, dpi = 150)
    }
  }

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Exploratory analysis complete!\n")
  if (!is.null(save_dir)) {
    cat("Figures saved to:", save_dir, "\n")
  }
  cat(strrep("=", 60), "\n")

  return(results)
}
